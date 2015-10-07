#lang typed/racket/base
;
; Apollo Art PEx System Control
;

(require racket/list
         racket/match
         racket/format
         typed/racket/class)

(require mordae/match
         mordae/syntax
         libserialport)

(require/typed pex/regexp-read
  (regexp-match-bytes-evt (-> Bytes Input-Port (Evtof (U EOF False Bytes)))))

(provide Bank%
         Relay%
         Fader%
         bank%
         relay%
         fader%)


(define-logger pex)


(define-type Bank%
  (Class
    (init-field (path Path-String)
                (id Natural))

    (command (-> String String (U False String)))

    (list-relays (-> (Listof Positive-Integer)))
    (list-faders (-> (Listof Positive-Integer)))

    (get-relay (-> Positive-Integer (Instance Relay%)))
    (get-fader (-> Positive-Integer (Instance Fader%)))))


(define-type Relay%
  (Class
    (init-field (bank (Instance Bank%))
                (id Positive-Integer))

    (get-status (-> String))

    (get-on? (-> Boolean))
    (set-on! (-> Boolean Void))))


(define-type Fader%
  (Class
    (init-field (bank (Instance Bank%))
                (id Positive-Integer))

    (get-status (-> String))

    (get-level (-> Natural))
    (set-level! (-> Natural Void))
    (fade-to-level! (-> Natural Void))))


(: pex-connect (-> Path-String (values Input-Port Output-Port)))
(define (pex-connect path)
  (open-serial-port path #:baudrate 19200 #:parity 'even))


(: bank% Bank%)
(define bank%
  (class object%
    (init-field path id)

    (begin
      (when (> id 9)
        (raise-arguments-error 'bank% "invalid bank" "id" id)))


    (: in Input-Port)
    (: out Output-Port)
    (define-values (in out)
      (pex-connect path))


    (define lock
      (make-semaphore 1))

    (define relays : (HashTable Positive-Integer (Instance Relay%))
      (make-hash))

    (define faders : (HashTable Positive-Integer (Instance Fader%))
      (make-hash))


    (begin
      (for ((relay-id : Positive-Integer (in-range 1 97)))
        (let ((relay (new relay% (bank this) (id relay-id))))
          (with-handlers ((exn:fail? void))
            (when (send relay get-status)
              (hash-set! relays relay-id relay)))))

      (for ((fader-id : Positive-Integer (in-range 1 33)))
        (let ((fader (new fader% (bank this) (id fader-id))))
          (with-handlers ((exn:fail? void))
            (when (send fader get-status)
              (hash-set! faders fader-id fader))))))


    (: drain-leftover-data (-> Void))
    (define/private (drain-leftover-data)
      (let* ((bstr (make-bytes 256))
             (len (read-bytes-avail!* bstr in)))
        (unless (or (eof-object? len)
                    (procedure? len))
          (when (> len 0)
            (log-pex-warning "<- discarded ~s" (subbytes bstr 0 len))
            (drain-leftover-data)))))


    (define/public (command head body)
      (with-semaphore lock
        (let retry : (U False String) ()
          (drain-leftover-data)

          (let ((str (string-append "\x01" head "\x02" body "\x17\x03")))
            (log-pex-info "-> ~a ~a" head body)
            (write-string str out)
            (flush-output out))

          (let/ec return : (U False String)
            (define reply-evt : (Evtof (U EOF False Bytes))
              (regexp-match-bytes-evt #"\x01[\x01-\x7f]*?\x03" in))

            (define reply : (U EOF False Bytes)
              (sync/timeout 0.05 reply-evt))

            (when (eof-object? reply)
              (error 'command "unexpected end-of-file"))

            (unless reply
              (when (sync/timeout 0 in)
                (log-pex-error "<- corrupted reply, retrying")
                (return (retry)))

              (log-pex-info "<- no reply")
              (return #f))

            (match reply
              ((regexp-parts #"\x01(.*?)\x02(.*?)\x17\x03" (_ head body))
               (log-pex-info "<- ~s ~s" head body)
               (bytes->string/utf-8 body)))))))


    (define/public (get-relay id)
      (unless (hash-has-key? relays id)
        (raise-arguments-error 'get-relay "invalid relay" "id" id))
      (hash-ref relays id))

    (define/public (get-fader id)
      (unless (hash-has-key? faders id)
        (raise-arguments-error 'get-fader "invalid fader" "id" id))
      (hash-ref faders id))


    (define/public (list-relays)
      (sort (hash-keys relays) <))

    (define/public (list-faders)
      (sort (hash-keys faders) <))

    (super-new)))


(: relay% Relay%)
(define relay%
  (class object%
    (init-field bank id)

    (: get-status (-> String))
    (define/public (get-status)
      (let* ((bank-id (get-field id bank))
             (str-id (~a id #:width 2 #:pad-string "0" #:align 'right))
             (head (format "?d~a~a" bank-id str-id))
             (status (send bank command head "000099")))
        (unless status
          (error 'get-status "invalid relay"))
        (values status)))

    (define/public (get-on?)
      (let* ((status (get-status))
             (value (char->integer (string-ref status 2))))
        (bitwise-bit-set? value 0)))

    (define/public (set-on! v)
      (let* ((bank-id (get-field id bank))
             (slot (floor (/ (sub1 id) 6)))
             (flip (modulo (sub1 id) 6))
             (mask (arithmetic-shift 1 flip))
             (head (format "d~a00" (integer->char (+ #x40 bank-id))))
             (body (make-string 32 #\0)))

        ;; Flip correct bit in the "on" (base-0) or "off" (base-16) section
        ;; of the message. This allows us to not touch other relays.
        (string-set! body
                     (+ (if v 0 16) slot)
                     (integer->char (+ #x30 mask)))

        (void
          (send bank command head body))))


    (super-new)))


(: fader% Fader%)
(define fader%
  (class object%
    (init-field bank id)

    (define/public (get-status)
      (let* ((bank-id (get-field id bank))
             (str-id (~a id #:width 2 #:pad-string "0" #:align 'right))
             (head (format "?f~a~a" bank-id str-id))
             (status (send bank command head "000099")))
        (unless status
          (error 'get-status "invalid fader"))
        (values status)))

    (define/public (get-level)
      (let ((level (string->number (substring (get-status) 3 5))))
        (cast level Natural)))

    (define/public (set-level! v)
      (when (> v 99)
        (set! v 99))

      (let* ((bank-id (get-field id bank))
             (str-level (~a v #:width 2 #:pad-string "0" #:align 'right))
             (head (format "f~a0" str-level))
             (body (make-string 33 #\/)))
        (string-set! body 0 (integer->char (+ #x30 bank-id)))
        (string-set! body id #\3)
        (void
          (send bank command head body))))

    (define/public (fade-to-level! v)
      (when (> v 99)
        (set! v 99))

      (define direction
        (if (> v (get-level)) #\) #\())

      (let* ((bank-id (get-field id bank))
             (str-level (~a v #:width 2 #:pad-string "0" #:align 'right))
             (head (format "f~a0010" str-level))
             (body (make-string 33 #\/)))
        (string-set! body 0 (integer->char (+ #x30 bank-id)))
        (string-set! body id direction)
        (void
          (send bank command head body))))

    (super-new)))


; vim:set ts=2 sw=2 et:
