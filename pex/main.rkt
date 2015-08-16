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

(provide Bank%
         Relay%
         Fader%
         bank%
         relay%
         fader%)


(define-logger device)


(define-type Bank%
  (Class
    (init-field (path Path-String)
                (id Natural))

    (command (-> String String (U False String)))

    (list-relays (-> (Listof Natural)))
    (list-faders (-> (Listof Natural)))

    (get-relay (-> Natural (Instance Relay%)))
    (get-fader (-> Natural (Instance Fader%)))))


(define-type Relay%
  (Class
    (init-field (bank (Instance Bank%))
                (id Natural))

    (get-on? (-> Boolean))
    (set-on! (-> Boolean Void))))


(define-type Fader%
  (Class
    (init-field (bank (Instance Bank%))
                (id Natural))

    (get-level (-> Natural))
    (set-level! (-> Natural Void))))


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

    (define relays : (HashTable Natural (Instance Relay%))
      (make-hash))

    (define faders : (HashTable Natural (Instance Fader%))
      (make-hash))


    (begin
      (for ((relay-id 100))
        (when (relay-status relay-id)
          (let ((relay (new relay% (bank this) (id relay-id))))
            (hash-set! relays relay-id relay)))))


    ;; TODO: look for faders


    (define/public (command head body)
      (with-semaphore lock
        (let ((str (string-append "\x01" head "\x02" body "\x17\x03")))
          (log-device-debug "-> ~a ~a" head body)
          (write-string str out)
          (flush-output out))

        (let/ec return : (U False String)
          (unless (sync/timeout 0.05 in)
            (log-device-debug "<- no reply")
            (return #f))

          (define reply
            (regexp-match #"\x01.*?\x03" in))

          (unless reply
            (error 'command "protocol error"))

          (match (car reply)
            ((regexp-parts #"\x01(.*?)\x02(.*?)\x17\x03" (_ head body))
             (log-device-debug "<- ~s ~s" head body)
             (bytes->string/utf-8 body))))))


    (: relay-status (-> Natural (U Boolean String)))
    (define/private (relay-status relay-id)
      (let ((str-id (~a relay-id #:width 2 #:pad-string "0" #:align 'right)))
        (command (format "?d~a~a" id str-id) "000003")))


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
    (define/private (get-status)
      (let* ((bank-id (get-field id bank))
             (str-id (~a id #:width 2 #:pad-string "0" #:align 'right))
             (head (format "?d~a~a" bank-id str-id))
             (status (send bank command head "000003")))
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

        ;; Flip correct on or off bit.
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

    (define/public (get-level)
      0)

    (define/public (set-level! v)
      (void))

    (super-new)))


; vim:set ts=2 sw=2 et:
