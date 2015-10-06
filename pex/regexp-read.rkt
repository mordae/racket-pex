#lang racket/base
;
;
;

(require racket/port
         racket/match)

(provide regexp-match-bytes-evt)


(define (regexp-match-bytes-evt b-rx in)
  (wrap-evt (regexp-match-evt b-rx in)
            (λ (result)
              (match result
                ((list-rest head _)
                 (values head))

                (eof-or-false
                  (values eof-or-false))))))


; vim:set ts=2 sw=2 et:
