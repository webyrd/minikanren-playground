#lang racket

;;; Runs a query expression, with a timeout, in the context of the relational interpreter.
;;; The query expression is evaluated in the context of `(run 1 (q) ,query).
;;; For example, '(fresh (x y) (evalo x y) (== (list x y) q))' is a legitimate input.

(require "mk.rkt")
(require "interp-uber.rkt")
(require racket/gui/base)
(require racket/engine)


;;; timeout, in milliseconds
(define TIMEOUT-MS 500)


;;; window size
(define HORIZ-SIZE 800)
(define VERT-SIZE 400)

(define FRESH-VARS-STR-INIT "(a c t u v w x y z)")
(define PROGRAM-NAME-STR-INIT "append")
(define PROGRAM-STR-INIT "(lambda (l s) (if (null? l) s (cons (car l) (append (cdr l) s))))")
(define IN-STR-INIT "(append '(a b c) '(d e))")
(define OUT-STR-INIT "(a b c d e)")

(define *fresh-vars-str* FRESH-VARS-STR-INIT)
(define *program-name-str* PROGRAM-NAME-STR-INIT)
(define *program-str* PROGRAM-STR-INIT)
(define *in-str* IN-STR-INIT)
(define *out-str* OUT-STR-INIT)

(define make-fail-handler
  (lambda (display-full-query display-ans)
    (lambda (exn)
      (send display-full-query set-label "-")
      (send display-ans set-label "-"))))

(define make-run-query
  (lambda (display-full-query display-ans)
    (lambda ()
      (let ((fresh-vars-str *fresh-vars-str*)
            (program-name-str *program-name-str*)
            (program-str *program-str*)
            (in-str *in-str*)
            (out-str *out-str*))
        (let ((full-query-str (format "(run 1 (q) (fresh ~a (let ((in-tmp `~a) (out-expr `~a)) (let ((in-expr `(letrec ((~a ~a)) ,in-tmp))) (fresh () (== (list in-expr out-expr) q) (evalo in-expr out-expr))))))" fresh-vars-str in-str out-str program-name-str program-str)))
          (display "full-query-str:")
          (display full-query-str)
          (newline)
          (let ((sp (open-input-string full-query-str)))
            (let ((query (read sp)))
              (display "query:")
              (display query)
              (newline)
              (let ((e (engine
                        (lambda (_)
                          (display "in engine...")
                          (newline)
;                          (send display-full-query set-label (format "~s" query))
;                          (send display-full-query set-label full-query-str)                          
                          (display "calculating ans...")
                          (newline)
                          (let ((ans (eval query)))
                            (display "ans:")
                            (display ans)
                            (newline)
                            ans)))))
                (let ((completed (engine-run TIMEOUT-MS e)))
                  (engine-kill e)
                  (if completed
                      (let ((ans (engine-result e)))
                        (if (null? ans)
                            (begin
                              (send display-ans set-label "-"))
                            (begin
                              (send display-ans set-label (format "~s" (car ans))))))
                      (begin
                        (send display-ans set-label "timed out"))))))))))))

(define (assistant)
  (let ((frame (new frame%
                    (label "Synthesis Assistant")
                    (width HORIZ-SIZE)
                    (height VERT-SIZE))))
    (letrec ((txt-fresh-vars (new text-field%
                                  (label "fresh-vars")
                                  (parent frame)
                                  (init-value FRESH-VARS-STR-INIT)
                                  (callback (lambda (button event)
                                              (with-handlers ([exn:fail? fail-handler])
                                                (let ((fresh-vars-str (send button get-value)))
                                                  (set! *fresh-vars-str* fresh-vars-str)
                                                  (run-query)))))))
             (txt-program-name (new text-field%
                                    (label "program-name")
                                    (parent frame)
                                    (init-value PROGRAM-NAME-STR-INIT)
                                    (callback (lambda (button event)
                                                (with-handlers ([exn:fail? fail-handler])
                                                  (let ((program-name-str (send button get-value)))
                                                    (set! *program-name-str* program-name-str)
                                                    (run-query)))))))
             (txt-program (new text-field%
                               (label "program")
                               (parent frame)
                               (style '(multiple))
                               (init-value PROGRAM-STR-INIT)
                               (callback (lambda (button event)
                                           (with-handlers ([exn:fail? fail-handler])
                                             (let ((program-str (send button get-value)))
                                               (set! *program-str* program-str)
                                               (run-query)))))))
             (txt-in (new text-field%
                          (label "in")
                          (parent frame)
                          (init-value IN-STR-INIT)
                          (callback (lambda (button event)
                                      (with-handlers ([exn:fail? fail-handler])
                                        (let ((in-str (send button get-value)))
                                          (set! *in-str* in-str)
                                          (run-query)))))))
             (txt-out (new text-field%
                           (label "out")
                           (parent frame)
                           (init-value OUT-STR-INIT)
                           (callback (lambda (button event)
                                       (with-handlers ([exn:fail? fail-handler])
                                         (let ((out-str (send button get-value)))
                                           (set! *out-str* out-str)
                                           (run-query)))))))
             (display-ans (new message% (parent frame)
                               (label "display-ans")
                               (auto-resize #t)))
             (display-full-query (new message% (parent frame)
;                                      (label "display-full-query")
                                      (label "")
                                      (auto-resize #t)))
             (fail-handler (make-fail-handler display-full-query display-ans))
             (run-query (make-run-query display-full-query display-ans)))
      (send frame show #t))))


#|
(let ((fresh-vars-str "(a* e in out)")
      (program-name-str "fn")
      (program-str "(lambda ,a* ,e)")
      (in-str ",in")
      (out-str ",out"))
  (format
   "(run 1 (q) (fresh ~a (let ((in-tmp `~a) (out-expr `~a)) (let ((in-expr `(letrec ((~a ~a)) ,in-tmp))) (fresh () (== (list in-expr out-expr) q) (evalo in-expr out-expr))))))" fresh-vars-str in-str out-str program-name-str program-str))
=>
"(run 1 (q) (fresh (a* e in out) (let ((in-tmp `,in) (out-expr `,out)) (let ((in-expr `(letrec ((fn (lambda ,a* ,e))) ,in-tmp))) (fresh () (== (list in-expr out-expr) q) (evalo in-expr out-expr))))))"

(let ((fresh-vars-str "(a* e x)")
      (program-name-str "fn")
      (program-str "(lambda ,a* ,e)")
      (in-str "(fn 5)")
      (out-str "5"))
  (format
   "(run 1 (q) (fresh ~a (let ((in-tmp `~a) (out-expr `~a)) (let ((in-expr `(letrec ((~a ~a)) ,in-tmp))) (fresh () (== (list in-expr out-expr) q) (evalo in-expr out-expr))))))" fresh-vars-str in-str out-str program-name-str program-str))
=>
"(run 1 (q) (fresh (a* e x) (let ((in-tmp `(fn 5)) (out-expr `5)) (let ((in-expr `(letrec ((fn (lambda ,a* ,e))) ,in-tmp))) (fresh () (== (list in-expr out-expr) q) (evalo in-expr out-expr))))))"

> (run 1 (q) (fresh (a* e x) (let ((in-tmp `(fn 5)) (out-expr `5)) (let ((in-expr `(letrec ((fn (lambda ,a* ,e))) ,in-tmp))) (fresh () (== (list in-expr out-expr) q) (evalo in-expr out-expr))))))
'((((letrec ((fn (lambda _.0 '5))) (fn 5)) 5) (=/= ((_.0 quote))) (sym _.0)))
|#
