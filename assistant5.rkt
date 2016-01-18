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

(define IN1-STR-INIT "(append '() '())")
(define OUT1-STR-INIT "()")

(define IN2-STR-INIT "(append '() '(foo bar))")
(define OUT2-STR-INIT "(foo bar)")

(define IN3-STR-INIT "(append '(a b c) '(d e))")
(define OUT3-STR-INIT "(a b c d e)")

(define *fresh-vars-str* FRESH-VARS-STR-INIT)
(define *program-name-str* PROGRAM-NAME-STR-INIT)
(define *program-str* PROGRAM-STR-INIT)

(define *in1-str* IN1-STR-INIT)
(define *out1-str* OUT1-STR-INIT)

(define *in2-str* IN2-STR-INIT)
(define *out2-str* OUT2-STR-INIT)

(define *in3-str* IN3-STR-INIT)
(define *out3-str* OUT3-STR-INIT)

(define make-fail-handler
  (lambda (display-full-query display-ans1 display-ans2 display-ans3)
    (lambda (exn)
      (send display-full-query set-label "-")
      (send display-ans1 set-label "-")
      (send display-ans2 set-label "-")
      (send display-ans3 set-label "-"))))

(define make-run-query
  (lambda (display-full-query display-ans1 display-ans2 display-ans3)
    (lambda ()
      (let ((fresh-vars-str *fresh-vars-str*)
            (program-name-str *program-name-str*)
            (program-str *program-str*)

            (in1-str *in1-str*)
            (out1-str *out1-str*)

            (in2-str *in2-str*)
            (out2-str *out2-str*)

            (in3-str *in3-str*)
            (out3-str *out3-str*))
        (let ((full-query1-str (format "(run 1 (q) (fresh ~a (let ((in-tmp `~a) (out-expr `~a)) (let ((in-expr `(letrec ((~a ~a)) ,in-tmp))) (fresh () (== (list in-expr out-expr) q) (evalo in-expr out-expr))))))" fresh-vars-str in1-str out1-str program-name-str program-str))
              (full-query2-str (format "(run 1 (q) (fresh ~a (let ((in-tmp `~a) (out-expr `~a)) (let ((in-expr `(letrec ((~a ~a)) ,in-tmp))) (fresh () (== (list in-expr out-expr) q) (evalo in-expr out-expr))))))" fresh-vars-str in2-str out2-str program-name-str program-str))
              (full-query3-str (format "(run 1 (q) (fresh ~a (let ((in-tmp `~a) (out-expr `~a)) (let ((in-expr `(letrec ((~a ~a)) ,in-tmp))) (fresh () (== (list in-expr out-expr) q) (evalo in-expr out-expr))))))" fresh-vars-str in3-str out3-str program-name-str program-str)))

          (display "full-query1-str:")
          (display full-query1-str)

          (display "full-query2-str:")
          (display full-query2-str)

          (display "full-query3-str:")
          (display full-query3-str)
          
          (newline)
          (let ((sp1 (open-input-string full-query1-str))
                (sp2 (open-input-string full-query2-str))
                (sp3 (open-input-string full-query3-str)))
            (let ((query1 (read sp1))
                  (query2 (read sp2))
                  (query3 (read sp3)))

              (display "query1:")
              (display query1)
              (newline)

              (display "query2:")
              (display query2)
              (newline)

              (display "query3:")
              (display query3)
              (newline)

              (let ((e1 (engine
                         (lambda (_)
                           (display "in engine 1...")
                           (newline)
                           (display "calculating ans1...")
                           (newline)
                           (let ((ans1 (eval query1)))
                             (display "ans1:")
                             (display ans1)
                             (newline)
                             ans1))))
                    (e2 (engine
                         (lambda (_)
                           (display "in engine 2...")
                           (newline)
                           (display "calculating ans2...")
                           (newline)
                           (let ((ans2 (eval query2)))
                             (display "ans2:")
                             (display ans2)
                             (newline)
                             ans2))))
                    (e3 (engine
                         (lambda (_)
                           (display "in engine 3...")
                           (newline)
                           (display "calculating ans3...")
                           (newline)
                           (let ((ans3 (eval query3)))
                             (display "ans3:")
                             (display ans3)
                             (newline)
                             ans3)))))
                (let ((completed1 (engine-run TIMEOUT-MS e1))
                      (completed2 (engine-run TIMEOUT-MS e2))
                      (completed3 (engine-run TIMEOUT-MS e3)))
                  (engine-kill e1)
                  (engine-kill e2)
                  (engine-kill e3)
                  (if completed1
                      (let ((ans1 (engine-result e1)))
                        (if (null? ans1)
                            (begin
                              (send display-ans1 set-label "-"))
                            (begin
                              (send display-ans1 set-label (format "~s" (car ans1))))))
                      (begin
                        (send display-ans1 set-label "timed out")))
                  (if completed2
                      (let ((ans2 (engine-result e2)))
                        (if (null? ans2)
                            (begin
                              (send display-ans2 set-label "-"))
                            (begin
                              (send display-ans2 set-label (format "~s" (car ans2))))))
                      (begin
                        (send display-ans2 set-label "timed out")))
                  (if completed3
                      (let ((ans3 (engine-result e3)))
                        (if (null? ans3)
                            (begin
                              (send display-ans3 set-label "-"))
                            (begin
                              (send display-ans3 set-label (format "~s" (car ans3))))))
                      (begin
                        (send display-ans3 set-label "timed out"))))))))))))

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
             (txt-in1 (new text-field%
                           (label "in1")
                           (parent frame)
                           (init-value IN1-STR-INIT)
                           (callback (lambda (button event)
                                       (with-handlers ([exn:fail? fail-handler])
                                         (let ((in1-str (send button get-value)))
                                           (set! *in1-str* in1-str)
                                           (run-query)))))))
             (txt-out1 (new text-field%
                            (label "out1")
                            (parent frame)
                            (init-value OUT1-STR-INIT)
                            (callback (lambda (button event)
                                        (with-handlers ([exn:fail? fail-handler])
                                          (let ((out1-str (send button get-value)))
                                            (set! *out1-str* out1-str)
                                            (run-query)))))))
             (display-ans1 (new message% (parent frame)
                                (label "display-ans1")
                                (auto-resize #t)))


             (txt-in2 (new text-field%
                           (label "in2")
                           (parent frame)
                           (init-value IN2-STR-INIT)
                           (callback (lambda (button event)
                                       (with-handlers ([exn:fail? fail-handler])
                                         (let ((in2-str (send button get-value)))
                                           (set! *in2-str* in2-str)
                                           (run-query)))))))
             (txt-out2 (new text-field%
                            (label "out2")
                            (parent frame)
                            (init-value OUT2-STR-INIT)
                            (callback (lambda (button event)
                                        (with-handlers ([exn:fail? fail-handler])
                                          (let ((out2-str (send button get-value)))
                                            (set! *out2-str* out2-str)
                                            (run-query)))))))
             (display-ans2 (new message% (parent frame)
                                (label "display-ans2")
                                (auto-resize #t)))


             (txt-in3 (new text-field%
                           (label "in3")
                           (parent frame)
                           (init-value IN3-STR-INIT)
                           (callback (lambda (button event)
                                       (with-handlers ([exn:fail? fail-handler])
                                         (let ((in3-str (send button get-value)))
                                           (set! *in3-str* in3-str)
                                           (run-query)))))))
             (txt-out3 (new text-field%
                            (label "out3")
                            (parent frame)
                            (init-value OUT3-STR-INIT)
                            (callback (lambda (button event)
                                        (with-handlers ([exn:fail? fail-handler])
                                          (let ((out3-str (send button get-value)))
                                            (set! *out3-str* out3-str)
                                            (run-query)))))))
             (display-ans3 (new message% (parent frame)
                                (label "display-ans3")
                                (auto-resize #t)))
             
             
             (display-full-query (new message% (parent frame)
;                                      (label "display-full-query")
                                      (label "")
                                      (auto-resize #t)))
             (fail-handler (make-fail-handler display-full-query display-ans1 display-ans2 display-ans3))
             (run-query (make-run-query display-full-query display-ans1 display-ans2 display-ans3)))
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
