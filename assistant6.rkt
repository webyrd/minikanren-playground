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
  (lambda (display-ans)
    (lambda (exn)
      (send display-ans set-label "-"))))

(define make-super-fail-handler
  (lambda (display-ans1 display-ans2 display-ans3)
    (lambda (exn)
      (send display-ans1 set-label "-")
      (send display-ans2 set-label "-")
      (send display-ans3 set-label "-"))))

(define make-run-query
  (lambda (display-ans in-str out-str)
    (lambda ()
      (let ((fresh-vars-str *fresh-vars-str*)
            (program-name-str *program-name-str*)
            (program-str *program-str*))

        (display "in run-query")
        (newline)
                
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

(define-syntax run-everything
  (syntax-rules ()
    [(_ button str *str* display-ans1 display-ans2 display-ans3)
     (begin

       (display "entering run-everything")
       (newline)
       
       (with-handlers ([exn:fail? (make-super-fail-handler display-ans1 display-ans2 display-ans3)])
         (let ((str (send button get-value)))
           (set! *str* str)))

       (display "1")
       (newline)
       
       (with-handlers ([exn:fail? (make-fail-handler display-ans1)])
         ((make-run-query display-ans1 *in1-str* *out1-str*)))

       (display "2")
       (newline)
       
       (with-handlers ([exn:fail? (make-fail-handler display-ans2)])
         ((make-run-query display-ans2  *in2-str* *out2-str*)))

       (display "3")
       (newline)

       (with-handlers ([exn:fail? (make-fail-handler display-ans3)])
         ((make-run-query display-ans3  *in3-str* *out3-str*)))

       (display "4")
       (newline)

       )]))

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
                                              (run-everything button fresh-vars-str *fresh-vars-str*
                                                              display-ans1 display-ans2 display-ans3)))))
             (txt-program-name (new text-field%
                                    (label "program-name")
                                    (parent frame)
                                    (init-value PROGRAM-NAME-STR-INIT)
                                    (callback (lambda (button event)
                                                (run-everything button program-name-str *program-name-str*
                                                                display-ans1 display-ans2 display-ans3)))))
             (txt-program (new text-field%
                               (label "program")
                               (parent frame)
                               (style '(multiple))
                               (init-value PROGRAM-STR-INIT)
                               (callback (lambda (button event)
                                           (run-everything button program-str *program-str*
                                                           display-ans1 display-ans2 display-ans3)))))
             
             (txt-in1 (new text-field%
                           (label "in1")
                           (parent frame)
                           (init-value IN1-STR-INIT)
                           (callback (lambda (button event)
                                       (run-everything button in1-str *in1-str*
                                                       display-ans1 display-ans2 display-ans3)))))
             (txt-out1 (new text-field%
                            (label "out1")
                            (parent frame)
                            (init-value OUT1-STR-INIT)
                            (callback (lambda (button event)
                                        (run-everything button out1-str *out1-str*
                                                        display-ans1 display-ans2 display-ans3)))))
             (display-ans1 (new message% (parent frame)
                                (label "display-ans1")
                                (auto-resize #t)))


             (txt-in2 (new text-field%
                           (label "in2")
                           (parent frame)
                           (init-value IN2-STR-INIT)
                           (callback (lambda (button event)
                                       (run-everything button in2-str *in2-str*
                                                       display-ans2 display-ans2 display-ans3)))))
             (txt-out2 (new text-field%
                            (label "out2")
                            (parent frame)
                            (init-value OUT2-STR-INIT)
                            (callback (lambda (button event)
                                        (run-everything button out2-str *out2-str*
                                                        display-ans2 display-ans2 display-ans3)))))
             (display-ans2 (new message% (parent frame)
                                (label "display-ans2")
                                (auto-resize #t)))

             (txt-in3 (new text-field%
                           (label "in3")
                           (parent frame)
                           (init-value IN3-STR-INIT)
                           (callback (lambda (button event)
                                       (run-everything button in3-str *in3-str*
                                                       display-ans3 display-ans2 display-ans3)))))
             (txt-out3 (new text-field%
                            (label "out3")
                            (parent frame)
                            (init-value OUT3-STR-INIT)
                            (callback (lambda (button event)
                                        (run-everything button out3-str *out3-str*
                                                        display-ans3 display-ans2 display-ans3)))))
             (display-ans3 (new message% (parent frame)
                                (label "display-ans3")
                                (auto-resize #t)))

             
             )
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
