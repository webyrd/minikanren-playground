#lang racket

;;; Runs a query expression, with a timeout, in the context of the relational interpreter.
;;; The query expression is evaluated in the context of `(run 1 (q) ,query).
;;; For example, '(fresh (x y) (evalo x y) (== (list x y) q))' is a legitimate input.

(require "mk.rkt")
(require "interp-uber.rkt")
(require racket/gui/base)
(require racket/engine)

;;; timeout, in milliseconds
(define TIMEOUT-MS 100)


;;; window size
(define HORIZ-SIZE 800)
(define VERT-SIZE 700)


(define (assistant)
  (let ((frame (new frame%
                    (label "Synthesis Assistant")
                    (width HORIZ-SIZE)
                    (height VERT-SIZE))))
    (let ((display-ans (new message% (parent frame)
                            (label "display-ans")
                            (auto-resize #t))))
      (letrec ((txt-query (new text-field%
                               (label "query")
                               (parent frame)
                               (init-value "?")
                               (callback (lambda (button event)
                                           (with-handlers ([exn:fail?
                                                            (lambda (exn)
                                                              (send display-ans set-label "-"))])
                                             (let ((query-str (send button get-value)))
                                               (let ((sp (open-input-string query-str)))
                                                 (let ((query (read sp)))
                                                   (let ((e (engine
                                                             (lambda (_)
                                                               (eval `(run 1 (q) ,query))))))
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
                                                             (send display-ans set-label "timed out"))))))))))))))
        (send frame show #t)))))
