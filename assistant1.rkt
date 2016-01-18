#lang racket

;;; Very simple interactive interface to the relational interpreter, with 'expr' and 'value' text fields.
;;; Uses engines to enforce timeouts for potentially divergent inputs.

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
    (let ((display-expr (new message% (parent frame)
                             (label "display-expr")
                             (auto-resize #t)))
          (display-value (new message% (parent frame)
                              (label "display-value")
                              (auto-resize #t))))
      (letrec ((txt-expr (new text-field%
                              (label "expr")
                              (parent frame)
                              (init-value "?")
                              (callback (lambda (button event)
                                          (with-handlers ([exn:fail?
                                                           (lambda (exn)
                                                             (send display-value set-label "-")
                                                             (send display-expr set-label "-")
                                                             (send txt-value set-value "?"))])
                                            (let ((expr-str (send button get-value)))
                                              (let ((sp (open-input-string expr-str)))
                                                (let ((expr (read sp)))
                                                   (let ((e (engine
                                                             (lambda (_)
                                                               (run 1 (value) (evalo expr value))))))
                                                     (let ((completed (engine-run TIMEOUT-MS e)))
                                                       (engine-kill e)
                                                       (if completed
                                                           (let ((ans (engine-result e)))
                                                             (if (null? ans)
                                                                 (begin
                                                                   (send display-value set-label "-")
                                                                   (send display-expr set-label "-")
                                                                   (send txt-value set-value "?"))
                                                                 (begin
                                                                   (send display-expr set-label (format "~s" expr))
                                                                   (send display-value set-label (format "~s" (car ans)))
                                                                   (send txt-value set-value "?"))))
                                                           (begin
                                                             (send display-expr set-label "timed out")
                                                             (send display-value set-label "timed out")
                                                             (send txt-expr set-value "?")))))))))))))
               (txt-value (new text-field%
                               (label "value")
                               (parent frame)
                               (init-value "?")
                               (callback (lambda (button event)
                                           (with-handlers ([exn:fail?
                                                            (lambda (exn)
                                                              (send display-value set-label "-")
                                                              (send display-expr set-label "-")
                                                              (send txt-expr set-value "?"))])
                                             (let ((value-str (send button get-value)))
                                               (let ((sp (open-input-string value-str)))
                                                 (let ((value (read sp)))
                                                   (let ((e (engine
                                                             (lambda (_)
                                                               (run 1 (expr) (evalo expr value))))))
                                                     (let ((completed (engine-run TIMEOUT-MS e)))
                                                       (engine-kill e)
                                                       (if completed
                                                           (let ((ans (engine-result e)))
                                                             (if (null? ans)
                                                                 (begin
                                                                   (send display-expr set-label "-")
                                                                   (send display-value set-label "-")
                                                                   (send txt-expr set-value "?"))
                                                                 (begin
                                                                   (send display-value set-label (format "~s" value))
                                                                   (send display-expr set-label (format "~s" (car ans)))
                                                                   (send txt-expr set-value "?"))))
                                                           (begin
                                                             (send display-expr set-label "timed out")
                                                             (send display-value set-label "timed out")
                                                             (send txt-expr set-value "?"))))))))))))))
        (send frame show #t)))))
