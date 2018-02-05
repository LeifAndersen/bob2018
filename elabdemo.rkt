#lang slideshow

(provide (all-defined-out))
(require racket/gui/base
         slideshow/repl
         slideshow/code
         "utils.rkt")

(define (make-repl-slide c)
  (define (make-ns)
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (namespace-require 'compatibility/defmacro)
      (namespace-require 'syntax/parse/define)
      (namespace-require 'racket)
      (eval (syntax->datum c)))
    ns)
  (slide
   (scale
    (code #,c)
    1.4))
  (slide
   (code #,c)
   (repl-area
    #:make-namespace make-ns
    #:width 900
    #:height 300)))

(define (make-repl-only-slide . data)
  (slide
    (repl-area
  #:make-namespace (λ ()
                     (define ns (make-base-namespace))
                     (parameterize ([current-namespace ns])
                       (map eval data))
                     ns)
  #:width 900
  #:height 600)))
