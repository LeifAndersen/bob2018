#lang slideshow

(provide (all-defined-out))
(require racket/gui/base
         ppict/pict
         ppict/slideshow2
         slideshow/repl
         slideshow/code
         syntax/to-string
         "utils.rkt")

(define (make-repl-slides c
                          #:background [background #f]
                          #:middle [middle #f]
                          . init)
  (define (make-ns)
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (namespace-require 'compatibility/defmacro)
      (namespace-require 'syntax/parse/define)
      (namespace-require 'racket)
      (eval (syntax->datum c)))
    ns)
  (pslide
   #:go (coord 1/2 1/2 'cc)
   (or background (blank))
   #:go (coord 1/2 1/2 'cc)
   (scale
    (code #,c)
    1.4))
  (when middle
    (middle))
  (for ([i (in-list init)])
    (slide
     (code #,c)
     (apply repl-area
            #:make-namespace make-ns
            #:width 900
            #:height 450
            (if init
                (list (string-replace (syntax->string #`(#,i)) "\n" "\n  "))
                (list))))))

(define (make-repl-only-slide #:init [init #f]
                              . data)
  (define init*
    (cond [(list? init) init]
          [(syntax? init) (list init)]
          [else (list)]))
  (for ([i (in-list init*)])
    (slide
     (apply repl-area
            #:make-namespace (λ ()
                               (define ns (make-base-namespace))
                               (parameterize ([current-namespace ns])
                                 (map eval data))
                               ns)
            #:width 900
            #:height 600
            (if i
                (list (string-replace (syntax->string #`(#,i)) "\n" "\n  "))
                (list))))))
