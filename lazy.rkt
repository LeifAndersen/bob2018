#lang scratch

(module A racket
  (require (for-syntax syntax/parse)
           syntax/parse/define
           syntax/wrap-modbeg)
  (provide (rename-out [#%lazy-app #%app]
                       [#%lazy-module-begin #%module-begin]
                       [#%lazy-top-interaction #%top-interaction]
                       [lazy-displayln displayln]
                       [lazy-lambda lambda]
                       [lazy-+ +])
           define
           #%datum
           let)
  (define-syntax-parser #%lazy-app
    [(_ rator rand ...) #'(lazy (#%app rator (lazy rand) ...))])
  (define-syntax #%lazy-module-begin
    (make-wrapping-module-begin
     #'force #'#%module-begin))
  (define-syntax (#%lazy-top-interaction stx)
    (syntax-parse stx
      [(_ . form)
       #'(#%top-interaction . (force form))]))
  (define (strictify f)
    (lambda args
      (apply f (map force args))))
  (define-syntax-parser lazy-lambda
    [(_ (args:id ...) body ...)
     #:with (new-args ...) (syntax-local-introduce #'(args ...))
     #'(lambda (new-args ...)
         (let-syntax ([args (syntax-parser
                              [name #'(force new-args)])]
                      ...)
           body ...))])
  (define lazy-+ (strictify +))
  (define lazy-displayln (strictify displayln)))

(module B (submod ".." A)
  (displayln "Hello")
  (define x ((lambda (x) (x x)) (lambda (x) (x x))))
  (displayln "World"))

(require 'A)
;(require 'B)
