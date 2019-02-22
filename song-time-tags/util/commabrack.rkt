#lang racket/base

(provide (for-syntax atom
                     bexpr
                     ;; ---
                     commaseq
                     commaseq+
                     commaseq++
                     ))

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/class/paren-shape))

(begin-for-syntax
  (define-syntax-class atom
    #:attributes [norm]
    #:datum-literals [unquote]
    [pattern {~and {~not {~or (unquote . _) {~brackets _ ...}}}
                   norm:expr}])

  (define-splicing-syntax-class brackapp
    [pattern {~seq f:atom {~brackets a:commaseq}}
      #:with norm #'(f.norm a.norm ...)]
    [pattern {~seq {~brackets f:atom a:commaseq}}
      #:with norm #'(f.norm a.norm ...)])

  (define-splicing-syntax-class commabrackapp
    #:datum-literals [unquote]
    [pattern {~seq ({~datum unquote} f:atom) {~brackets a:commaseq}}
      #:with norm #'(f.norm a.norm ...)]
    [pattern {~seq ({~datum unquote} {~brackets f:atom a:commaseq})}
      #:with norm #'(f.norm a.norm ...)])

  (define-splicing-syntax-class commaseq+
    #:attributes [[norm 1]]
    [pattern {~seq a:bexpr b:commabexpr ...}
      #:with [norm ...] #'(a.norm b.norm ...)])
  (define-splicing-syntax-class commaseq++
    #:attributes [[norm 1]]
    [pattern {~seq a:bexpr b:commabexpr ...+}
      #:with [norm ...] #'(a.norm b.norm ...)])
  (define-splicing-syntax-class commaseq
    #:attributes [[norm 1]]
    [pattern {~seq} #:with [norm ...] '()]
    [pattern {~seq :commaseq+}])

  (define-splicing-syntax-class bexpr
    #:attributes [norm]
    [pattern {~seq :brackapp}]
    [pattern {~seq :atom}])

  (define-splicing-syntax-class commabexpr
    #:attributes [norm]
    #:datum-literals [unquote]
    [pattern {~seq :commabrackapp}]
    [pattern {~seq ({~datum unquote} :atom)}]))

