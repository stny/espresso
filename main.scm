#!/usr/bin/env gosh

(add-load-path "." :relative)
(use signal-handler)

(define prompt "latte>> ")

(define global-environment '())

(define gauche-eval eval)

(define (eval exp env)
  exp
  )

(define (repl)
  (display prompt)
  (flush)
  (let ((input (read)))
    (let ((output (eval input global-environment)))
      (print output)
      (repl)
      )
    )
  )
(define (main args)
  (repl)
  0)
