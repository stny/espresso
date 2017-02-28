#!/usr/bin/env gosh

(add-load-path "." :relative)
(use signal-handler)

(define prompt "espresso>> ")

(define gauche-apply apply)
(define gauche-eval eval)

(define (apply procedure args)
  (display "apply: ")
  (print procedure)
  (gauche-apply procedure args)
  )

(define (eval exp env)
  (display "eval: ")
  (print exp)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (find-variable exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (extract-operands (operands exp) env)))
  (else
    (error "EVAL ERROR" exp))))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (find-variable var env)
  (if (eq? env '()) (error "Unbound variable" var))
  (let ((frame (first-frame env)))
    (let ((value (frame-get frame var)))
      (if value
        value
        (find-variable var (cdr env))))))

(define (extract-operands exps env)
  (if (null? exps)
    '()
    (cons (eval (car exps) env) (extract-operands (cdr exps) env))
  ))

(define empty-frame '())

(define (make-frame)
  (make-hash-table))

(define (first-frame frame)
  (car frame))

(define (frame-get frame var)
  (hash-table-get frame var #f))

(define (add-binding-to-frame! var val frame)
  (hash-table-put! frame var val))

(define (extend-frame frame base-frame)
  (cons frame base-frame))

(define (init-environment)
  (let ((frame (make-frame)))
    (install-primitive-procedures frame)
    (extend-frame frame empty-frame)
    ))

(define primitive-procedures
  '(
    +
    -
    *
    /
    =
    >
    <
    car
    cdr
    cons
    eq?
    )
  )

(define (install-primitive-procedures frame)
  (define (install procedures)
    (if (null? procedures)
      '()
      (begin
        (add-binding-to-frame!
          (car procedures)
          (gauche-eval (car procedures) (interaction-environment)) frame)
        (install (cdr procedures)))
      )
    )
    (install primitive-procedures))

(define global-environment (init-environment))

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

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
