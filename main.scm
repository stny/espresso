#!/usr/bin/env gosh

(add-load-path "." :relative)
(use signal-handler)

(define prompt "espresso>> ")

(define gauche-apply apply)
(define gauche-eval eval)

; apply {{{

(define (apply procedure args)
  (display "apply: ")
  (print procedure)
  (cond ((primitive-procedure? procedure)
         (gauche-apply (cdr procedure) args))
        ((extend-procedure? procedure)
         (extend-apply (cdr procedure) args))
        (else (error "Unknown procedure -- APPLY" procedure))))

(define (extend-apply proc-info args)
  (let ((proc-args (car proc-info))
        (proc-body (caadr proc-info))
        (proc-base-env (caddr proc-info)))
    (let
        ((env (extend-frame (make-frame) proc-base-env)))
        (define (define-loop variables values)
          (if (null? variables)
            '()
            (begin
              (define-variable! (car variables) (car values) env)
              (define-loop (cdr variables) (cdr values)))))
        (define-loop proc-args args)
        (eval proc-body env))))

; }}}
; eval {{{

(define (eval exp env)
  (display "eval: ")
  (print exp)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (find-variable exp env))
        ((quoted? exp) (eval-quote exp))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (eval-lambda exp env))
        ((definition? exp) (eval-definition exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (extract-operands (operands exp) env)))
  (else
    (error "EVAL ERROR" exp))))

; }}}
; detector {{{
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (check-element? exp 'quote))

(define (if? exp)
  (check-element? exp 'if)
  )

(define (primitive-procedure? exp)
  (check-element? exp 'primitive)
  )

(define (extend-procedure? exp)
  (check-element? exp 'procedure)
  )

(define (definition? exp)
  (check-element? exp 'define))

(define (assignment? exp)
  (check-element? exp 'set!))

(define (lambda? exp)
  (check-element? exp 'lambda))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (check-element? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f
    ))

(define (application? exp)
  (pair? exp))

; }}}

(define (eval-quote exp)
  (cadr exp))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    #f))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env) env))

(define (eval-assignment exp env)
  (define (assignment-loop search-env)
    (if (null? search-env) (error "Unbound variable -- SET!"))
    (let ((frame (first-frame search-env))
          (variable (definition-variable exp))
          (value (eval (definition-value exp) env)))
      (if (hash-table-exists? frame variable)
        (add-binding-to-frame! variable value frame)
        (assignment-loop (cdr search-env)))))
  (assignment-loop env))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
     (cadr exp)
     (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (func-param exp) (func-body exp))))

(define (func-param exp)
  (cdr (car (cdr exp))))

(define (func-body exp)
  (cdr (cdr exp)))

(define (make-lambda param body)
  (cons 'lambda (cons param body)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (add-binding-to-frame! var val frame)))

(define (eval-lambda exp env)
  (list 'procedure (cadr exp) (cddr exp) env))

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

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

; primitive {{{
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
          (cons 'primitive
                (gauche-eval (car procedures) (interaction-environment))) frame)
        (install (cdr procedures)))
      )
    )
    (install primitive-procedures))

; }}}
; frame & environment {{{

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

(define global-environment (init-environment))

; }}}
; main {{{

(define (repl)
  (display prompt)
  (flush)
  (let ((input (read)))
    (if (eof-object? input)
      (begin
        (print "(╯°□°）╯︵ ┻━┻")
        (exit)))
    (let ((output (eval input global-environment)))
      (print output)
      (repl)
      )
    )
  )

(define (main args)
  (repl)
  0)

; }}}
