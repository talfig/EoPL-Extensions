(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the LET language.

  (require "drscheme-init.scm")
  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; helpers ;;;;;;;;;;;;;;;;

  ;; Wrap exceptions in expval when they appear as plain except values.
  (define (check-exception v)
    (if (except? v) (excp-val v) v))

;;;;;;;;;;;;;;;; program entry ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; %value-of : Exp * Env -> (maybe raw) value
  ;; Internal worker that may (in legacy code paths) return raw values; caller wraps.
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;; constants
        (const-exp (num)
          (num-val num))

        ;; variables
        (var-exp (var)
          (check-exception (apply-env env var)))

        ;; subtraction
        (diff-exp (exp1 exp2)
          (let* ([val1 (value-of exp1 env)]
                 [val2 (value-of exp2 env)]
                 [num1 (expval->num val1)]
                 [num2 (expval->num val2)])
            (cond [(except? num1) (excp-val num1)]
                  [(except? num2) (excp-val num2)]
                  [else           (num-val (- num1 num2))])))

        ;; zero? test
        (zero?-exp (exp1)
          (let* ([val1 (value-of exp1 env)]
                 [num1 (expval->num val1)])
            (if (except? num1)
                (excp-val num1)
                (bool-val (zero? num1)))))

        ;; if
        (if-exp (exp1 exp2 exp3)
          (let* ([val1 (value-of exp1 env)]
                 [b1   (expval->bool val1)])
            (if (except? b1)
                (excp-val b1)
                (if (eq? b1 #t)
                    (value-of exp2 env)
                    (value-of exp3 env)))))

        ;; let
        (let-exp (var exp1 body)
          (let ([v1 (value-of exp1 env)])                ; already an expval
            (cases expval v1
              (excp-val (reason) (excp-val reason))
              (else (value-of body (extend-env var v1 env))))))

        ;; try/catch/finally
        (try-exp (exp1 excpts excptexps finexps)
          (if (> (length finexps) 1)
              (excp-val (exception "general"))
              (let ([ret (try-catch exp1 excpts excptexps env)])
                (begin
                  (for-each (lambda (e) (value-of e env)) finexps)
                  ret))))

        ;; throw
        (throw-exp (excpt)
          (excp-val excpt))
      )))

  ;; Evaluate exp1; if it yields (excp-val reason), dispatch to matching handler.
  (define (try-catch exp1 excpts excptexps env)
    (let ([v (value-of exp1 env)]) ; normalized to expval by public value-of
      (cases expval v
        (excp-val (reason)
          (catch-exception reason excpts excptexps env))
        (else v))))

  ;; Match reason against excpts; on hit, evaluate corresponding handler expression.
  (define (catch-exception reason excpts excptexps env)
    (cond
      [(null? excpts)
       (excp-val reason)]
      [(equal? reason (car excpts))
       (value-of (car excptexps) env)]
      [else
       (catch-exception reason (cdr excpts) (cdr excptexps) env)]))

)
