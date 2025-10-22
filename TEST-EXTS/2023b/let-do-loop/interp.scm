(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (do-exp (ids inits steps bools results)
          (cond 
            ((null? ids)
             (eopl:error 'do-exp "do loop with no variables are not allowed"))
            ((null? bools)
             (eopl:error 'do-exp "do loop with no booleans are not allowed"))
            (else (do-loop
                   ids steps bools results (init-loop ids inits env)))))
        
        )))

  (define init-loop
    (lambda (ids inits env)
      (if (null? ids)
          env
          (let ((var (car ids))
                (val (value-of (car inits) env)))
            (init-loop (cdr ids) (cdr inits) (extend-env var val env))))))
  
  (define do-step
    (lambda (ids steps env vals)
      (if (null? ids)
          vals
          (let* ((var (car ids))
                 (val (apply-env env var))
                 (add (value-of (car steps) env)))
            (do-step (cdr ids) (cdr steps) env
                     (append vals (list 
                                   (const-exp (+ (expval->num val)
                                                 (expval->num add))))))))))
  
  (define check-bools
    (lambda (bools env)
      (if (null? bools)
          #f
          (if (expval->bool (value-of (car bools) env))
              #t
              (check-bools (cdr bools) env)))))
  
  (define loop-result
    (lambda (bools results env)
      (if (expval->bool (value-of (car bools) env))
          (value-of (car results) env)
          (loop-result (cdr bools) (cdr results) env))))

  (define do-loop
    (lambda (ids steps bools results env)
      (if (check-bools bools env)
          (loop-result bools results env)
          (do-loop 
           ids steps bools results (init-loop
                                    ids (do-step ids steps env '()) env)))))

  )

