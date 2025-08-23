(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
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
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
            (args (map (lambda (rand) (value-of rand env)) rands)))
              (apply-procedure proc args)))
        
        (varproc-exp (body)
          (proc-val (procedure 'args body env)))
        
        (each-exp (id body-exp)
          (let* ((args (expval->list (apply-env env 'args)))
            (results (map (apply-each body-exp id env) args)))
              (list-val results)))
        
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (var body saved-env)
        (if (eqv? var 'args)
            ;; varproc case: bind *all* vals as a list
            (value-of body
                      (extend-env 'args (list-val vals) saved-env))
            ;; normal proc: must have exactly one argument
            (if (= (length vals) 1)
                (value-of body
                          (extend-env var (car vals) saved-env))
                (eopl:error 'apply-procedure
                            "Procedure ~s expected 1 argument, got ~s"
                            var (length vals))))))))

  
  (define (apply-each exp id env)
    (lambda (arg)
      (value-of exp (extend-env id arg env))))
  
  )

  
