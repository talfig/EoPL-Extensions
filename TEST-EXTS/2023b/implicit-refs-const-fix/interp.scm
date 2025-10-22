(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) 
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var) (deref (apply-env env var)))

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
        (let-exp (opt var exp1 body)
          (let ((v1 (value-of exp1 env)))
            (cases option (extend-env-record->opt env)
              (opt-null () 
                        (value-of body
                                  (extend-env var (newref v1) opt env)))
              (else (report-const-error 'let-exp var)))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator opt rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc opt arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (let ((saved-env (apply-env->old-env env var)))
              (if (null? saved-env)
                  (eopl:error 'apply-env "No binding for ~s" var)
                  (cases option (extend-env-record->opt saved-env)
                    (opt-null () 
                              (setref!
                               (apply-env env var)
                               (value-of exp1 env)))
                    (else (report-const-error 'assign-exp var))))))
          (num-val 27))

        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  (define apply-procedure
    (lambda (proc1 opt arg)
      (cases proc proc1
        (procedure (var body env)
          (let*  ((saved-env (apply-env->old-env env var))
                  (new-env (extend-env var (newref arg) opt env))
                  (ret (value-of body new-env)))
            (if (null? saved-env)
                ret
                (cases option (extend-env-record->opt saved-env)
                  (opt-null () ret)
                  (else (report-const-error 'call-exp var))
                  )))
            ))))

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))
  
  (define report-const-error
    (lambda (proc-name var)
      (eopl:error proc-name "Cannot reassign const ~s" var)
      ))
  
 (define (extend-env-record->opt env)
   (cases environment env
     (extend-env (bvar bval opt saved-env) opt)
     (else (eopl:error "Unexpected environment ~s" env)))
   )

  )
  


  
