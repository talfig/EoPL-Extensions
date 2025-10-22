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
          (value-of exp1 (init-env) (init-guard-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env guard-env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) 
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var) (deref (apply-env env var)))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env guard-env))
                (val2 (value-of exp2 env guard-env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env guard-env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env guard-env)))
            (if (expval->bool val1)
              (value-of exp2 env guard-env)
              (value-of exp3 env guard-env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (grd var exp1 body)
          (let* ((v1       (value-of exp1 env guard-env))
                 (existing (apply-env-no-error guard-env var)))
            (if (null? existing)
                (begin
                  (ensure-val-compatible! 'let-exp grd v1 var)
                  (value-of body
                            (extend-env var (newref v1) env)
                            (extend-env var (newref grd) guard-env)))
                (begin
                  (ensure-guard-compatible! 'let-exp (deref existing) grd var)
                  (value-of body
                            (extend-env var (newref v1) env)
                            guard-env)))))
  
        (proc-exp (grd var body)
          (let ((existing (apply-env-no-error guard-env var)))
            (if (null? existing)
                (proc-val (procedure var body env (extend-env var (newref grd) guard-env)))
                (begin 
                  (ensure-guard-compatible! 'proc-exp (deref existing) grd var)
                  (proc-val (procedure var body env guard-env))))))
        
        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env guard-env)))
                (arg  (value-of rand  env guard-env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env) guard-env))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env guard-env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
            (let ((existing (deref (apply-env guard-env var)))
                  (ref (apply-env env var))
                  (val (value-of exp1 env guard-env)))
              (begin 
                (ensure-val-compatible! 'assign-exp existing val var)
                (setref!
                 ref
                 val)
                (num-val 27))))

        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var body env guard-env)
                   (let ((grd (deref (apply-env guard-env var))))
                     (ensure-val-compatible! 'call-exp grd val var)
                     (value-of body
                               (extend-env var (newref val) env)
                               guard-env))))))
  
  ;; instrumented version
  ;; (define apply-procedure
  ;; (lambda (proc1 arg)
  ;;   (cases proc proc1
  ;;     (procedure (var body saved-env)
  ;;       (let ((r (newref arg)))
  ;;         (let ((new-env (extend-env var r saved-env)))
  ;;           (if (instrument-let)
  ;;             (begin
  ;;               (eopl:printf
  ;;                 "entering body of proc ~s with env =~%"
  ;;                 var)
  ;;               (pretty-print (env->list new-env)) 
  ;;               (eopl:printf "store =~%")
  ;;               (pretty-print (store->readable (get-store-as-list)))
  ;;               (eopl:printf "~%"))
  ;;             )
  ;;           (value-of body new-env)))))))  

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
  
  (define (guard-compatible? existing new)
    (if (or (null? existing) (null? new))
        #t
        (cases guard existing
          (guard-default () #t)
          (guard-num ()
                     (cases guard new
                       (guard-num () #t)
                       (guard-default () #t)
                       (else #f)))
          (guard-bool ()
                      (cases guard new
                        (guard-bool () #t)
                        (guard-default () #t)
                        (else #f)))
          (guard-proc ()
                      (cases guard new
                        (guard-proc () #t)
                        (guard-default () #t)
                        (else #f)))
          (else #f))))
  
  (define (ensure-guard-compatible! proc-name existing new var)
    (if (not (guard-compatible? existing new))
        (eopl:error proc-name "guard violation of variable ~s" var)
        'ok))
  
  (define (ensure-val-compatible! proc-name existing val var)
    (if (not (val-compatible? existing val))
        (eopl:error proc-name "guard violation of variable ~s" var)
        'ok))
  
  (define (val-compatible? grd val)
    (if (null? grd)
        #t
        (cases guard grd
          (guard-default () #t)
          (guard-num ()
                     (cases expval val
                       (num-val (n) #t)
                       (else #f)))
          (guard-bool ()
                      (cases expval val
                        (bool-val (b) #t)
                        (else #f)))
          (guard-proc ()
                      (cases expval val
                        (proc-val (p) #t)
                        (else #f)))
          (else #f))))

  )
  


  
