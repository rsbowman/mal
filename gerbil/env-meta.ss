(import :std/sugar
        :gerbil/gambit/misc
        :gerbil/gambit/ports            ; println
        "reader-meta")

(export environment make-env env-set! env-find env-get env-print)

(defstruct environment (outer bindings))

(def (make-env (outer #f) (names []) (exprs []))
  (let ((e (make-environment outer (make-hash-table-eq))))
    (let loop ((names names)
               (exprs exprs))
      (cond
       ((null? names)
        e)
       ((eq? (car names) '&)
        (begin
          (env-set! e (cadr names) (make-mal-list exprs))
          e))
       (else
        (begin
          (env-set! e (car names) (car exprs))
          (loop (cdr names) (cdr exprs))))))))

(def (env-set! env sym mal-val)
  (hash-put! (environment-bindings env) sym mal-val))

(def (env-find env sym)
  (cond
   ((hash-key? (environment-bindings env) sym) env)
   ((environment-outer env) (env-find (environment-outer env) sym))
   (else #f)))

(def (env-get env sym)
  (let ((env2 (env-find env sym)))
    (if env2
      (hash-get (environment-bindings env2) sym)
      (raise (string-append "'" (symbol->string sym) "' not found")))))

(def (env-print env (msg "env:"))
  (println msg)
  (hash-for-each (lambda (k v)
                   (println "  " (symbol->string k) " -> " (mal-repr v)))
                 (environment-bindings env))
  ;; (let ((o (environment-outer env)))
  ;;   (when o
  ;;     (env-print o "and...")))
  )
