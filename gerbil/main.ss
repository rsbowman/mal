#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/sugar
        :std/misc/repr
        "reader-meta"
        "env-meta"
        "core-meta"
        "trace")

(def (mal-eval-list elems env)
  (map (cut mal-eval <> env) elems))

(def (mal-eval-def args env)
  (with ((mal-symbol sym) (car args))
    (let ((e (mal-eval (cadr args) env)))
      (env-set! env sym e)
      e)))

(def (make-env-with-bindings env-parent bindings)
  (let ((env-new (make-env env-parent)))
    (let loop ((bindings bindings))
      (if (null? bindings)
        env-new
        (with ((mal-symbol sym) (car bindings))
          (env-set! env-new sym (mal-eval (cadr bindings) env-new))
          (loop (cdr (cdr bindings))))))))

(def (make-env-with-mal-bindings env-parent bindings)
  (match bindings
    ((mal-vector bs _) (make-env-with-bindings env-parent bs))
    ((mal-list bs _)   (make-env-with-bindings env-parent bs))
    (else (raise "Can't make env w/ non listy bindings"))))

(def (mal-eval-let args env-parent)
  (with ([bindings body] args)
    (let ((env (make-env-with-mal-bindings env-parent bindings)))
      (mal-eval body env))))

(def (mal-eval-do args env)
  (with ([n . rest] args)
    (let ((e (mal-eval n env)))
      (if (null? rest)
        e
        (mal-eval-do rest env)))))

(def (mal-eval-if args env)
  (let ((c (mal-eval (car args) env)))
    (cond
     ((mal-truthy? c) (mal-eval (cadr args) env))
     ((not (null? (cddr args))) (mal-eval (caddr args) env))
     (else mal-nil))))

(def (mal-params->symbols params)
  (map mal-symbol-sym (mal->list params)))

(def (mal-eval-fn params-and-body env)
  (make-mal-function                    ;  XXX or macro?
   (lambda args
     (let* ((names (mal-params->symbols (car params-and-body)))
            (body (cadr params-and-body))
            (env-new (make-env env names args)))
       (mal-eval body env-new)))
   mal-nil))

;; TODO lots of duplication with the list vs. vector...
(def (mal-eval-quasiquote-helper arg env)
  (match arg
    ((mal-list [(mal-symbol 'unquote) x] _)
     (mal-eval x env))
    ((mal-vector [(mal-symbol 'unquote) x] _)
     (mal-eval x env))
    ((mal-list [(mal-list [(mal-symbol 'splice-unquote) x]) . rest] _)
     (core-concat (mal-eval x env)
                  (map (cut mal-eval-quasiquote-helper <> env) rest)))
    ((mal-vector [(mal-list [(mal-symbol 'splice-unquote) x]) . rest] _)
     (core-concat (mal-eval x env)
                  (map (cut mal-eval-quasiquote-helper <> env) rest)))
    ((mal-list [x . xs] _)
     (core-cons (mal-eval-quasiquote-helper x env)
                (mal-eval-quasiquote-helper (make-mal-list xs) env)))
    ((mal-vector [x . xs] _)
     (core-cons (mal-eval-quasiquote-helper x env)
                (mal-eval-quasiquote-helper (make-mal-list xs) env)))
    (o o)))

(def (mal-eval-quasiquote args env)
  (when (and (list? args)
             (not (eq? (length args) 1)))
    (raise "quasiquote takes a single arg"))
  (mal-eval-quasiquote-helper (car args) env))

(def (mal-lookup-head-macro ast env)
  (match ast
    ((mal-list [(mal-symbol sym) . _] _) (if (env-find env sym)
                                           (env-get env sym)
                                           #f))
    (else #f)))

(def (mal-macroexpand ast env)
  (match (mal-lookup-head-macro ast env)
    ((mal-macro f _)
     (mal-macroexpand (apply f (cdr (mal-list-elements ast))) env))
    (else ast)))

(def (mal-eval-defmacro args env)
  (let* ((name (mal-symbol-sym (car args)))
         (body (mal-eval (cadr args) env))
         (mac  (make-mal-macro (mal-function-closure body) mal-nil)))
    (env-set! env name mac)
    mac))

(def (mal-eval-try args env)
  (match args
    ([body (mal-list [(mal-symbol 'catch*) (mal-symbol ex-name) catch-body] _)]
     (try
      (mal-eval body env)
      (catch (ex)
        (mal-eval catch-body (make-env env [ex-name] [ex])))))
    (else (raise "Bad try*/catch* more than one body form?"))))

(def (mal-apply func args)
  (match func
    ((mal-function f _) (apply f args))
    (else  (raise "can't apply non function"))))

(def (mal-eval-special es env)
  (with ([f . args] es)
    (match f
      ((mal-symbol 'def!)          (mal-eval-def args env))
      ((mal-symbol 'defmacro!)     (mal-eval-defmacro args env))
      ((mal-symbol 'let*)          (mal-eval-let args env))
      ((mal-symbol 'do)            (mal-eval-do args env))
      ((mal-symbol 'if)            (mal-eval-if args env))
      ((mal-symbol 'fn*)           (mal-eval-fn args env))
      ((mal-symbol 'quote)         (car args))
      ((mal-symbol 'quasiquote)    (mal-eval-quasiquote args env))
      ((mal-symbol 'macroexpand)   (mal-macroexpand (car args) env))
      ((mal-symbol 'try*)          (mal-eval-try args env))
      ((mal-function f meta)       (mal-apply f (mal-eval-list args env)))
      (else                        (mal-apply (mal-eval f env)
                                     (mal-eval-list args env))))))

(def (mal-eval-special-macro mal-lst env)
  (let ((ast (mal-macroexpand mal-lst env)))
    (match ast
      ((mal-list [] _) ast)
      ((mal-list es _) (mal-eval-special es env))
      (else            (mal-eval ast env)))))

(def (mal-eval o env)
  (match o
    ((mal-symbol sym)       (env-get env sym))
    ((mal-keyword kw)       o)
    ((mal-list [] _)        o)
    ((mal-list es _)        (mal-eval-special-macro o env))
    ((mal-vector es _)      (make-mal-vector (mal-eval-list es env)))
    ((mal-map es _)         (make-mal-map (mal-eval-list es env)))
    (else                   o)))

(def (readline prompt)
  (display prompt)
  (let ((input (read-line)))
    (if (eof-object? input)
        #f
        input)))

(def (mal-mal-eval ast)
  (mal-eval ast repl-env))

(add-core-fn! 'eval mal-mal-eval)
(add-core-fn! 'readline readline)
(env-set! repl-env '*host-language* (make-mal-symbol 'gerbil))

(mal-eval (mal-read "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))") repl-env)
(mal-eval (mal-read "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))") repl-env)

(mal-eval (mal-read "(def! *gensym-counter* (atom 0))") repl-env)
(mal-eval (mal-read "(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))") repl-env)
(mal-eval (mal-read "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))") repl-env)

(def (main-repl)
  (let loop ()
    (let ((input (readline "user> ")))
      (when input
        (try
         (displayln (mal-repr (mal-eval (mal-read input) repl-env)))
         (catch (e)
           (displayln e)))
        (loop)))))

(def (main . args)
  (env-set! repl-env '*ARGV* (make-mal-list (if (null? args)
                                              args
                                              (cdr args))))
  (if (null? args)
    (main-repl)
    (mal-eval (mal-read (string-append "(load-file \"" (car args) "\")")) repl-env)))
