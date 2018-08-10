(import :std/srfi/1
        :rsb/prelude
        :std/misc/ports
        :gerbil/gambit/ports
        :gerbil/gambit/misc
        :gerbil/gambit/os               ; time->seconds
        :std/misc/list                  ; alist->plist
        "reader-meta"
        "env-meta"
        "trace")

(export mal->list repl-env core-cons core-concat mal-equal? add-core-fn!)

(def (mal->list x)
  (match x
    ((mal-list es _) es)
    ((mal-vector es _) es)
    (else x)))

(def repl-env (make-env))

(env-set! repl-env 'nil mal-nil)

(def (add-core-fn! sym func)
  (env-set! repl-env sym (make-mal-function func mal-nil)))

(add-core-fn! '+ (lambda (x y) (+ x y)))
(add-core-fn! '- (lambda (x y) (- x y)))
(add-core-fn! '* (lambda (x y) (* x y)))
(add-core-fn! '/ (lambda (x y) (/ x y)))

(def (core-equal-mal-bool? x y)
  (mal-bool (mal-equal? x y)))

;; Ok listen, I apologise for inflicting this upon the world.  This is for fun.  Don't look too closely.
;; I would not use alists if I did this again, let's make that clear.
(def (mal-map->list es)
  (match es
    ((mal-map ees _) (mal-map->list ees))
    ([k v . rest] (cons [k . v] (mal-map->list rest)))
    ([] [])
    (else (raise "Non list in mal->list"))))

(def (mal-alist-contains-kv? akv ls)
  (match ls
    ([kv . rest] (or (and (mal-equal? (car akv) (car kv))
                          (mal-equal? (cdr akv) (cdr kv)))
                          (mal-alist-contains-kv? akv rest)))
    ([] #f)
    (else (raise "Non list in mal-alist-contains-kv?"))))

(def (mal-alist-contains? ak ls)
  (match ls
    ([kv . rest] (or (mal-equal? ak (car kv))
                     (mal-alist-contains? ak rest)))
    ([] #f)
    (else (raise "Non list in mal-alist-contains?"))))

(def (mal-alist-get ak ls)
  (match ls
    ([[k . v] . rest] (if (mal-equal? ak k)
                        v
                        (mal-alist-get ak rest)))
    ([] mal-nil)
    (else (raise "Non list in mal-alist-get"))))

(def (mal-alist-subset? lx ly)
  (match lx
    ([kv . rest] (and (mal-alist-contains-kv? kv ly)
                           (mal-alist-subset? rest ly)))
    ([] #t)
    (else (raise "Non list in mal-alist-subset?"))))

(def (mal-alist-dissoc1 d l)
  (match l
    ([[k . v] . rest] (if (mal-equal? d k)
                        (mal-alist-dissoc1 d rest)
                        (cons [k . v] (mal-alist-dissoc1 d rest))))
    ([] [])
    (else (raise "Non list in mal-alist-dissoc1"))))

(def (mal-alist-dissoc ds l)
  (match ds
    ([] l)
    ([k . ks] (mal-alist-dissoc1 k (mal-alist-dissoc ks l)))
    (else (raise "Non list in mal-alist-dissoc"))))

(def (mal-map-equal? x y)
  (let ((lx (mal-map->list x))
        (ly (mal-map->list y)))
    (and (mal-alist-subset? lx ly)
         (mal-alist-subset? ly lx))))

(def (mal-equal? x y)
  (match* ((mal->list x) (mal->list y))
    (((mal-symbol a) (mal-symbol b))   (eq? a b))
    (((mal-keyword a) (mal-keyword b)) (eq? a b))
    (((mal-map x _) (mal-map y _))     (mal-map-equal? x y))
    (((? list? as) (? list? bs))       (and (= (length as) (length bs))
                                            (andmap mal-equal? as bs)))
    ((a b)                             (equal? a b))))

(add-core-fn! '= core-equal-mal-bool?)
(add-core-fn! 'list (lambda args (make-mal-list args)))

(def (core-count mal-l)
  (let ((l (mal->list mal-l)))
    (if (list? l)
      (length l)
      0)))

(add-core-fn! 'count core-count)

(def (mal-bool b)
  (if b mal-true mal-false))

(add-core-fn! 'not (lambda (x) (mal-bool (not (mal-truthy? x)))))
(add-core-fn! 'empty? (lambda (x) (mal-bool (null? (mal->list x)))))
(add-core-fn! 'list? (lambda (x) (match x
                                   ((mal-list _ _) mal-true)
                                   (else mal-false))))

(add-core-fn! '< (lambda (x y) (mal-bool (< x y))))
(add-core-fn! '> (lambda (x y) (mal-bool (> x y))))
(add-core-fn! '<= (lambda (x y) (mal-bool (<= x y))))
(add-core-fn! '>= (lambda (x y) (mal-bool (>= x y))))


(def (core-str . args)
  (string-join (map (cut mal-repr <> readable: #f) args) ""))

(def (core-pr-str . args)
  (string-join (map (cut mal-repr <> readable: #t) args) " "))

(def (core-prn . args)
  (begin
    (displayln (apply core-pr-str args))
    mal-nil))

(def (core-println . args)
  (begin
    (displayln (string-join (map (cut mal-repr <> readable: #f) args) " "))
    mal-nil))

(add-core-fn! 'str core-str)
(add-core-fn! 'pr-str core-pr-str)
(add-core-fn! 'prn core-prn)
(add-core-fn! 'println core-println)

(add-core-fn! 'read-string mal-read)

(def (core-slurp filename)
  (read-file-string filename))
(add-core-fn! 'slurp core-slurp)

(def (core-atom value)
  (make-mal-atom value))

(def (core-atom? o)
  (mal-bool (mal-atom? o)))

(def (core-deref a)
  (mal-atom-value a))

(def (core-reset! a v)
  (begin
    (mal-atom-value-set! a v)
    v))

(def (core-swap! a f . args)
  (core-reset! a (apply (mal-function-closure f) (mal-atom-value a) args)))

(add-core-fn! 'atom core-atom)
(add-core-fn! 'atom? core-atom?)
(add-core-fn! 'deref core-deref)
(add-core-fn! 'reset! core-reset!)
(add-core-fn! 'swap! core-swap!)

(def (core-cons v l)
  (make-mal-list [v (mal->list l)...]))
(add-core-fn! 'cons core-cons)

(def (core-concat . args)
  (make-mal-list (concatenate (map mal->list args))))
(add-core-fn! 'concat core-concat)

(def (core-first lst)
  (match lst
    ((mal-list [x . xs] _)   x)
    ((mal-vector [x . xs] _) x)
    (else                    mal-nil)))

(add-core-fn! 'first core-first)

(def (core-rest lst)
  (match lst
    ((mal-list [x . xs] _)    (make-mal-list xs))
    ((mal-vector [x . xs] _)  (make-mal-list xs))
    ((mal-list [] _)          (make-mal-list []))
    ((mal-vector [] _)        (make-mal-list []))
    (mal-nil                  (make-mal-list []))
    (else                     mal-nil)))

(add-core-fn! 'rest core-rest)

(def (core-nth lst n)
  (match lst
    ((mal-list es _)   (list-ref es n))
    ((mal-vector es _) (list-ref es n))))
(add-core-fn! 'nth core-nth)

(def (core-throw val)
  (raise val))
(add-core-fn! 'throw core-throw)

(def (core-map f lst)
  (make-mal-list (map (mal-function-closure f) (mal->list lst))))
(add-core-fn! 'map core-map)

(add-core-fn! 'hash-map (lambda args (make-mal-map args)))
(add-core-fn! 'vector (lambda args (make-mal-vector args)))

(def (core-dissoc m . ks)
  (make-mal-map (alist->plist (mal-alist-dissoc ks (mal-map->list m)))))
(add-core-fn! 'dissoc core-dissoc)

(add-core-fn! 'symbol? (comp mal-bool mal-symbol?))
(add-core-fn! 'keyword? (comp mal-bool mal-keyword?))
(add-core-fn! 'vector? (comp mal-bool mal-vector?))
(add-core-fn! 'true?   (lambda (s) (mal-bool (eq? s mal-true))))
(add-core-fn! 'false?   (lambda (s) (mal-bool (eq? s mal-false))))
(add-core-fn! 'nil?   (lambda (s) (mal-bool (eq? s mal-nil))))

(def (apply-args args)
  (let loop ((args args)
             (out []))
    (match args
      ([l] (reverse (append (reverse (mal->list l)) out)))
      ([x . rest] (loop rest (cons x out)))
      (else (raise "Got non listy args in apply")))))

(def (core-apply f . args)
  (apply (mal-function-closure f) (apply-args args)))

(add-core-fn! 'apply core-apply)

(add-core-fn! 'symbol (lambda (s) (make-mal-symbol (if (symbol? s) s (string->symbol s)))))
(add-core-fn! 'keyword (lambda (s) (make-mal-keyword (string->symbol (string-append ":" s)))))
(def (core-keys m)
  (make-mal-list (map car (mal-map->list m))))
(add-core-fn! 'keys core-keys)
(def (core-vals m)
  (make-mal-list (map cdr (mal-map->list m))))
(add-core-fn! 'vals core-vals)

;; NOTE cheap hack here, put the new kvs first so new keys are found first in lookup
(def (core-assoc m . kvs)
  (make-mal-map [kvs ... (mal-map-elements m) ...]))
(add-core-fn! 'assoc core-assoc)

(def (core-get m k)
  (if (eq? m mal-nil)
    mal-nil
    (mal-alist-get k (mal-map->list m))))

(add-core-fn! 'get core-get)

(def (core-contains m k)
  (if (eq? m mal-nil)
    mal-nil
    (mal-bool (mal-alist-contains? k (mal-map->list m)))))

(add-core-fn! 'contains? core-contains)

(add-core-fn! 'map? (comp mal-bool mal-map?))

(def (core-sequential v)
  (match v
    ((mal-list es _) #t)
    ((mal-vector es _) #t)
    (else #f)))
(add-core-fn! 'sequential? (comp mal-bool core-sequential))

(def (core-meta f)
  (match f
    ((mal-function _ m) m)
    ((mal-macro _ m) m)
    ((mal-list _ m) m)
    ((mal-vector _ m) m)
    ((mal-map _ m) m)
    (else mal-nil)))

(add-core-fn! 'meta core-meta)

(def (core-with-meta mal-val v)
  (match mal-val
    ((mal-function f _) (make-mal-function f v))
    ((mal-macro f _)    (make-mal-macro f v))
    ((mal-vector es _)  (make-mal-vector es v))
    ((mal-list es _)    (make-mal-list es v))
    ((mal-map es _)     (make-mal-map es v))
    (else mal-val)))

(add-core-fn! 'with-meta core-with-meta)

(def (core-seq s)
  (match s
    ((mal-list []) mal-nil)
    ((mal-vector []) mal-nil)
    ((mal-vector es) (make-mal-list es))
    ((? string? s)  (if (equal? s "")
                       mal-nil
                       (make-mal-list (map string (string->list s)))))

    (else s)))
(add-core-fn! 'seq core-seq)

(def (core-conj s . args)
  (match s
    ((mal-list es) (make-mal-list (append (reverse args) es)))
    ((mal-vector es) (make-mal-vector (append es args)))
    (else (raise "Non list or vector arg to conj"))))
(add-core-fn! 'conj core-conj)

(def (core-macro f)
  (match f
    ((mal-macro _ _) mal-true)
    (else mal-false)))
(add-core-fn! 'macro? core-macro)

(def (core-fn f)
  (match f
    ((mal-function _ _) mal-true)
    (else mal-false)))
(add-core-fn! 'fn? core-fn)

(add-core-fn! 'number? (comp mal-bool number?))
(add-core-fn! 'string? (comp mal-bool string?))

(add-core-fn! 'time-ms (lambda () (* 1000 (time->seconds (current-time)))))
(add-core-fn! 'start-time (* 1000 (time->seconds (current-time))))

(let ((counter 0))
  (def (core-gensym)
    (set! counter (1+ counter))
    (string-append "_sym_" (number->string counter)))
  (add-core-fn! 'gensym core-gensym))
