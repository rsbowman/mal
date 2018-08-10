(import :std/srfi/1
        :std/pregexp
        :std/misc/repr
        :std/sugar
        :rsb/prelude
        :gerbil/gambit/ports            ; println
        :gerbil/gambit/misc             ; pp
        :gerbil/gambit/exceptions
        )

(export success failure
        make-success make-failure
        mal-tokens
        mal-read mal-read-nonlist mal-read-form mal-read-list
        mal-list make-mal-list mal-list-elements
        mal-vector make-mal-vector mal-vector?
        mal-map make-mal-map mal-map? mal-map-elements
        mal-symbol make-mal-symbol mal-symbol? mal-symbol-sym
        mal-keyword make-mal-keyword mal-keyword?
        mal-repr mal-truthy?
        mal-true mal-false mal-nil
        mal-atom mal-atom? make-mal-atom mal-atom-value mal-atom-value-set!
        mal-macro make-mal-macro mal-macro?
        mal-function mal-function? make-mal-function mal-function-closure)

(defstruct mal-literal (rep))
(defstruct mal-symbol (sym))
(defstruct mal-keyword (kw))
(defstruct mal-list (elements metadata)
  constructor: init!)
(defstruct mal-vector (elements metadata)
  constructor: init!)
(defstruct mal-map (elements metadata)
  constructor: init!)
(defstruct mal-atom (value))

(def mal-nil (make-mal-literal "nil"))
(def mal-true (make-mal-literal "true"))
(def mal-false (make-mal-literal "false"))

(defstruct mal-function (closure metadata))
(defstruct mal-macro (closure metadata))

(defmethod {init! mal-list}
  (lambda (self es (meta mal-nil))
    (set! (mal-list-elements self) es)
    (set! (mal-list-metadata self) meta)))

(defmethod {init! mal-vector}
  (lambda (self es (meta mal-nil))
    (set! (mal-vector-elements self) es)
    (set! (mal-vector-metadata self) meta)))

(defmethod {init! mal-map}
  (lambda (self es (meta mal-nil))
    (set! (mal-map-elements self) es)
    (set! (mal-map-metadata self) meta)))

;; parse success/failure
(defstruct success (parsed rest))
(defstruct failure ())

(def mal-re "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;[^\n]*|[^\\s\\[\\]{}('\"`,;)]*)")

(def (mal-tokens input)
  (let loop ((input input)
             (tokens []))
    (with ([whole-match token] (pregexp-match mal-re input))
      (if (string-empty? whole-match)
        (reverse tokens)
        (loop (substring input
                         (string-length whole-match)
                         (string-length input))
              (if (string-prefix? token ";")
                tokens
                (cons token tokens)))))))

(def (mal-nonlist? s)
  (not (string-prefix? s "(")))

(def (mal-read-quotelike quotelike-mal-obj tokens)
  (with ((success form rest) (mal-read-form (cdr tokens)))
    (make-success (make-mal-list [quotelike-mal-obj form]) rest)))

(def (mal-read-meta tokens)
  (with ((success meta-form rest1) (mal-read-form (cdr tokens)))
    (with ((success data-form rest2) (mal-read-form rest1))
      (make-success (make-mal-list [(make-mal-symbol 'with-meta) data-form meta-form]) rest2))))

(def (mal-read-form tokens)
  (match (car tokens)
    ("("  (mal-read-list tokens "(" ")" make-mal-list))
    ("["  (mal-read-list tokens "[" "]" make-mal-vector))
    ("{"  (mal-read-list tokens "{" "}" make-mal-map))
    ("'"  (mal-read-quotelike (make-mal-symbol 'quote) tokens))
    ("`"  (mal-read-quotelike (make-mal-symbol 'quasiquote) tokens))
    ("~"  (mal-read-quotelike (make-mal-symbol 'unquote) tokens))
    ("~@" (mal-read-quotelike (make-mal-symbol 'splice-unquote) tokens))
    ("@"  (mal-read-quotelike (make-mal-symbol 'deref) tokens))
    ("^"  (mal-read-meta tokens))
    (else (mal-read-nonlist tokens))))

(def (mal-read-list tokens start-tok end-tok list-ctor)
  (assert! (equal? (car tokens) start-tok))
  (let loop ((tokens (cdr tokens))
             (forms []))
    (cond
     ((null? tokens)            (raise (string-append "expected '" end-tok "', got EOF")))
     ((equal? (car tokens)
              end-tok)          (make-success (list-ctor (reverse forms)) (cdr tokens)))
     (else                      (with ((success parsed rest) (mal-read-form tokens))
                                  (loop rest (cons parsed forms)))))))

(def (unescape s)
  (let loop ((inchars (string->list s))
             (outchars []))
    (match inchars
      ([] (list->string (reverse outchars)))
      ([#\\ #\\ . rest] (loop rest (cons #\\ outchars)))
      ([#\\ #\" . rest] (loop rest (cons #\" outchars)))
      ([#\\ #\n . rest] (loop rest (cons #\newline outchars)))
      ([x . rest]       (loop rest (cons x outchars))))))

(def (mal-read-nonlist tokens)
  (let ((token (car tokens)))
    (assert! (mal-nonlist? token))
    (make-success (cond
                   ((string->number token) => identity)
                   ((pregexp-match "^\".*\"$" token)
                    (unescape (substring token 1 (dec (string-length token)))))
                   ((equal? token "nil") mal-nil)
                   ((equal? token "true") mal-true)
                   ((equal? token "false") mal-false)
                   ((string-prefix? token ":")
                    (make-mal-keyword (string->symbol token)))
                   (else
                    (make-mal-symbol (string->symbol token))))
                  (cdr tokens))))

(def (mal-read s)
  (with ((success form rest) (mal-read-form (mal-tokens s)))
    form))

(def (mal-truthy? o)
  (not (or (eq? o mal-nil)
           (eq? o mal-false))))

(def (mal-repr-list ldelim rdelim is-readable elems)
  (let ((elem-reprs (map (cut mal-repr <> readable: is-readable) elems)))
    (string-append ldelim (string-join elem-reprs " ") rdelim)))

(def (mal-escape s)
  (list->string
   (append-map (lambda (c)
                 (case c
                   ((#\\) [#\\ #\\])
                   ((#\") [#\\ #\"])
                   ((#\newline) [#\\ #\n])
                   (else [c])))
               (string->list s))))

(def (mal-repr-str s is-readable)
  (if is-readable
    (string-append "\"" (mal-escape s) "\"")
    s))

(def (mal-repr obj readable: (is-readable #t))
  (match obj
    ((mal-literal rep)  rep)
    ((mal-list es _)    (mal-repr-list "(" ")" is-readable es))
    ((mal-vector es _)  (mal-repr-list "[" "]" is-readable es))
    ((mal-map es _)     (mal-repr-list "{" "}" is-readable es))
    ((mal-symbol sym)   (symbol->string sym))
    ((mal-keyword kw)   (symbol->string kw))
    ((mal-atom v)       (string-append "(atom " (mal-repr v readable: is-readable) ")"))
    ((mal-function f m) "<fn>")
    ((mal-macro f m)    "<macro>")
    ((? number? o)      (number->string o))
    ((? string? s)      (mal-repr-str s is-readable))
    (else               (if (error-exception? obj)
                          (println
                           (error-exception-message obj)
                           "; params "
                           (map mal-repr (error-exception-parameters obj)))
                          (string-append "UNKNOWN (" (repr obj) ")")))))
