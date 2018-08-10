#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/test
        "reader-meta"
        "env-meta"
        "core-meta")

;; (def (t/put env sym val)
;;   (env-set! env sym val))

;; (def (t/find env sym)
;;   (env-find env sym))

;; (def (t/get env sym)
;;   (env-get env sym))

;; (load "gerbil/step9_try.ss")
(load "gerbil/stepA_mal.ss")

(def env-tests
  (test-suite "env"
    (test-case "set and get"
      (let* ((e-parent (make-env))
             (e-child (make-env e-parent)))
        (env-set! e-child 'a 1)
        (env-set! e-child 'b 2)
        (env-set! e-parent 'c 7)
        (env-set! e-parent 'b 11)
        (check (env-get e-child 'a) => 1)
        (check (env-get e-child 'b) => 2)
        (check (env-get e-parent 'c) => 7)
        (check (env-get e-parent 'b) => 11)
        (check (env-get e-child 'c) => 7)))
    (test-case "env w/ bindings"
      (let ((e (make-env #f ['a 'b] [3 4])))
        (check (env-get e 'a) => 3)
        (check (env-get e 'b) => 4)))
    (test-case "env w/ & rest bindings"
      (let ((e (make-env #f ['a '& 'r] [1 2 3])))
        (check (env-get e 'a) => 1)
        ;; (check (env-get e 'r) => )
        ))
    ))

(def (t/eval mal-input)
  (mal-eval (mal-read mal-input) repl-env))

(def (t/eval-sym mal-input)
  (let ((expr (mal-eval (mal-read mal-input) repl-env)))
    (match expr
      ((mal-symbol s) s)
      (else expr))))

(def eval-tests
  (test-suite "test eval"
    (test-case "let"
      (check (mal-eval (mal-read "(let* (q 9) q)") repl-env) => 9)
      (check (mal-eval (mal-read "(let* (p (+ 2 3) q (+ 2 p)) (+ p q))") repl-env)
             => 12))
    (test-case "keyword"
      (check (mal-repr (t/eval ":a")) => ":a"))
    (test-case "do"
      (check (mal-eval (mal-read "(do 3 4)") repl-env) => 4))
    (test-case "if"
      (check (mal-truthy? (mal-eval (mal-read "false") repl-env)) => #f)
      (check (mal-truthy? (mal-eval (mal-read "true") repl-env)) => #t)
      (check (mal-eval (mal-read "(if true 1 2)") repl-env) => 1)
      (check (mal-eval (mal-read "(if false 1 2)") repl-env) => 2))
    (test-case "="
      (check (mal-eval (mal-read "(= 1 1)") repl-env) => mal-true)
      (check (mal-eval (mal-read "(= 1 2)") repl-env) => mal-false)
      (check (mal-eval (mal-read "(= (list 1 2) [1 2])") repl-env) => mal-true)
      (check (mal-eval (mal-read "(= [(list)] (list []))") repl-env) => mal-true)
      (check (mal-eval (mal-read "(= :abc :abc)") repl-env) => mal-true)
      (check (mal-eval (mal-read "(= (list 1) [])") repl-env) => mal-false))
    (test-case "fn"
      (check (mal-eval (mal-read "( (fn* () 4) )") repl-env) => 4)
      (check (mal-eval (mal-read "((fn* (a) a) 7)") repl-env) => 7))
    ))

(def qq-tests
  (test-suite "quasiquote"
    (check (mal-repr (t/eval "(quasiquote (1 2 3))")) => "(1 2 3)")
    (pp (mal-repr (t/eval "(quasiquote (1 (splice-unquote '(1 2)) 3))")))))

(def core-tests
  (test-suite "core"
    (test-case "print/str/etc"
      (check (t/eval "(str [])") => "[]")
      (check (t/eval "(pr-str \"\")") => "\"\""))
    (test-case "thrwo"
      (let ((t1 "(try* (throw \"my exception\") (catch* exc (do (prn \"exc:\" exc) 7)))"))
        (check (t/eval t1) => 7)))
    (test-case "buch of randos"
      (check (t/eval "(get {:a 3} :a)") => 3)
      (check (t/eval "(contains? {:abc 123} :abc)") => mal-true)
      (t/eval "(def! e (atom {}))")
      (check (mal-repr (t/eval "(swap! e assoc :a 1)")) => "{:a 1}")
      (check (t/eval "(apply str (seq \"this is a test\"))") => "this is a test"))))

;; (trace mal-equal?)
(def misc-tests
  (test-suite "misc"
    (check (t/eval "(let* [or_poop 23] (or false (+ or_poop 100)))") => 123)))

;; NEW UNIT TEST:
;;  (= nil (if false 1))
;;  (= "AB" (str "A" "B"))


(apply run-tests! [;; env-tests eval-tests core-tests qq-tests
                   misc-tests
                   ])
(test-report-summary!)

(case (test-result)
  ((OK) (exit 0))
  (else (exit 1)))
