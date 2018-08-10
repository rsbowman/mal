(import :gerbil/gambit/ports
        "reader-meta")

(export trace trace-msg)

(def *trace-indent* 0)

(def *debug* #t)

(def (trace-title . args)
  (when *debug*
    (apply print [(make-string (* 2 *trace-indent*) #\ ) *trace-indent* " " args ...])))

(def (trace-msg . args)
  (when *debug*
    (apply print [(make-string (* 2 *trace-indent*) #\ ) " " args ...])))

(defrules trace ()
  ((_ msg really? body ...)
   (begin
     (when really?
       (set! *trace-indent* (+ *trace-indent* 1))
       (set! *debug* #t)
       (trace-title "BEG " msg " "))
     (let ((ret (begin body ...)))
       (when really?
         (trace-title "END " msg ", return " (mal-repr ret) "\n")
         (set! *trace-indent* (- *trace-indent* 1)))
       (set! *debug* #f)
       ret))))
