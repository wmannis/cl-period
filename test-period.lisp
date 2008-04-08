;;; $Id$
;;;
;;; For now this is just for testing during development, and isn't
;;; loadable via ASDF trickery.

(asdf:oos 'asdf:load-op :cl-period)
(asdf:oos 'asdf:load-op :lift)
(use-package :cl-period)
(use-package :lift)


(deftestsuite period-tests () ())

(addtest (period-tests)
  range-condition-1
  (ensure-condition period-range-violation
    (compile-period-string "Hr4-33")))

(addtest (period-tests)
  range-condition-2
  (ensure-condition period-range-violation
    (in-period-p '(:minute 60))))

(addtest (period-tests)
  range-condition-3
  (ensure-condition period-range-violation
    (compile-period-string "Hr33")))

(addtest (period-tests)
  range-condition-4
  (ensure-condition period-range-violation
    (compile-period-string "Sec93")))

(addtest (period-tests)
  range-condition-5
  (ensure-condition period-range-violation
    (compile-period-string "Day43")))

(addtest (period-tests)
  arity-condition-1
  (ensure-condition period-arity-violation
    (in-period-p '(:minute-range 5))))

(addtest (period-tests)
  arity-condition-2
  (ensure-condition period-arity-violation
    (in-period-p '(:date-range 5 10 15))))

(addtest (period-tests)
  syntax-condition-1
  (ensure-condition period-syntax-violation
    (compile-period-string "Day40-")))

(addtest (period-tests)
  syntax-condition-2
  (ensure-condition period-syntax-violation
    (compile-period-string "Hr-4")))

(addtest (period-tests)
  syntax-condition-3
  (ensure-condition period-syntax-violation
    (compile-period-string "Min30->")))

(addtest (period-tests)
  syntax-condition-4
  (ensure-condition period-syntax-violation
    (compile-period-string "Sec30->")))

(addtest (period-tests)
  year-range-warning
  (ensure-warning 'gibberish-cycling-year-range
    (in-period-p '(:year-range 2000 1995))))

(addtest (period-tests)
  compiler-1
  (ensure-same '(:hour-range 1 2)
               (compile-period-string "Hr1-2")
               :test #'equal))

(addtest (period-tests)
  compiler-2
  (ensure-same '(and (:hour-range 1 2) (:minute 30))
               (compile-period-string "Hr1-2.Min30")
               :test #'equal))

(addtest (period-tests)
  compiler-3
  (ensure-same '(or (and (:hour-range 1 2) (:minute 30)) (:minute 45))
               (compile-period-string "Hr1-2.Min30|Min45")
               :test #'equal))

(addtest (period-tests)
  compiler-4
  (ensure-same '(and (:hour-range 1 2) (or (:minute 30) (:minute 45)))
               (compile-period-string "Hr1-2.(Min30|Min45)")
               :test #'equal))

(addtest (period-tests)
  compiler-5
  (ensure-same (compile-period-string "~Wednesday")
               (compile-period-string "!Wednesday")
               :test #'equal))

(addtest (period-tests)
  compiler-6
  (ensure-same '(and (:hour 2) (:day-of-week :wednesday) (:month :april))
               (compile-period-string "Hr2.Wednesday.April")
               :test #'equal))

(addtest (period-tests)
  compiler-7
  (ensure-same '(:day-of-week-range :friday :tuesday)
               (compile-period-string "Friday-Tuesday")
               :test #'equal))

(addtest (period-tests)
  compiler-8
  (ensure-same '(:hour-range 17 23)
               (compile-period-string "Hr17->0")
               :test #'equal))

(addtest (period-tests)
  compiler-9
  (ensure-same '(:hour 17)
               (compile-period-string "Hr17")
               :test #'equal))

(addtest (period-tests)
  compiler-10
  (ensure-same '(:day-of-week :tuesday)
               (compile-period-string "Tuesday")
               :test #'equal))

;;; test-period.lisp ends here
