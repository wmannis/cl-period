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
  (ensure-condition range-violation
    (compile-period-string "Hr4-33")))

(addtest (period-tests)
  range-condition-2
  (ensure-condition range-violation
    (in-period-p '(:minute 60))))

(addtest (period-tests)
  range-condition-3
  (ensure-condition range-violation
    (compile-period-string "Hr33")))

(addtest (period-tests)
  arity-condition-1
  (ensure-condition arity-violation
    (in-period-p '(:minute-range 5))))

(addtest (period-tests)
  arity-condition-2
  (ensure-condition arity-violation
    (in-period-p '(:date-range 5 10 15))))

;;; test-period.lisp ends here
