If you're going to be checking periods very often you really shouldn't use the string form directly in **`in-period-p`**, but should compile the period.  On an aged Sun Blade running SBCL:

```
PERIOD> (setf bob (compile-period-string "Monday-Friday.Hr9->17"))
(AND (:DAY-OF-WEEK-RANGE :MONDAY :FRIDAY) (:HOUR-RANGE 9 16))
PERIOD> (time (dotimes (i 5000)
                (declare (fixnum i))
                (in-period-p bob)))
Evaluation took:
  0.192 seconds of real time
  0.170147 seconds of user run time
  0.007827 seconds of system run time
  0 page faults and
  760,072 bytes consed.
NIL
PERIOD> (time (dotimes (i 5000)
                (declare (fixnum i))
                (in-period-p "Monday-Friday.Hr9->17")))
Evaluation took:
  3.554 seconds of real time
  3.202775 seconds of user run time
  0.225485 seconds of system run time
  0 page faults and
  11,960,256 bytes consed.
NIL
PERIOD> 
```