;;; -*- mode: lisp; syntax: common-lisp; package: cl-period; encoding: utf-8 -*-
;;; $Id$
;;;
;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

(in-package :cl-period)

(defparameter +days-of-week+
 '(:monday 0 :tuesday 1 :wednesday 2 :thursday 3 :friday 4
   :saturday 5 :sunday 6))

(defparameter +months+
 '(:january 1 :february 2 :march 3 :april 4 :may 5 :june 6 :july 7
   :august 8 :september 9 :october 10 :november 11 :december 12))

(defvar *period-classes* (make-hash-table)
  "holds user-defined period classes")


(define-condition contract-violation (error)
  ((message :initarg :message :accessor contract-violation-message))
  (:report (lambda (condition stream)
             (format stream (contract-violation-message condition)))))

(define-condition arity-violation (contract-violation)
  ())

(define-condition range-violation (contract-violation)
  ())

(define-condition syntax-violation (contract-violation)
  ())

(defun arity-error (&rest format-args)
  (error 'arity-violation :message (apply #'format (cons nil format-args))))

(defun range-error (&rest format-args)
  (error 'range-violation :message (apply #'format (cons nil format-args))))

(defun syntax-error (&rest format-args)
  (error 'syntax-violation :message (apply #'format (cons nil format-args))))

(define-condition gibberish-cycling-year-range (warning)
  ((range :initarg :range :accessor gibberish-range))
  (:report (lambda (condition stream)
             (format stream "cycling year range makes little sense: ~A" 
                            (gibberish-range condition)))))


;;; Blunt, but useful.
(defmacro with-decoded-time (time &body body)
 `(multiple-value-bind
   (second minute hour date month year day-of-week daylight-p zone)
   (decode-universal-time ,time)
   (declare (ignorable second minute hour date month year day-of-week daylight-p zone))
     ,@body))

(defgeneric sanity-check-args (op args)
 (:documentation
  "ensure correct number and type of arguments for a time specifier"))

(defmacro make-sanity-checker (op &body range-test)
 `(defmethod sanity-check-args ((op (eql ,op)) args)
    (unless (= (length args) 1)
      (arity-error "~A takes one argument: ~{~A~^, ~}" op args))
    (unless (apply #',@range-test args)
      (range-error "~A not meaningful in ~A clause" args op))))

(make-sanity-checker :second (lambda (n) (<= 0 n 59)))
(make-sanity-checker :minute (lambda (n) (<= 0 n 59)))
(make-sanity-checker :hour (lambda (n) (<= 0 n 23)))
(make-sanity-checker :date (lambda (n) (<= 1 n 32)))
(make-sanity-checker :year (lambda (n) (declare (ignore n)) t))
(make-sanity-checker :month (lambda (n) (getf +months+ n)))
(make-sanity-checker :day-of-week (lambda (n) (getf +days-of-week+ n)))
(make-sanity-checker 'not (lambda (n) (declare (ignore n)) t))
(make-sanity-checker :class (lambda (n) (gethash n *period-classes*)))

(defun get-unary-arg (op applicand)
 "ensure the sanity of a unary op argument and extract the value"
 (sanity-check-args op applicand)
 (first applicand))

;;; This assumes that START and END have already been sanity-checked.
(defun range-test (p start end)
 (if (> start end)
     (or (>= p start)
         (<= p end))
     (<= start p end)))

(defgeneric in-range-p (op val args)
 (:documentation "sanity check and test period range"))

(defmethod in-range-p ((op symbol) val args)
 (unless (= (length args) 2)
   (arity-error "~A range requires two arguments: ~A" op args))
 (let ((start (first args))
       (end (second args)))
   (sanity-check-args op (list start))
   (sanity-check-args op (list end))
   (range-test val start end)))

(defmethod in-range-p ((op (eql :year)) val args)
 (unless (= (length args) 2)
   (arity-error "~A range requires two arguments: ~A" op args))
 (let ((start (first args))
       (end (second args)))
   (sanity-check-args op (list start))
   (sanity-check-args op (list end))
   (when (> start end)
     (warn 'gibberish-cycling-year-range :range args))
   (range-test val start end)))

(defmethod in-range-p ((op (eql :month)) val args)
 (unless (= (length args) 2)
   (arity-error "~A range requires two arguments: ~A" op args))
 (let ((start (first args))
       (end (second args)))
   (sanity-check-args op (list start))
   (sanity-check-args op (list end))
   (range-test val (getf +months+ start) (getf +months+ end))))

(defmethod in-range-p ((op (eql :day-of-week)) val args)
 (unless (= (length args) 2)
   (arity-error "~A range requires two arguments: ~A" op args))
 (let ((start (first args))
       (end (second args)))
   (sanity-check-args op (list start))
   (sanity-check-args op (list end))
   (range-test val (getf +days-of-week+ start) (getf +days-of-week+ end))))


(defun eval-period (period time)
  (with-decoded-time time
    (labels 
        ((evaluate (period)
           (let ((op (car period))
                 (args (cdr period)))
             (case op
               (and
                (dolist (subperiod args t)
                  (unless (evaluate subperiod)
                    (return nil))))
               (or
                (dolist (subperiod args nil)
                  (when (evaluate subperiod)
                    (return t))))
               (not (not (evaluate (get-unary-arg op args))))
               (:second (= second (get-unary-arg op args)))
               (:minute (= minute (get-unary-arg op args)))
               (:hour (= hour (get-unary-arg op args)))
               (:date (= date (get-unary-arg op args)))
               (:month (= month (getf +months+ (get-unary-arg op args))))
               (:year (= year (get-unary-arg op args)))
               (:day-of-week
                (= day-of-week (getf +days-of-week+ (get-unary-arg op args))))
               (:second-range (in-range-p :second second args))
               (:minute-range (in-range-p :minute minute args))
               (:hour-range (in-range-p :hour hour args))
               (:date-range (in-range-p :date date args))
               (:month-range (in-range-p :month month args))
               (:year-range (in-range-p :year year args))
               (:day-of-week-range (in-range-p :day-of-week day-of-week args))
               (:class (progn 
                         (sanity-check-args :class args)
                         (evaluate (gethash (first args) *period-classes*))))
               (otherwise (syntax-error "unknown period specifier: ~A" op))))))
      (evaluate period))))


;;; Customized period tokens (called classes in honor of the inspiration
;;; for the notation, cfengine).
(defun add-period-class (class period)
  (if (and (keywordp class) (not (find #\- (symbol-name class))))
      (setf (gethash class *period-classes*) period)
      (syntax-error "period class names must be keywords without dashes: ~A" class)))


;;;
;;; Next, period strings based on cfengine's time classes.
;;;
(defun tokenize-period (string)
  (let ((token (make-array 0 :adjustable t :fill-pointer 0))
        (tokenized ()))
    (labels ((op->tok (op)
               (case op
                 (#\. :and)
                 (#\| :or)
                 ((#\! #\~) :not)
                 (#\( :oparen)
                 (#\) :cparen)))
             (push-current-token-with-op (op)
               (unless (= 0 (length token))
                 (push (coerce token 'string) tokenized)
                 (setf token (make-array 0 :adjustable t :fill-pointer 0)))
               (when op
                 (push (op->tok op) tokenized))))
      (loop for c across (string-upcase string)
         if (member c '(#\. #\| #\( #\) #\! #\~) :test #'char=)
           do (push-current-token-with-op c)
         else
           do (vector-push-extend c token)
         finally
           (push-current-token-with-op nil))
      (nreverse tokenized))))

(defun string->keyword (s)
  (intern (string-upcase s) "KEYWORD"))

(eval-when (compile load eval)          ; for yacc:define-parser
  (defun or-expr (a b c)
    (declare (ignore b))
    (labels ((or-clause-p (c)
               (and (listp c)
                    (eql 'or (first c)))))
      (if (or-clause-p a)
          (append a (list c))
          (list 'or a c))))
  
  (defun and-expr (a b c)
    (declare (ignore b))
    (labels ((and-clause-p (c)
               (and (listp c)
                    (eql 'and (first c)))))
      (if (and-clause-p a)
          (append a (list c))
          (list 'and a c))))
  
  (defun group-expr (a b c)
    (declare (ignore a c))
    b)
  
  (defun not-expr (a b)
    (declare (ignore a))
    (list 'not b))
)

(defun make-exclusive-range (op end)
  "Adjust an exclusive range into the default inclusive range notation."
  (labels ((rollover-range (n max)
             (if (= n 0) max (1- n))))
    (case op
      ((:second :minute) (rollover-range end 59))
      (:hour (rollover-range end 23))
      (:date (if (= end 1) 32 (1- end)))
      (:day-of-week (case end
                      (:monday :sunday) (:tuesday :monday)
                      (:wednesday :tuesday) (:thursday :wednesday)
                      (:friday :thursday) (:saturday :friday)
                      (:sunday :saturday)))
      (:month (case end
                (:january :december) (:february :january) (:march :february)
                (:april :march) (:may :april) (:june :may) (:july :june)
                (:august :july) (:september :august) (:october :september)
                (:november :october) (:december :november)))
      (:year (1- end)))))

(defun numeric-period-p (token)
  "Determine if a period token is a numeric one (Hr, Day, etc), returning
multiple values, a boolean and one of HR, YR, SEC, MIN, DAY."
  (cond ((string= token "HR" :end1 2) (values t "HR"))
        ((string= token "YR" :end1 2) (values t "YR"))
        ((string= token "SEC" :end1 3) (values t "SEC"))
        ((string= token "MIN" :end1 3) (values t "MIN"))
        ((string= token "DAY" :end1 3) (values t "DAY"))
        (t (values nil nil))))

(defun period-string->keyword (s)
  (cond 
    ((string= s "SEC") :second)
    ((string= s "MIN") :minute)
    ((string= s "HR") :hour)
    ((string= s "DAY") :date)
    ((string= s "YR") :year)
    (t (error "unknown period string ~A" s))))

;;; This seems like more faffing about than should be necessary.  However,
;;; it has to cope with numeric ranges (Hr4-9) as well as named ranges
;;; for months and days of the week.  The addition of an exclusive range,
;;; notation (Hr9->17) adds to the mess.
(defun parse-range-expr (expr)
  (multiple-value-bind (numeric-range-p range-string) (numeric-period-p expr)
    (let* ((dash-idx (search "-" expr))
           (exclusive-range-p (char= #\> (char expr (1+ dash-idx))))
           (start1 (if numeric-range-p
                       (length range-string)
                       0))
           (end1 dash-idx)
           (start2 (if exclusive-range-p
                       (+ 2 dash-idx)
                       (+ 1 dash-idx))))
      (labels ((get-range ()
                 (list
                  (subseq expr start1 end1)
                  (subseq expr start2)))
               (get-range-numbers ()
                 (mapcar #'parse-integer (get-range)))
               (rangeify-name (kw)
                 (string->keyword
                  (concatenate 'string (symbol-name kw) "-RANGE")))
               (make-numeric-range (op range)
                 (let ((start (first range))
                       (end (second range)))
                   (sanity-check-args op (list start))
                   (sanity-check-args op (list end))
                   (if exclusive-range-p
                       (list (rangeify-name op)
                             start
                             (make-exclusive-range op end))
                       (list (rangeify-name op) start end))))
               (make-named-range (range)
                 (let* ((start (string->keyword (first range)))
                        (end (string->keyword (second range))))
                   (cond ((and (getf +months+ start) (getf +months+ end))
                          (if exclusive-range-p
                              (list :month-range start (make-exclusive-range :month end))
                              (list :month-range start end)))
                         ((and (getf +days-of-week+ start)
                               (getf +days-of-week+ end))
                          (if exclusive-range-p
                              (list :day-of-week-range start (make-exclusive-range :days-of-week end))
                              (list :day-of-week-range start end)))
                         (t (syntax-error "indecipherable range: ~A" range))))))
        (if numeric-range-p
            (make-numeric-range (period-string->keyword range-string)
                                (get-range-numbers))
            (make-named-range (get-range)))))))

(eval-when (compile load eval)          ; for yacc:define-parser
  (defun period-expr (e)
    (let ((k (string->keyword e)))
      (if (find #\- e)
          ;; ranges
          (parse-range-expr e)
          ;; simple single periods, checked for sanity
          (labels ((check-and-return (op n)
                     (sanity-check-args op (list n))
                     n))
            (cond ((getf +days-of-week+ k) (list :day-of-week k))
                  ((getf +months+ k) (list :month k))
                  ((string= e "HR" :end1 2)
                   (list :hour (check-and-return
                                :hour (parse-integer e :start 2))))
                  ((string= e "YR" :end1 2)
                   (list :year (check-and-return
                                :year (parse-integer e :start 2))))
                  ((string= e "MIN" :end1 3)
                   (list :minute (check-and-return
                                  :minute (parse-integer e :start 3))))
                  ((string= e "SEC" :end1 3)
                   (list :second (check-and-return
                                  :second (parse-integer e :start 3))))
                  ((string= e "DAY" :end1 3)
                   (list :date (check-and-return
                                :date (parse-integer e :start 3))))
                  (t (list :class k)))))))
)

(defun token-list-lexer (list)
  #'(lambda ()
      (let ((value (pop list)))
        (if (null value)
            (values nil nil)
            (let ((term
                   (cond ((member value '(:and :or :not :period :oparen :cparen)) value)
                         ((stringp value) :period)
                         (t (syntax-error "ghastly error in token stream - ~S - please report this as a bug" value)))))
              (values term value))))))

;;; If any of the functions referred to below (#'or-expr, etc) is changed
;;; during a development session this form has to be evaluated, too.  It
;;; keeps hold of the old definitions since it's not keeping their names
;;; but their function values.
(yacc:define-parser *period-class-parser*
  (:start-symbol expr)
  (:terminals (:and :or :tok :not :period :oparen :cparen))
  (:precedence ((:right :not) (:left :and) (:left :or)))

  (expr
   (expr :or expr  #'or-expr)
   (expr :and expr #'and-expr)
   (:not expr      #'not-expr)
   term)

  (term
   (:period #'period-expr)
   (:oparen expr :cparen #'group-expr)))


;;; period.lisp ends here