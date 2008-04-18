;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-period; Lowercase: Yes -*-
;;; Version: $Id$
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

(defun compile-period-string (s)
  (yacc:parse-with-lexer
   (token-list-lexer (tokenize-period s))
   *period-class-parser*))


(defgeneric in-period-p (period &optional time))

(defmethod in-period-p ((period list) &optional (time (get-universal-time)))
  (eval-period period time))

(defmethod in-period-p ((period string) &optional (time (get-universal-time)))
  (eval-period (compile-period-string period) time))


(defgeneric define-period-class (name period))

(defmethod define-period-class ((name string) (period string))
  (add-period-class (string->keyword name) (compile-period-string period)))

(defmethod define-period-class ((name string) (period list))
  (add-period-class (string->keyword name) period))

;;; ADD-PERIOD-CLASS checks that NAME is a keyword symbol.
(defmethod define-period-class ((name symbol) (period string))
  (add-period-class name (compile-period-string period)))

(defmethod define-period-class ((name symbol) (period list))
  (add-period-class name period))

;;; api.lisp ends here
