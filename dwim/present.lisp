;;; -*- Syntax: Common-lisp; Package: DWIM -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :dwim)

;;; I guess this is as good of a place as any to put this.
(clim:define-command-table :global)

;;;
;;; Manipulating presentations
;;;

(progn
  (clim:define-gesture-name :left :pointer-button :left)
  (clim:define-gesture-name :middle :pointer-button :middle)
  (clim:define-gesture-name :right :pointer-button :right))

(defun presentation-p (object)
  (typep object 'clim:presentation))

(defun redisplayable-format (stream string &rest args)
  (if (eq stream 't) (setq stream *standard-output*))
  (if (clim:redisplayable-stream-p stream)
      (let ((a (copy-list args)))
	(with-redisplayable-output (:stream stream
					    :unique-id string
					    :cache-value a
					    :cache-test #'equal)
	  (apply #'format stream string a)))
      (apply #'format stream string args)))

;;;
;;; Presentation parser primitives
;;;

(defun read-char-for-accept (stream)
  (read-char stream nil nil))

(defun unread-char-for-accept (char stream)
  (if (clim:activation-gesture-p char)
      ;; unreading an activation character causes problems for stream-unread-gesture.
      (clim:with-activation-gestures (nil :override t) (unread-char char stream))
      (unread-char char stream)))

(defun peek-char-for-accept (stream &optional hang)
  (let ((ch (and (or hang
		             (not (clim:extended-input-stream-p stream))
		     (< (input-position stream) (insertion-pointer stream)))
		 (read-char-for-accept stream))))
    (when ch
      (unread-char-for-accept ch stream))
    ch))

(defun compare-char-for-accept (char-from-accept comparandum)
  (and char-from-accept
       (typecase char-from-accept
	 (character (char-equal comparandum char-from-accept))
	 (list
	   ;; this should only happen in DW.
	   (and (member (first char-from-accept) '(:activation :blip-character :accept))
		(characterp (second char-from-accept))
		(char-equal comparandum (second char-from-accept)))))))

(defun input-position (stream)
  ;; This location identifies the current position of the parser in a (buffered)
  ;; input stream.  When a character gets read by read-char-for-accept, this pointer
  ;; gets incremented.  Upon failure, the parser backtracks by decrementing it.
  (if (clim:input-editing-stream-p stream)
      (clim:stream-scan-pointer stream)
      (file-position stream)))

(defmethod (setf input-position) (new stream)
  (if (clim:input-editing-stream-p stream)
      (setf (clim:stream-scan-pointer stream) new)
      (file-position stream new)))

(defun insertion-pointer (stream)
  (cond ((clim:extended-input-stream-p stream)
	 (clim:stream-insertion-pointer stream))))

(defvar *%%ready-to-catch%%* nil)

(defmacro catching-parser-failures (form &rest failure-actions)
  "Use this to catch a parser failure and do something about it."
  (let ((normal (gensym)))
    `(let ((*%%ready-to-catch%%* t))
       (catch ',normal
	 (catch 'catch-parser-failures
	   (handler-bind ((error #'(lambda (error)
				     (when (and (typep error 'clim:abort-gesture)
						(find-restart 'abort))
				       (invoke-restart 'abort))
				     (throw 'catch-parser-failures t))))
	     (throw ',normal ,form)))
	 ,@failure-actions))))

;;;
;;; Presentation types
;;;

(defun menu-execute-no-side-effects (item)
  (cond ((atom item) item)
	((atom (cdr item)) (cdr item))
	((atom (cddr item)) (cadr item))
	((eq (second item) :value) (third item))))

(defun token-element-string (element)
  (typecase element
    (null (symbol-name element))		
    (cons (string (first element)))
    (symbol (string-capitalize (symbol-name element)))
    (string element)
    (otherwise (present-to-string element))))

(defun type-for-avv-choice ()
  'clim-internals::accept-values-one-of)

(defun readline-no-echo (stream)
  (clim:with-output-recording-options (stream :draw nil :record nil)
    (accept 'string :stream stream :prompt nil :default nil)))

;;; A hack so the user doesnt have to see some ugly commands get echoed.
;;; Also seems like a useful way to read a password.
(define-presentation-type invisible-object ()
  :parser ((stream)
	   (values (readline-no-echo stream) 'invisible-object))
  :printer ((object stream)
            (declare (ignore object))
	    (write-string "*" stream)))
