;;; -*- Syntax: Common-lisp; Package: DWIM-*-
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

(defmacro printing-random-object ((object stream &rest options)
				  &body body)
  `(print-unreadable-object (,object ,stream
			     :type ,(member :typep options)
			     :identity ,(not (member :no-pointer options)))
     ,@body))

(defmacro with-stack-list ((var &rest elements) &body body)
  `(funcall #'(lambda (&rest ,var)
		(declare (dynamic-extent ,var))
		,@body)
	  ,@elements))

(defmacro with-output-truncation ((stream) &body body)
  `(clim:with-end-of-line-action
    (,stream :allow)
    (clim:with-end-of-page-action (,stream :allow) ,@body)))

(defmacro with-redisplayable-output
	  ((&key stream
		 (unique-id nil unique-id-p)
		 (id-test '#'eq)
		 (cache-value nil cache-value-p)
		 (cache-test '#'eql) copy-cache-value)
	   &body body)
  `(if (clim:extended-input-stream-p ,stream)
	      (clim:updating-output
	       (,stream ,@(if unique-id-p `(:unique-id ,unique-id))
			:id-test ,id-test
			,@(if cache-value-p `(:cache-value ,cache-value))
			:cache-test ,cache-test
			:copy-cache-value ,copy-cache-value)
	       ,@body)
	      (progn ,@body)))

(defmacro for-each-frame ((symbol) &body body)
  "Iteratively bind SYMBOL to all enabled frames."
  `(clim:map-over-ports
    #'(lambda (port)
        (unless (eq (clim:port-type port) :postscript)
          (dolist (,symbol (clim:frame-manager-frames
                            (clim:find-frame-manager :port port)))
            (when (member (clim:frame-state ,symbol) '(:shrunk :enabled))
              ,@body))))))
