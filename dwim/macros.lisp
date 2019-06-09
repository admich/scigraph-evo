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

(defun canonicalize-argument-list (list)
  (remove '&key list))

(defun canonicalize-command-table (command-table)
  (if (symbolp command-table)
      command-table
      (eval command-table)))

(defun canonicalize-documentation (documentation)
  documentation)


(defmacro define-presentation-to-command-translator
    (name
     (presentation-type
      &key (menu t) gesture documentation (pointer-documentation documentation)
	   command-name tester command-table)
     arguments
     &body body)
  (setq documentation (canonicalize-documentation documentation)
	command-table (canonicalize-command-table command-table))
  `(clim:define-presentation-to-command-translator
    ,name
    (,presentation-type ,command-name ,command-table
     :tester ,(if tester
                  (cons (canonicalize-argument-list
                         (car tester))
                        (cdr tester)))
     :gesture ,gesture
     :menu ,menu
     :pointer-documentation ,pointer-documentation
     :documentation ,documentation)
    ;; Don't know who's right, but my reading of the spec suggests
    ;; that &key shouldn't be in the argument list. -- moore
    ,(canonicalize-argument-list arguments)
    ,@body))

(defmacro define-presentation-type (name arglist
				    &key 
				    parser printer abbreviation-for (no-deftype t)
				    typep describer description accept-values-displayer
				    highlighter
				    INHERIT-FROM)
  (if (null no-deftype)
      (format t "Presentation type ~A, :NO-DEFTYPE option is obsolete and is ignored."
	      name))
  (if (and (consp arglist) (consp (car arglist)))
      (error "Obsolete arglist.  Use (&REST ARGS) rather than ((&REST ARGS))"))
  `(progn
    ;; Methods automatically get lexical access to the presentation arguments.
    ;; TO DO: handle the keywords from :parser and :printer arglists.
    ,(let ((superclasses (cond (INHERIT-FROM
                                (list (eval INHERIT-FROM)))
                               ((find-class name nil)
                                (mapcar #'class-name
                                        (#+mcclim
                                         c2mop:class-direct-superclasses
                                         #-mcclim
                                         class-direct-superclasses
                                         (find-class name)))))))
       (if superclasses
           (setq superclasses
                 (if (cdr superclasses)
                     (cons 'and superclasses)
                     (car superclasses))))
       (if abbreviation-for
           `(clim:define-presentation-type-abbreviation ,name ,arglist ,abbreviation-for)
           `(clim:define-presentation-type ,name ,arglist
             :description ,description
             :inherit-from ,(and superclasses `',superclasses))))
    ,(when parser
       (let ((args (canonicalize-argument-list (first parser)))
             (body (rest parser)))
         `(clim:define-presentation-method
           clim:accept
           ((type ,name) stream (view clim:textual-view) &key)
           (let ((,(first args) stream))
             ,@body))))
    ,(when describer
       (let ((args (canonicalize-argument-list (first describer)))
             (body (rest describer)))
         `(clim:define-presentation-method
           clim:describe-presentation-type
           ((type ,name) stream plural-count)
           (let ((,(first args) stream))
             plural-count
             ,@body))))
    ,(when typep
       (let ((args (canonicalize-argument-list (first typep)))
             (body (rest typep)))
         `(clim:define-presentation-method
           clim:presentation-typep
           ((,(car args) t) (type ,name))
           ,@body)))
    ,(when printer
       (let ((args (canonicalize-argument-list (first printer)))
             (body (rest printer)))
         `(clim:define-presentation-method
           clim:present
           (object (type ,name) stream (view clim:textual-view) &key)
           (let ((,(first args) object)
                 (,(second args) stream))
             ,@body))))
    ,(when accept-values-displayer
       (let* ((arglist (car accept-values-displayer))
              (stream (first arglist))
              (default (second arglist))
              (query-identifier (third arglist)))
         `(clim:define-presentation-method
           clim:accept-present-default
           ((type ,name)
            ,stream
            (view #+clim-1.0 clim:dialog-view
             #+clim-2 clim:gadget-dialog-view)
            ,default default-supplied-p present-p ,query-identifier
            #+(and clim-2 (not mcclim)) &key)
           (declare (ignore default-supplied-p present-p))
           ,@(cdr accept-values-displayer))))
    ,(when highlighter
       (let ((args (first highlighter)))
         `(clim:define-presentation-method
           clim:highlight-presentation
           ((type ,name) ,@ARGS)
           ,@(cdr highlighter))))))

(defmacro with-output-as-presentation
    ((&key stream object (type ''expression)
	   single-box (allow-sensitive-inferiors t)
	   dont-snapshot-variables
	   record-type)
     &body body)
  dont-snapshot-variables allow-sensitive-inferiors
  (or record-type (setq record-type ''clim::standard-presentation))
  `(clim:with-output-as-presentation
    (,stream ,object ,type
     :allow-sensitive-inferiors ,allow-sensitive-inferiors
     :single-box ,single-box :record-type ,record-type)
    ,@body))

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

(defmacro accepting-values ((stream &key own-window label (abort-value :ABORT)
				    (exit-boxes ''((:exit "   OK   ")
						   (:abort "   Cancel   "))))
			    &body body)
  ;; add :exit-boxes arg
  `(if (eq :abort
        (restart-case
            (clim:accepting-values
                (,stream :own-window ,own-window
                         :label ,label
                         :exit-boxes ,exit-boxes
                         :resynchronize-every-pass t
                         ;; get these things to come up
                         ;; in some nonrandom location
                         :x-position 200
                         :y-position 200
                         :scroll-bars :both)
              ,@body)
          (abort () :abort)))
    ;; If you quit using a keyboard accelerator, clim leaves the keystroke
    ;; in the input buffer (clim bug).
    ,abort-value t))

(defmacro for-each-frame ((symbol) &body body)
  "Iteratively bind SYMBOL to all enabled frames."
  `(clim:map-over-ports
    #'(lambda (port)
        (unless (eq (clim:port-type port) :postscript)
          (dolist (,symbol (clim:frame-manager-frames
                            (clim:find-frame-manager :port port)))
            (when (member (clim:frame-state ,symbol) '(:shrunk :enabled))
              ,@body))))))
