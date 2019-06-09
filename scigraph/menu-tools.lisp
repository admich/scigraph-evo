;;; -*- Syntax: Common-lisp; Package: TOOL -*-
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

(in-package :tool)

(defun string-size (stream style format-string &rest format-args)
  ;; A bad implementation of this can really slow down graph generation.
  (unless (stringp format-string)
    (setq format-string (string format-string)))
  (when format-args
    ;; Typically, the formatter is not needed.
    (setq format-string (apply #'format nil format-string format-args)))
  (let* ((return #.(aref (format nil "~%") 0))
	 (line-cnt (1+ (count return format-string :test #'char=))))
    ;; This is 2-3 times faster than using continuation-output-size.
    (multiple-value-bind (x xmax)
	(stream-string-width stream format-string :text-style style)
      (declare (ignore x))
      (values xmax (* line-cnt (stream-line-height stream style))))))

;;; These things are needed mainly for annotations, but they are kept in a separate
;;; file to minimize the clutter in the annotations code.

(clim:define-presentation-type-abbreviation button-subset (&key alist (test 'equal))
  `(subset-alist ,alist :test ,test))

(defun SEVERAL-CHOOSE (ITEM-LIST
		       &key highlighted-values (label "Choose Several")
			    (stream *standard-output*) (own-window t))
  "Lets you select several choices."
  ;; Used by choose-descriptors to produce interval annotations.
  ;;
  ;; item-list is a list whose elements are either:
  ;;   a.  atoms
  ;;   b.  lists of length 2 whose CAR is the pretty name and whose CADR is the
  ;;        actual value.
  (labels ((stringify (thing)
	     (typecase thing
	       (string thing)
	       (symbol (symbol-name thing))
	       (otherwise (format nil "~A" thing)))))
    (let ((ptype `(button-subset
		   :alist
		   ,(mapcar #'(lambda (item)
				(if (atom item)
				    (list (stringify item)
					  :value item)
				  (list (stringify (car item))
					:value (cadr item))))
			    item-list))))
      (if (eq :abort
	      (accepting-values (stream :own-window own-window
					:label "Choose")
				(format stream label)
				(terpri stream)
				(setq highlighted-values
				  (accept
				   ptype
				   :default highlighted-values
				   :view
				   '(check-box-view :orientation :vertical)
				   :prompt "Choose Several"
				   :stream stream))
				(terpri stream)))
	  (values nil t)
	(nreverse highlighted-values)))))

(defun test-chooser ()
  (several-choose '(apples oranges pears)))

(defun character-style-choices (family)
  (mapcar
   #'(lambda (style)
       `(,(apply #'format nil "~A ~A ~A" style)	:value ,style :style ,style))
   (mapcar #'(lambda (face-size) (cons family face-size))
	   (mapcan #'(lambda (size)
		       `(((:bold :italic) ,size)
			 (:bold ,size)
			 (:italic ,size)
			 (:roman ,size)))
		   '(:very-large :large :normal :small :very-small)))))

(defun CHOOSE-CHARACTER-STYLE ()
  (let* ((family (menu-choose
		  (mapcar #'(lambda (fam)
			      `(,fam :value ,fam :style (,fam :roman :normal)))
			  '(:fix :serif :sans-serif))
		  :label "Family"
          :associated-window *standard-input*))
	 (style (when family (menu-choose (character-style-choices family)
                                      :label "Character Styles"
                                      :associated-window *standard-input*))))
    style))

(defun WINDOW-EDIT-TEXT (window left top right bottom &optional string)
  "Edit text in the given region of the window."
  (declare (ignore left top right bottom))
  (let ((new-string string))
      (accepting-values (window :own-window t)
        (setf new-string (accept 'string :stream window :prompt "Input a string" :default string :view '(text-editor-view :ncolumns 80 :nlines 10))))
      new-string))


