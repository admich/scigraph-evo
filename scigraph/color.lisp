;;; -*- Syntax: Common-lisp; Package: GRAPH -*-
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

(in-package :graph)

(defvar *colors* '() "Used for pop-edit.")

(defun init-colors (port)
  (setf *colors*
        (concatenate 'list
                     (list
                      +green+
                      +magenta+
                      +cyan+
                      +red+
                      +gold+
                      +sienna+
                      +salmon+
                      +orange+
                      +aquamarine+
                      +black+)
                     (make-contrasting-inks
                      (contrasting-inks-limit port)))))

(defmethod name ((color climi::named-color))
  (with-slots ((name climi::name)) color
    name))

(defun draw-color-swatch (stream color pretty-name selected-p &optional size)
  "Draw a sample of the given color at the current cursor position."
  (declare (ignore pretty-name))
  (or size (setq size (stream-line-height stream)))
  (let ((rad (1- (values (truncate size 2)))))
    (multiple-value-bind (x y) (stream-cursor-position stream)
      (draw-circle* stream (+ x rad 1) (+ y rad 1) rad
		   :filled t :ink color)
      (draw-rectangle* stream x (+ y size -1) (+ x size -1) y 
		      :filled nil :ink (if selected-p +foreground-ink+ +background-ink+)))))

(defun display-colors (&optional
		       (stream *standard-output*)
		       (colors *colors*))
  "Display the current colors and their names."
  (dolist (color colors)
    (terpri stream)
    (draw-color-swatch stream color nil nil)
    (stream-increment-cursor-position stream 15 0)
    (format stream " ~A " (name color))))

(defun display-gray-wash (&optional
			  (stream *standard-output*)
			  (quantum (/ 1.0 200)))
  "Display the range of gray scales."
  ;; Useful on 8-bit color systems to see what resolution there really is.
  (window-clear stream)
  (multiple-value-bind (left top right bottom) (stream-viewport stream)
    (let* ((intensity 0.0)
	   (width (- right left))
	   (height (- bottom top))
	   (increment 0.0))
      (setq left (+ left (* .1 width))
	    increment (* .8 quantum width)
	    right (+ left increment)
	    top (+ top (* .1 height))
	    bottom (- bottom (* .1 height)))
      (loop
	(if (> intensity 1.0) (return))
	(let ((gray (clim:make-rgb-color intensity intensity intensity)))
	  (draw-rectangle* stream left top right bottom :ink gray :filled t)
	  (setq left right right (+ right increment))
	  (incf intensity quantum))))))

(define-presentation-type-abbreviation color-presentation ()
  ;; Can't simply call this 'color' because that already names a class.
  `((member ,@*colors*)
    :name-key name
    :printer present-color
    :highlighter highlight-color))

(defun present-color (object stream &key acceptably)
  (declare (ignore acceptably))
  (with-room-for-graphics (stream)
    (draw-color-swatch stream object
		       (string object)
		       nil)))

(defun highlight-color (continuation object stream)
  (clim:surrounding-output-with-border
   (stream)
   (funcall continuation object stream)))
