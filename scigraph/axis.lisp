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

;;; AXIS LABELING

(defun down (x d)
  "Round x down modulo d."
  (- x (mod x d)))

(defun AUTOTICK-internal (xmin xmax div max-ticks &rest choices)
  (let ((range (/ (abs (- xmax xmin)) div))
	(ifac 1.0)
	(tick 1.0))
    ;; Beat range so that 1.0 <= range <= 10.0 and compute ifac
    ;; so that original-range = ifac * range
    (loop while (> range 10.0) do
      (setq ifac (* ifac 10.0))
      (setq range (/ range 10.0)))
    (loop while (<= range 1.0) do
      (setq ifac (/ ifac 10.0))
      (setq range (* range 10.0)))
    (setq tick
	  (loop for c in choices
		when (<= (* range c) max-ticks)
		  do (return (/ 1.0 c))
		finally (return 1.0)))
    (* tick ifac div))) 

(defun TIME-AUTOTICK (xmin xmax)
  (let* ((range (abs (- xmax xmin)))
	 (interval (do ((intervals '#.(list (* 365 86400) (* 7 86400)
					    86400 3600 60 1)
				   (cdr intervals)))
		       ((null intervals) 1)
		     (when (> range (first intervals))
		       (return (if (> range (* 2 (first intervals)))
				   (first intervals)
				   (or (second intervals) 1))))))
	 (tick (values (round (autotick-internal xmin xmax interval 12 4 2)))))
    (if (> interval 3600) tick
	(* (values (ceiling tick interval)) interval))))

(defun auto-tick (min max)
  (let* ((range (- max min))
	 (tick (expt 10 (truncate (log range 10))))
	 (count (/ range tick)))
    (coerce (cond ((<= count 2) (/ tick 5))
		  ((<= count 5) (/ tick 2))
		  (t tick))
	    'single-float)))

(defun make-adjustable-string (&optional (size 7))
  ;; (internal to float-to-string)
  ;; Cons a new string every time; if you reuse an old string, you get graphics
  ;; turds.
  (make-array size
	      :element-type 'character
	      :adjustable t
	      :fill-pointer 0))

(defun float-to-string (number &optional (max-digits *max-digits*) (string nil))
  "Converts a FLOAT into as short a string as possible while providing
   max-digits of accuracy.  The string may take (+ MAX-DIGITS 6)
   characters to print.  If OUTPUT-STRING, a string with a fill pointer
   is provided, it will be filled.  Otherwise a new string will be created."
  ;; Sliders call this within mouse-tracking, so it needs to be fast.
  (or max-digits (setq max-digits *max-digits*))	; user provided nil?
  ;; String is assumed to have a fill pointer.
  (if string
      (setf (fill-pointer string) 0)
      (setq string (make-adjustable-string (+ max-digits (if (minusp number) 2 1)))))
  (if (zerop number)
      ;; Handle zero as a special case.  float-to-string-internal can't deal with it?
      (progn (vector-push-extend #\0 string) string)
      (float-to-string-internal number max-digits string)))

(defun float-to-string-internal (number max-digits string
				 &aux (exponent 0) ilength flength
				      elength exponent-p
				      (extension 5))
  (when (< number 0)	; Sign.
    (vector-push-extend #\- string extension)
    (setq number (abs number)))
  (loop while (>= number 10.0)			
	do (setq number (/ number 10.0))
	   (incf exponent))
  (loop while (< number 1.0)
	do (setq number (* number 10.0))
	   (decf exponent))
  ;; now original (abs number) = number * 10^exponent, 1.0 <= number < 10.0
  (incf number (/ 5.0 (expt 10 max-digits)))	; Round up
  (when (>= number 10.0)	; But not too much.
    (setq number (/ number 10.0))
    (incf exponent))
  (cond ((or (> exponent (1- max-digits))	; E format.
	     (< exponent -3))
	 (setq ilength 1
	       exponent-p t))
	(t (setq ilength	; F format.
		 (if (>= exponent 0) (1+ exponent) 0)
		 exponent-p nil)))
  (macrolet
    ((push-digits (number length string)
       `(dotimes (.i. ,length)
	 (vector-push-extend (digit-char (values (floor ,number))) ,string extension)
	  (setf ,number (mod (* 10.0 ,number) 10.0)))))
    (push-digits number ilength string)	; Integer part.
    (setq flength (- max-digits ilength))	; Fractional part.
    (when (or (> flength 0) exponent-p (< exponent 0))
      (vector-push-extend #\. string extension)
      (when (not exponent-p)
	(loop while (< exponent -1)
	      do (vector-push-extend #\0 string extension)
		 (incf exponent)))
      (when (not (= number 0.0))
	(push-digits number flength string))
      (loop while (char= (char string (decf (fill-pointer string))) #\0)
	    finally (incf (fill-pointer string)))
      (if (char= (char string (1- (fill-pointer string))) #\.)
	  (decf (fill-pointer string))))
    (when exponent-p	; Exponent
      (vector-push-extend #\e string extension)
      (when (< exponent 0)
	(vector-push-extend #\- string extension)
	(setq exponent (abs exponent)))
      (setq elength 1)
      (loop while (>= exponent 10.0)
	    do (setq exponent (/ exponent 10.0))
	       (incf elength))
      (push-digits exponent elength string))
    string))

(defun draw-linear-axis
    (graph
     stream
     major-min major-max	
     minor
     direction ;:x :y
     dtick	; Tick spacing in axis units or a list of ticks.  
     tick-size	; Length of tick in pixels.  Ticks are draw on the
                                        ; left side of axis if tick-size > 0, else right side.
     tick-numbering ; Should axis numbers be added?  They are placed
			    ; on the side of the axis opposite the ticks.
			    ; Values are NIL, :MINIMAL, or :EACH.
     axis-number ; a function with lambda list (x y tick)
     )
  (with-xy-coordinates (graph stream)
      (if (equal direction :y)
          (draw-line* stream minor major-min minor major-max)
          (draw-line* stream major-min minor major-max minor)))
  (let* ((gated-ticks (and (consp dtick)
                           (remove-if-not (lambda (x) (<= major-min x major-max)) dtick)))
         (ticks (or gated-ticks
                    (loop for tick from (+ (down major-min dtick) dtick) below major-max
                       by dtick collect tick))))
    (loop for tick in ticks
       with first-tick = (first ticks)
       and last-tick = (car (last ticks)) do
         (with-xy-coordinates (graph stream)
           (if (equal direction :y)
              (draw-line* stream minor tick (+ minor tick-size) tick)
              (draw-line* stream tick minor tick (+ minor tick-size))))
         (when (and axis-number (or (eq tick-numbering :each)
                                    (and (eq tick-numbering :minimal)
                                         (or (= tick first-tick)
                                             (= tick last-tick)))))
           (if (equal direction :y)
               (multiple-value-bind (x y) (xy-to-rs graph minor tick)
                 (funcall axis-number x y tick))
               (multiple-value-bind (x y) (xy-to-rs graph tick minor)
                 (funcall axis-number x y tick)))))))

