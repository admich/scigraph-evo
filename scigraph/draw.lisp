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


(defvar %thickness 0)

;; (make-contrasting-dash-patterns 6)
(defvar *dash-patterns* 
  #(#(2 2) #(4 4) #(8 8) #(8 2) #(2 2 8 2) #(4 4 2 4)))

(defun device-draw-diamond (stream u v size &rest keys)
  "Given screen coordinates, clip and draw a diamond."
  (declare (fixnum u v size))
  (setq size (values (truncate size 2)))
  (let ((points (list u           (+ v size)
                      (- u size) v
                      u           (- v size)
                      (+ u size) v
                      u           (+ v size))))
    ;; No stack allocation, please, redisplay needs the list permanently.
    (apply #'draw-polygon* stream points keys)))

(defun device-draw-equilateral-triangle (stream u v side &rest keys)
  "Given screen coordinates, clip and draw a triangle centered on the given point."
  (declare (fixnum u v side))
  (let* ((x (values (truncate side 2)))
	 (y (values (round (the fixnum x) #.(sqrt 3.0)))))
    (declare (fixnum x y))
    (let* ((points (list (- u x) (+ v y)
                         u (- v (the fixnum (* y 2)))
                         (+ u x) (+ v y))))
      (apply #'draw-polygon* stream points
             keys))))




