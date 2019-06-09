;;; -*- Syntax: Common-lisp; Package: graph -*-
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

;;;
;;; Mouse stuff
;;;

(defmacro button-case (button &key left middle right)
  "Implementation-specific way to dispatch based on the button pushed."
  `(cond
    ,@(when left
        `(((event-matches-gesture-name-p ,button :select)
           ,left)))
    ,@(when middle
        `(((event-matches-gesture-name-p ,button :describe)
           ,middle)))
    ,@(when right
        `(((event-matches-gesture-name-p ,button :menu)
           ,right)))))

(defmethod post-mouse-documentation (stream string)
  (declare (ignore stream))
  (clim-extensions:frame-display-pointer-documentation-string
   *application-frame* string))

(defmacro with-mouse-documentation ((window string) &body body)
  `(unwind-protect
       (progn (post-mouse-documentation ,window (or ,string " ")) ,@body)
     (post-mouse-documentation ,window " ")))

(defmacro with-pointer-cursor ((sheet cursor) &body body)
  `(let ((.old. (sheet-pointer-cursor ,sheet)))
     (unwind-protect
	 (progn (setf (sheet-pointer-cursor ,sheet) ,cursor)
		,@body)
       (setf (sheet-pointer-cursor ,sheet) .old.))))

;;; DRAG-ICON is used to do most all of the mouse tracking.  It differs from
;;; dragging-output in that the latter simply does output once and drags
;;; the output record around the screen.  This function explicitly erases and
;;; redraws, which is useful if the shape of the output depends upon its location
;;; (e.g. sliders).
(defun drag-icon (stream draw-it  move-it 
		          &optional documentation (cursor :move))
  "Mouse tracker for dragging graphic objects."
  ;; Erase the object before calling this function.
  ;; Dont forget to redraw the object after this function returns.
  ;; This requirement gives the caller the freedom to use an "abbreviated"
  ;; drawing for the inner loop, which may be necessary to create the
  ;; illusion of animation.
  (unless (extended-input-stream-p stream)
	(error "Cannot track the mouse on this stream (~S)" stream))
  (with-pointer-cursor (stream cursor)
    (let (last-x last-y output-record
	             (movements 0)
	             ;; If we have had some movement and then the mouse is released, we
	             ;; probably want to quit the loop.  We don't count the first few because the
	             ;; user might still be releasing the button that got him here.
	             (down-threshold 0)
	             (up-threshold 0))
      ;; Sometimes we get rationals.
      ;; (declare (fixnum last-x last-y movements))
      (unless documentation
	    (setq documentation "Click/Release mouse to set new position"))
      (multiple-value-setq (last-x last-y) (stream-pointer-position stream))
      (unless (and last-x last-y) (beep) (setq last-x 0 last-y 0))
      (labels ((erase () (when output-record (erase-output-record output-record stream nil)))
               (draw () (erase) (setf output-record (with-new-output-record (stream) (funcall draw-it stream))))
               (update-position (x y)
	           ;; "pixel" positions are often ratios and floats in clim
	           (post-mouse-documentation stream documentation)
	           (let ((dx (- x last-x))
		             (dy (- y last-y)))
		         ;;(declare (fixnum dx dy) (fixnum x y))
		         (when (or (not (zerop dx)) (not (zerop dy)))
		           (incf movements)
		           (funcall move-it dx dy)
		           (setq last-x x last-y y)
                   (draw)
		           ;; In X-windows, you need to force any buffered output.
		           (force-output stream))))
	           (button-clicked (button release-p)
	             ;; Seem to get spurious left-click button releases shortly
	             ;; after entering the tracker (13 movements).  Maybe leftover
	             ;; from the presentation menu that got us here...
	             (when (if release-p
			               (> movements up-threshold)
		                   (> movements down-threshold))
                   (erase)
		           (return-from drag-icon (values button last-x last-y)))))
	    (unwind-protect
	         (progn
               (draw)
		       (force-output stream)
		       (with-mouse-documentation (stream documentation)
                 ;; without :multiple-window t the pointer jump back and forth in x axis. I don't know why
		         (tracking-pointer (stream :multiple-window t)
				   (:pointer-motion
					(x y)
					(update-position (values (truncate x)) (values (truncate y))))
				   (:pointer-button-press 
					(event x y) 
					(update-position (values (truncate x)) (values (truncate y)))
					(button-clicked event nil)))))
	      ;; CLIM leaves the button event resulting from :button-press in the input
	      ;; buffer, so take it out now.
	      (force-output stream))))))

#|
(defun test-tracking (&optional (stream *standard-output*))
  (let ((string "Test Tracking") x y)
    (multiple-value-setq (x y) (stream-pointer-position* stream))
    (drag-icon stream
	       #'(lambda (stream) (draw-string string x y :stream stream :ink +flipping-ink+))
	       #'(lambda (dx dy)
		   (incf x dx)
		   (incf y dy)
		   (setq string (format nil "~S ~S" x y))))))
|#

(defun device-mouse-point (stream
			   &optional
			   (documentation "Mouse-Left: Select Point; Mouse-Middle: Cancel"))
  "Returns stream x, y chosen by the mouse."
  (with-pointer-cursor (stream :position)
    (tracking-pointer (stream)
      (:pointer-button-press (event x y)
                             (return-from device-mouse-point (and (or (event-matches-gesture-name-p event :select)
                                                                      (event-matches-gesture-name-p event :menu))
                                                                  (values x y)))))))

(defun map-polygon-edges (function corners)
  (let* ((this (car (last corners)))
	 (next (pop corners))
	 (x1 (pop this))
	 (y1 (pop this))
	 (x2 (pop next))
	 (y2 (pop next)))
    (loop
      (if (not x2) (return))
      (funcall function x1 y1 x2 y2)
      (setq next (pop corners))
      (setq x1 x2 y1 y2)
      (setq x2 (pop next) y2 (pop next)))))

(defun draw-screen-polygon (corners stream ink)
  (map-polygon-edges
    #'(lambda (x1 y1 x2 y2)
	(draw-line* stream x1 y1 x2 y2 :ink ink))
    corners))

(defun select-screen-polygon (stream &optional (cursor :position))
  "Select a sequence of points in screen coordinates.  Finish by clicking on first point."
  (multiple-value-bind (x0 y0) (device-mouse-point stream)
    (let* ((first (list x0 y0))
           (first-highlighted nil)
           (points (list first))
           (rad 5)
           (documentation
            "Mouse-Left: Select Point; Mouse-Middle: Cancel; Mouse-Right: Finish")
           (last-x x0)
           (last-y y0)
           first-highlighted)
      (unwind-protect
           (labels ((distance (x1 y1 x2 y2)
                      (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))
                    (near-first (x0 y0)
                      (< (distance x0 y0 (car first) (cadr first)) rad))
                    (draw (x y state)
                        (declare (ignore state))
                        (with-drawing-options (stream :ink +flipping-ink+)
                          (draw-line* stream last-x last-y x y)
                          (when (near-first x y)
                            (draw-circle* stream x0 y0 5)
                            (setf first-highlighted t)))))
             (draw-circle* stream x0 y0 5  :filled nil)
             (loop
                  (multiple-value-bind (nx ny)
                     (climi::dragging-drawing stream #'draw :finish-on-release nil :multiple-window t) 
                    (push (list nx ny) points)
                    (draw-line* stream last-x last-y nx ny :ink +flipping-ink+)
                    (setf last-x nx last-y ny)
                    (when (near-first nx ny)  (return points)))))
        (progn (draw-circle* stream x0 y0 5 :ink +flipping-ink+ :filled nil)
               (draw-screen-polygon points stream +flipping-ink+))))))
