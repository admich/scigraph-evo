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

;;; BASIC GRAPH CLASSES.

;;; To do:

;;; o High level idea: Separate transform, from graphics state and style
;;; o Move essential-graph-display into a basic-object mixin.

(defvar *max-digits* 6 
  "Default number of digits of precision in a float for axis labeling.")

(defvar *width-between-graphs* 3 "Pixels between graphs horizontally.")
(defvar *height-between-graphs* 3 "Pixels between graphs vertically.")

;;; THE FOLLOWING MACROS ARE USED IN METHOD BODIES FOR DRAWING.

(defun constrain-value-between (min x max)
  ;; this is wrong (min (max min x) max)
  (if (<= min max) (min (max min x) max) (min (max max x) min)))

(defun constrain-point-in-rectangle (x y left bottom right top)
  (values (constrain-value-between left x right)
	  (constrain-value-between bottom y top)))

(defclass graph-box
	  ()
  ((left :initform 50 :initarg :left :accessor left)
   (top :initform 50 :initarg :top :accessor top)
   (width :initform 700 :initarg :width :accessor width)
   (height :initform 700 :initarg :height :accessor height)))

(defmethod inside-box ((graph graph-box))
  "Return a bounding rectangle of the inside box in the stream coordinate"
  (let ((left (left graph))
        (top (top graph))
        (width (width graph))
        (height (height graph)))
    (make-rectangle* left top (+ left width) (+ top height))))

(defmethod set-inside-box ((graph graph-box) rectangle stream)
  "Set the inside box to rectangle in the stream coordinates"
  (declare (ignore stream))
  (with-slots (left top width height) graph
    (multiple-value-bind (min-x min-y max-x max-y) (rectangle-edges* rectangle)
      (setf left min-x
            top min-y
            width (- max-x min-x)
            height (- max-y min-y)))))


(defmethod outside-box ((graph graph-box))
  "Return a bounding rectangle of the outside box in the stream coordinates"
  (inside-box graph))

(defmethod set-outside-box ((graph graph-box) rectangle stream)
  (set-inside-box graph rectangle stream))

(defclass XY-BOX
	  ()
  ((x-min :initform 0.0 :initarg :x-min :accessor x-min)
   (y-min :initform 0.0 :initarg :y-min :accessor y-min)
   (x-max :initform 1.0 :initarg :x-max :accessor x-max)
   (y-max :initform 1.0 :initarg :y-max :accessor y-max)))

(defmethod xy-inside ((self xy-box))
  (with-slots (x-min y-min x-max y-max) self
    (values x-min x-max y-min y-max)))

(defmethod set-xy-inside ((self xy-box) STREAM left right bottom top)
  (declare (ignore STREAM))
  (with-slots (x-min y-min x-max y-max) self
    (when left   (setq x-min left))
    (when right  (setq x-max right))
    (when bottom (setq y-min bottom))
    (when top    (setq y-max top))))

(defmethod xy-rectangle ((self xy-box))
  (with-slots (x-min y-min x-max y-max) self
    (make-rectangle*  x-min y-min x-max y-max)))



(defclass basic-graph-coordinates-mixin
	  (xy-box
       graph-box)
  ()
  (:documentation
    "(x y) coordinate system."))

(defmethod x-scale ((self basic-graph-coordinates-mixin))
  "Determine scales used in mapping between x-y and u-v."
  (with-slots (x-min x-max width) self
    ;; Float to avoid consing big-rats.
    (/ width (float (- x-max x-min)))))

(defmethod y-scale ((self basic-graph-coordinates-mixin))
  "Determine scales used in mapping between x-y and u-v."
  (with-slots (y-min y-max height) self
    ;; Float to avoid consing big-rats.
    (/ height (float (- y-max y-min)))))

(defmethod xy-to-rs-transformation ((graph basic-graph-coordinates-mixin))
  (with-slots (width height x-min y-min x-max y-max) graph
    (make-transformation (x-scale graph) 0 0 (* -1 (y-scale graph)) (* -1 (x-scale graph) x-min) (+ (* (y-scale graph) y-min) height))))

(defmethod rs-to-xy-transformation ((graph basic-graph-coordinates-mixin))
  (invert-transformation (xy-to-rs-transformation graph)))

(defmethod rs-to-stream-transformation ((graph graph-box))
  (with-slots (left top) graph
    (make-transformation 1 0 0 1 left top)))

(defmethod stream-to-rs-transformation ((graph graph-box))
  (invert-transformation (rs-to-stream-transformation graph)))

(defmethod stream-to-xy-transformation ((graph basic-graph-coordinates-mixin))
  (compose-transformations (rs-to-xy-transformation graph) (stream-to-rs-transformation graph)))

(defmethod xy-to-stream-transformation ((graph basic-graph-coordinates-mixin))
  (invert-transformation (stream-to-xy-transformation graph)))

(defmethod stream-to-xy ((graph basic-graph-coordinates-mixin) x y )
  (transform-position (stream-to-xy-transformation graph) x y))

(defmethod xy-to-stream ((graph basic-graph-coordinates-mixin) x y)
  (transform-position (xy-to-stream-transformation graph) x y))

(defmethod rs-to-xy ((graph basic-graph-coordinates-mixin) x y)
  (transform-position (rs-to-xy-transformation graph) x y))

(defmethod xy-to-rs ((graph basic-graph-coordinates-mixin) x y)
  (transform-position (xy-to-rs-transformation graph) x y))

(defmethod rs-to-stream ((graph basic-graph-coordinates-mixin) x y)
  (transform-position (rs-to-stream-transformation graph) x y))

(defmethod stream-to-rs ((graph basic-graph-coordinates-mixin) x y)
  (transform-position (stream-to-rs-transformation graph) x y))

(defmacro with-xy-coordinates ((graph stream) &body body)
  `(with-local-coordinates (stream 0 0)
     (with-drawing-options (,stream
                            :transformation (xy-to-stream-transformation ,graph))
       ,@body)))

(defmacro with-rs-coordinates ((graph stream) &body body)
  `(with-local-coordinates (stream 0 0)
     (with-drawing-options (,stream
                            :transformation (rs-to-stream-transformation ,graph))
       ,@body)))

(defmethod set-xy-inside :after ((self basic-graph-coordinates-mixin)
				 STREAM left right bottom top)
  (declare (ignore stream))
  (when (or left right bottom top)
    (rescale self)))

(defmethod rescale ((self basic-graph-coordinates-mixin))
  "Determine scales used in mapping between x-y and u-v."
  ;; now the scale factor is not a slot but is a function
  '())


(defclass essential-graph-margin-mixin (basic-graph-coordinates-mixin)
  ;; Margin sizes in pixels.
  ((left-margin-size   :initform 10 :reader left-margin-size)
   (right-margin-size  :initform 10 :reader right-margin-size)
   (bottom-margin-size :initform 10 :reader bottom-margin-size)
   (top-margin-size    :initform 10 :reader top-margin-size))
  (:documentation
   "Provides graphs with a margin outside the normal plotting region."))

(defgeneric verify-new-outside (self outside-rectangle
				     left-margin top-margin right-margin
				     bottom-margin)
  (:method-combination or #-Lucid :most-specific-last ))

(defmethod margins ((self essential-graph-margin-mixin))
  (with-slots (left-margin-size right-margin-size
	       bottom-margin-size top-margin-size) self
    (values left-margin-size top-margin-size right-margin-size
	    bottom-margin-size)))

;;; Have the committee on code purity look at this!
;;; BW:  this resets the old values of uv-inside
;;;      if the new ones (from :set-uv-outside) were bad!!!!
(defmethod set-margins ((self essential-graph-margin-mixin) lm tm rm bm)
  (with-slots (left-margin-size right-margin-size
	       bottom-margin-size top-margin-size) self
    (setf left-margin-size lm
	  right-margin-size rm
	  bottom-margin-size bm
	  top-margin-size tm)))

(defmethod reset-margins ((self essential-graph-margin-mixin) stream
			  &optional verifyp)
  "Recompute margins after verifying they are OK."
  (let ((old-outside-box (outside-box self)))
        (multiple-value-bind (lm tm rm bm)
            (compute-margins self stream)
          (unless (or (new-outside-check self verifyp
                                         old-outside-box lm tm rm bm)
                      verifyp)
            (set-margins self lm tm rm bm)))))

(defgeneric compute-margins (self stream)
  (:documentation "Compute the margin sizes.  Whopperize this to add
  margin size.  DON'T SIDE EFFECT.  Return left-margin top-margin right-margin
  bottom-margin"))

(defmethod compute-margins ((self essential-graph-margin-mixin) stream)
  (declare (ignore stream))
  (values 0 0 0 0))

;;; BW: change name from :new-outside-ok-p
(defmethod new-outside-check ((self essential-graph-margin-mixin)
			      complain-p
                  outside-rectangle
			      left-margin top-margin right-margin
			      bottom-margin)
  "Check that new edges and margin is OK, complaining"
  (let ((error (verify-new-outside
		self
		outside-rectangle
		left-margin top-margin right-margin bottom-margin)))
    (when (and error complain-p)
	  (cerror error nil))
    ;; BW: return error, rather than success flag
    error))

(defmethod verify-new-outside or ((self essential-graph-margin-mixin)
				   outside-rectangle
				   left-margin  top-margin right-margin
				   bottom-margin)
  "Return a error message if you don't like these edges and margins.
   This method just checks that there is room left to plot."
  (when (or (< (- (rectangle-width outside-rectangle) left-margin right-margin) 0)
	    (< (- (rectangle-height outside-rectangle) top-margin bottom-margin ) 0))
    "Not enough room for inside of graph."))

(defmethod set-outside-box :around ((graph essential-graph-margin-mixin) box stream)
  (reset-margins graph stream)
  (with-slots (left-margin-size right-margin-size bottom-margin-size top-margin-size) graph
    (multiple-value-bind (min-x min-y max-x max-y) (rectangle-edges* box)
      (call-next-method graph
                        (make-rectangle* 
                         (+ min-x left-margin-size)
                         (+ min-y top-margin-size)
                         (- max-x right-margin-size)
                         (- max-y top-margin-size))
                        stream))))

(defmethod outside-box ((graph essential-graph-margin-mixin))
  (multiple-value-bind (x0 y0 x1 y1) (rectangle-edges* (inside-box graph))
    (make-rectangle* (- x0 (left-margin-size graph)) (- y0 (top-margin-size graph)) (+ x1 (right-margin-size graph)) (+ y1 (bottom-margin-size graph)))))


;;; This is now used by graphs, sliders, and annotations so that an object
;;; knows when it is displayed.
(defclass essential-display-mixin ()
  ((Displayed? :initform nil :initarg :displayed? :accessor displayed?)))

(defmethod display :after ((self essential-display-mixin) (STREAM t))
  (setf (displayed? self) t))

(defmethod erase :after ((self essential-display-mixin) (STREAM t))
  (setf (displayed? self) nil))

(defmethod refresh ((self essential-display-mixin) STREAM)
  "Erase, then draw."
  (erase self stream)
  (display self STREAM))


(defclass basic-graph 	(essential-graph-margin-mixin
                             essential-display-mixin
                             named-mixin)
  ()) ; How edges were specified.

(defmethod display :around ((self basic-graph) (STREAM t))
  "Set some stream"
  (let ((box (inside-box self)))
    (with-local-coordinates (stream (rectangle-min-x box) (rectangle-min-y box))
      (call-next-method))))

(defmethod display ((graph basic-graph) (STREAM t))
  "Render a graph on its display medium.  Primary method is a stub."
  nil)

(defmethod graph-with-clipping ((self basic-graph) STREAM inside-p continuation)
  ;; Internal to WITH-CLIPPING-TO-GRAPH macro.
  (let ((clip-region (transform-region
                      (stream-to-rs-transformation self)
                      (if inside-p (inside-box self) (outside-box self)))))
    (with-drawing-options (stream :clipping-region clip-region)
      (funcall continuation))))

(defmethod default-text-style ((graph basic-graph) stream)
  (medium-text-style stream))

(defmacro WITH-CLIPPING-TO-GRAPH ((graph STREAM inside-p) &body body)
  "Perform body while constraining clipping to GRAPH.  If INSIDE-P,
   the clipping rectangle is the inside of the graph, otherwise it is the
   outside." 
  `(graph-with-clipping
     ,graph ,STREAM ,inside-p
     #'(lambda ()
	 ,@body)))


;;; put commands in both the GRAPH and GLOBAL command tables.
(defmacro define-graph-command (name arguments &body body)
  `(progn (define-command (,name :command-table :graph :name t) ,arguments ,@body)
          (add-command-to-command-table ',name :global :name t :errorp nil)))

