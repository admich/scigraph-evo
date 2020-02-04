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

;;; SOME GRAPH MIXINS



(defclass GRAPH-MOUSE-RESOLUTION-MIXIN (basic-graph)
  ((dx-mouse :initform nil :initarg :dx-mouse)  ; Resolution (in units)
   (dy-mouse :initform nil :initarg :dy-mouse)) ; used to convert from
						; mouse -> xy coordinates
 )

(defmethod rescale :after ((self graph-mouse-resolution-mixin))
  (compute-mouse-resolution self))

(defmethod compute-mouse-resolution ((self graph-mouse-resolution-mixin))
  ;;This method provides at least 1 pixel accuracy.
  (with-slots (dx-mouse dy-mouse) self
    (setq dx-mouse (expt 10.0 (values (floor (log (/ (x-scale self)) 10))))
          dy-mouse (expt 10.0 (values (floor (log (/ (y-scale self)) 10)))))))

(defmethod initialize-instance :after ((self graph-mouse-resolution-mixin) &key)
  (compute-mouse-resolution self))

(defmethod rs-to-xy :around ((self graph-mouse-resolution-mixin) r s)
  (with-slots (dx-mouse dy-mouse) self
    (multiple-value-bind (x y)
        (call-next-method self r s)
      (values (* (values (round x dx-mouse)) dx-mouse)
          (* (values (round y dy-mouse)) dy-mouse)))))



(defclass graph-border-mixin (basic-graph)
  ((show-border :initform t :initarg :show-border :accessor show-border)
   (tick-size :initform 7. :initarg :tick-size)	   ;In pixels.
   (title :initform nil :initarg :title :accessor title) ;Title centered
   (x-label :initform nil :initarg :x-label        ;Label for left x axis.
	    :accessor x-label)
   (x-digits :initform 6 :initarg :x-digits :accessor x-digits)
   (x-auto-tick :initform t :initarg :x-auto-tick)  ;Auto-tick?
   (x-dtick :initform nil :initarg :x-dtick)        ;Tk Spacing if not auto.
   (x-tick-numbering :initform :minimal	           ; :minimal, :each, or nil.
		     :initarg :x-tick-numbering)

   (y-label :initform nil :initarg :y-label :accessor y-label)
   (y-digits :initform 6 :initarg :y-digits :accessor y-digits)
   (y-auto-tick :initform t :initarg :y-auto-tick)
   (y-dtick :initform nil :initarg :y-dtick)
   (y-tick-numbering :initform :minimal
		     :initarg :y-tick-numbering)
   (visible-borders :initform (copy-list '(:left :bottom :right :top))
		    :initarg :visible-borders
		    :accessor visible-borders))

  (:documentation "Simple Border and axis labeling."))

(defmethod (setf x-dtick) (arg (self graph-border-mixin))
  (declare (ignore arg))
  (with-slots (x-auto-tick) self
    (setq x-auto-tick nil)))

(defmethod (setf y-dtick) (arg (self graph-border-mixin))
  (declare (ignore arg))
  (with-slots (y-auto-tick) self
    (setq y-auto-tick nil)))

(defmethod COMPUTE-MARGINS :around ((self graph-border-mixin) stream)
  (multiple-value-bind (left top right bottom) (call-next-method)
    (with-slots (x-tick-numbering) self
      (values (+ left (stream-line-height stream) (* *max-digits* (stream-character-width stream)))
              (+ (* 2 (stream-line-height stream)) top) ; space for title
              (+ (stream-character-width stream) right)	; 1 character right
              (+ (* (if x-tick-numbering 3 2)	; X numbers and label
                    (stream-line-height stream)) bottom))))) 

(defmethod initialize-instance :after ((self graph-border-mixin)
					&key &allow-other-keys)
  (with-slots (title) self
    (when title (setf (title self) title))))

(defmethod (setf title) (new-title (self graph-border-mixin))
  (with-slots (title) self
    (setq title 
	  (when title
	    (cond ((stringp new-title) new-title)
		  ((symbolp new-title) (string new-title))
		  (t (format nil "~a" new-title)))))))


;;; These methods determine what the tick spacing should be.
(defmethod x-tick-spacing ((self graph-border-mixin))
  (with-slots (x-auto-tick x-min x-max x-dtick) self
    (if x-auto-tick (auto-tick x-min x-max) x-dtick)))

(defmethod y-tick-spacing ((self graph-border-mixin))
  (with-slots (y-auto-tick y-min y-max y-dtick) self
    (if y-auto-tick (auto-tick y-min y-max) y-dtick)))

(defmethod display-x-label ((self graph-border-mixin) stream)
  (with-slots (tick-size) self
    (let* ((label (x-label self))
           (inside-box (inside-box self))
           (x (/ (rectangle-width inside-box) 2))
           (y (+ (stream-line-height stream) (rectangle-height inside-box))))
      (when label
        (draw-text* stream label x y :align-x :center :align-y :top)))))

(defmethod display-y-label ((self graph-border-mixin) stream)
  (let* ((label (y-label self))
         (inside-box (inside-box self))
         (x (* -1 *max-digits* (stream-character-width stream)))
         (y (/ (rectangle-height inside-box) 2)))
    (when label
      (draw-text* stream label x y :align-x :center :transform-glyphs t :transformation (make-rotation-transformation* (/ pi -2 ) x y )))))

(defmethod display-title ((self graph-border-mixin) STREAM)
  (alexandria:when-let ((the-title (title self)))
    (with-slots (width) self
      (draw-text* stream  the-title  (/ width 2) -2 :align-x :center :align-y :bottom :ink +foreground-ink+))))

(defmethod display-labels ((self graph-border-mixin) stream)
  (display-x-label self stream)
  (display-y-label self stream)
  (display-title self stream)
  (force-output stream))

(defmethod display-border ((self graph-border-mixin) STREAM)
  (when (show-border self)
    (let* ((visible (visible-borders self)))
        (when (member :right visible)
          (display-right-border self STREAM nil))
        (when (member :top visible)
          (display-top-border self STREAM nil))
        (when (member :left visible)
          (display-left-border self STREAM nil))
        (when (member :bottom visible)
          (display-bottom-border self STREAM nil))
        (when (member :zero-abcissa visible)
          (display-zero-abcissa self stream nil))
        (when (member :zero-ordinate visible)
          (display-zero-ordinate self stream nil))
        (force-output stream))))

(defmethod DISPLAY-ZERO-ABCISSA ((self GRAPH-BORDER-MIXIN) STREAM line-drawer)
  (with-slots (x-min x-max tick-size x-tick-numbering) self
    (let* ((dtick (x-tick-spacing self))
           (tick-size (/ tick-size (y-scale self))))
      (draw-linear-axis self stream x-min x-max 0 :x dtick tick-size x-tick-numbering
                        #'(lambda (r s number)
                            (draw-text* stream
                                        (float-to-string number (x-digits self))
                                        (if (zerop number) (+ r (/ (stream-character-width stream) (x-scale self))) r)
                                        s
                                        :align-x :center :align-y :top))))))

(defmethod DISPLAY-BOTTOM-BORDER ((self graph-border-mixin) STREAM line-drawer)
  (with-slots (x-min x-max y-min tick-size x-tick-numbering) self
    (let* ((dtick (x-tick-spacing self))
           (tick-size (/ tick-size (y-scale self))))
      (draw-linear-axis self stream x-min x-max y-min :x dtick tick-size x-tick-numbering
                        #'(lambda (r s number)
                            (draw-text* stream (float-to-string number (x-digits self)) r (+ s 2) :align-x :center :align-y :top))))))

(defmethod display-top-border ((self graph-border-mixin) STREAM line-drawer)
  (with-slots (x-min x-max y-max tick-size x-tick-numbering) self
    (let* ((dtick (x-tick-spacing self))
           (tick-size (/ (* -1 tick-size) (y-scale self))))
      (draw-linear-axis self stream x-min x-max y-max :x dtick tick-size x-tick-numbering nil))))

(defmethod DISPLAY-ZERO-ORDINATE ((self GRAPH-BORDER-MIXIN) STREAM line-drawer)
  (with-slots (y-min y-max tick-size y-tick-numbering) self
    (let* ((dtick (y-tick-spacing self))
           (tick-size (/ tick-size (x-scale self))))
      (draw-linear-axis self stream y-min y-max 0 :y dtick tick-size y-tick-numbering
                        #'(lambda (r s number)
                            (draw-text* stream
                                        (float-to-string number (x-digits self))
                                        r
                                        (if (zerop number) (+ s (/ (stream-line-height stream) (y-scale self))) s)
                                        :align-x :right :align-y :center))))))

(defmethod DISPLAY-LEFT-BORDER ((self graph-border-mixin) STREAM line-drawer)
  (with-slots (x-min y-max y-min tick-size y-tick-numbering) self
    (let* ((dtick (y-tick-spacing self))
           (tick-size (/ tick-size (x-scale self))))
      (draw-linear-axis self stream y-min y-max x-min :y dtick tick-size y-tick-numbering
                        #'(lambda (r s number)
                            (draw-text* stream (float-to-string number (y-digits self))  (- r 1) s :align-x :right :align-y :center))))))


(defmethod DISPLAY-RIGHT-BORDER ((self graph-border-mixin) STREAM line-drawer)
  (with-slots (y-min y-max x-max tick-size y-tick-numbering) self
    (let* ((dtick (y-tick-spacing self))
           (tick-size (/ (* -1 tick-size) (x-scale self))))
      (draw-linear-axis self stream y-min y-max x-max :y dtick tick-size y-tick-numbering nil))))

(defmethod display :after ((self graph-border-mixin) STREAM)
  (display-border self STREAM)
  (display-labels self stream))


(defclass GRAPH-GRID-MIXIN (basic-graph) 
  ((show-grid :initform nil :initarg :show-grid)   ; Show grid?
   (x-auto-grid :initform t :initarg :x-auto-grid) ; Auto-space grid?
   (x-dgrid :initform nil :initarg :x-dgrid :accessor x-dgrid)       ; Spacing if not
   (y-auto-grid :initform t :initarg :y-auto-grid)
   (y-dgrid :initform nil :initarg :y-dgrid :accessor y-dgrid))
  (:documentation "Simple Grid overlay."))

(defmethod (SETF X-DGRID) :after (ignore (self graph-grid-mixin))
    (declare (ignore ignore))
    (setf (slot-value self 'x-auto-grid) nil))

(defmethod (setf Y-DGRID) :after (ignore (self graph-grid-mixin))
    (declare (ignore ignore))
    (setf (slot-value self 'y-auto-grid) nil))

(defmethod ink-for-grid ((self graph-grid-mixin) stream)
  (declare (ignore stream))
  ;; a dark gray.
  (clim:make-rgb-color .4 .4 .4))

(defun LINEAR-GRAPH-GRID
    (graph
     stream
     major-min major-max
     minor-min minor-max
     direction ;:x :y
     dgrid)
  (let* ((first-tick (+ (down major-min dgrid) dgrid))
         (ink (ink-for-grid graph stream)))
    (loop for tick from first-tick below major-max by dgrid do
         (with-xy-coordinates (graph stream)
           (if (equal direction :y)
               (draw-line* stream minor-min tick minor-max tick :ink ink)
               (draw-line* stream tick minor-min tick minor-max :ink ink))))))

(defun auto-grid (xmin xmax)
  ;; use default values for ticks as defaults for grids
  (auto-tick xmin xmax))

(defmethod DISPLAY-HORIZONTAL-GRID ((self graph-grid-mixin) STREAM)
  (with-slots (y-min y-max y-dgrid x-min x-max y-auto-grid) self
    (let ((dgrid (cond (y-auto-grid (auto-grid y-min y-max))
                       (t y-dgrid))))
      (linear-graph-grid self STREAM y-min y-max x-min x-max :y dgrid)
      (with-xy-coordinates (self stream)
        (draw-rectangle* stream x-min y-min x-max y-max :ink +blue+ :filled '())))))

(defmethod DISPLAY-VERTICAL-GRID ((self graph-grid-mixin) STREAM)
  (with-slots (y-min y-max x-min  x-max x-dgrid  x-auto-grid) self
    (let ((dgrid (cond (x-auto-grid (auto-grid x-min x-max))
                       (t x-dgrid))))
      (linear-graph-grid self STREAM x-min x-max y-min y-max :x dgrid))))

(defmethod DISPLAY-GRID ((self graph-grid-mixin) STREAM)
  (with-slots (show-grid) self
    (when show-grid
      (display-vertical-grid self STREAM)
      (display-horizontal-grid self STREAM))))

(defmethod DISPLAY :before ((self graph-grid-mixin) STREAM)
  (display-grid self STREAM))


(defclass GRAPH-DATASETS-MIXIN (basic-graph)
  ((datasets :initform nil :initarg :datasets :reader datasets)
   (hidden-datasets :initform nil :initarg :hidden-datasets :accessor hidden-datasets))
  (:documentation 
   "Allows several sets of data to be displayed on the graph, each in its own way."))

(defclass GRAPH-DATASETS-OB-MIXIN (graph-datasets-mixin)
  ()
  (:documentation 
   "Data set in pop-accept methods."))


(defmethod (setf hidden-datasets) :after ((new t) (graph GRAPH-DATASETS-MIXIN))
  (setf (auto-scale-needed graph) t))

(defmethod (setf datasets) (new-datasets (self graph-datasets-mixin))
  (with-slots (datasets) self
    (setq datasets nil)
    (dolist (dataset new-datasets)
      (add-dataset self dataset))))

(defmethod initialize-instance :after ((self graph-datasets-mixin)
				       &key &allow-other-keys)
  (with-slots (datasets) self
    (when datasets (setf (datasets self) datasets))))

(defmethod display :before ((self graph-datasets-mixin) STREAM)
  (let* ((datasets (datasets self)) 
         (used-color (map 'list #'ink datasets))
         (colors (set-difference *colors* used-color)))
    (loop for dataset in datasets do
         (unless colors (setf colors *colors*))
         (setf colors (remove (auto-set-dataset-color dataset colors) colors)))))

(defmethod display :after ((self graph-datasets-mixin) STREAM)
  (graph-display-data self STREAM))

(defmethod rescale :after ((self graph-datasets-mixin))
  (dolist (d (datasets self)) (rescale d)))

(defmethod add-dataset ((self graph-datasets-mixin) dataset)
  (with-slots (datasets) self
    (when (not (member dataset datasets :test #'eq))
      (if datasets (setf (cdr (last datasets)) (cons dataset nil))
	  (setq datasets (cons dataset nil))))))

;;; KRA: This used to take name-or-dataset, now it just takes dataset.
(defmethod remove-dataset ((self graph-datasets-mixin) dataset)
  (with-slots (hidden-datasets datasets) self
    (setq datasets (delete dataset datasets :test #'eq))
    (setq hidden-datasets (delete dataset hidden-datasets :test #'eq))))

(defmethod graph-display-data :around ((self graph-datasets-mixin) STREAM)
  (with-clipping-to-graph (self STREAM t)
    (call-next-method self STREAM)))

(defmethod graph-display-data ((self graph-datasets-mixin) STREAM)
  (let ((hidden (hidden-datasets self)))
    (dolist (set (datasets self))
      (or (member set hidden) (display-data set STREAM self)))
    (force-output stream)))

(defmethod x-label :around ((self graph-datasets-mixin))
  (or (call-next-method)
      (some #'x-label (datasets self))))

(defmethod y-label :around ((self graph-datasets-mixin))
  (or (call-next-method)
      (some #'y-label (datasets self))))

;;;NLC08NOV90 - The g8>graph> version returns a string or nil. Whereas this
;;;		returns a string or " ".
(defmethod title :around ((self graph-datasets-mixin))
  (let ((the-title nil))
    (setq the-title (or (call-next-method)
			(some #'title (datasets self))
			(name-string self)))
    (when the-title
      (unless (stringp the-title)
	(setq the-title (format nil "~a" the-title))))
    (or the-title " ")))


(defclass GRAPH-AUTO-SCALE-MIXIN (graph-datasets-mixin basic-graph) 
  ((auto-scale-needed :initform nil	; Data changed since last auto-scale?
		      :initarg :auto-scale-needed
		      :accessor auto-scale-needed)
   (auto-scale :initform :both		; Auto scale: (choose :x :y :both nil)
	       :initarg :auto-scale
	       :accessor auto-scale))
  (:documentation
   "Allows the axes of a graph to be automatically scaled from its datasets."))

(defmethod add-dataset :after ((self graph-auto-scale-mixin) ignore)
  (declare (ignore ignore))
  (with-slots (auto-scale-needed) self
    (setq auto-scale-needed t)))

(defmethod remove-dataset :after ((self graph-auto-scale-mixin) ignore)
  (declare (ignore ignore))
  (with-slots (auto-scale-needed) self
    (setq auto-scale-needed t)))

;;; Never auto-scale if user changes axis definitions.
(defmethod (setf x-max) :after ((new t) (self graph-auto-scale-mixin))
  (setf (auto-scale self)
	(case (auto-scale self) (:both :y) (:x nil) (otherwise (auto-scale self)))))

(defmethod (setf x-min) :after ((new t) (self graph-auto-scale-mixin))
  (setf (auto-scale self)
	(case (auto-scale self) (:both :y) (:x nil) (otherwise (auto-scale self)))))

(defmethod (setf y-min) :after ((new t) (self graph-auto-scale-mixin))
  (setf (auto-scale self)
	(case (auto-scale self) (:both :x) (:y nil) (otherwise (auto-scale self)))))

(defmethod (setf y-max) :after ((new t) (self graph-auto-scale-mixin))
  (setf (auto-scale self)
	(case (auto-scale self) (:both :x) (:y nil) (otherwise (auto-scale self)))))

(defmethod display :before ((self graph-auto-scale-mixin) STREAM)
  (declare (ignore stream))
  (with-slots (auto-scale auto-scale-needed) self
    (and auto-scale auto-scale-needed (do-auto-scale self))))

(defmethod (setf auto-scale) ((self graph-auto-scale-mixin) type)
  ;; Tells graph to do auto-scaling before graph is redisplayed.  Type may be:
  ;; :x      Auto scale the X axis.
  ;; :y      Auto scale the Y axis.
  ;; :both   Auto scale both axes
  ;; nil     No auto scaling.
  (if (not (member type '(:x :y :both nil)))
      (error "~&(auto-scale graph-datasets-mixin): ~a is invalid option" type))
  (with-slots (auto-scale auto-scale-needed) self
    (setq auto-scale-needed t)
    (setq auto-scale type)  ;;;NLC08NOV90 - This should return the value of AUTO-SCALE.
    ))

(defmethod graph-auto-scale-limits ((self graph-auto-scale-mixin))
  "Returns limits needed to show all datasets."
  (with-slots (datasets hidden-datasets x-min x-max y-min y-max auto-scale) self
    (loop with xmin and xmax and ymin and ymax
	for dataset in datasets
	as limits = (unless (member dataset hidden-datasets)
		      (auto-scale-limits dataset auto-scale
					 x-min x-max y-min y-max))
	when limits
	do (multiple-value-bind (left right bottom top)
	       (apply #'values limits)
	     (if (or (null xmin) (and left   (< left xmin)))
		 (setq xmin left))
	     (if (or (null xmax) (and right  (> right xmax)))
		 (setq xmax right))
	     (if (or (null ymin) (and bottom (< bottom ymin)))
		 (setq ymin bottom))
	     (if (or (null ymax) (and top    (> top ymax)))
		 (setq ymax top)))
	finally (return (values (or xmin 0) (or xmax 1)
				(or ymin 0) (or ymax 1))))))

(defmethod do-auto-scale :around ((self graph-auto-scale-mixin))
  ;; Only actually do auto scaling if you need to and can.
  (with-slots (auto-scale-needed datasets auto-scale) self
    (when (and auto-scale-needed datasets auto-scale)
      (call-next-method self)
      (rescale self)
      (setq auto-scale-needed nil))))

(defmethod do-auto-scale ((self graph-auto-scale-mixin))
  "Actually do the auto scaling."
  (with-slots (auto-scale x-min x-max y-min y-max) self
    (multiple-value-bind (xmin xmax ymin ymax)
	(graph-auto-scale-limits self)
      (when (member auto-scale '(:x :both) :test #'eq)
	(if xmin (setq x-min xmin))
	(if xmax (setq x-max xmax)))
      (when (member auto-scale '(:y :both) :test #'eq)
	(if ymin (setq y-min ymin))
	(if ymax (setq y-max ymax))))
    (when (= x-min x-max)			; Degenerate limits. set limits
      (decf x-min 1.0)			;  so data will be plotted.
      (incf x-max 1.0))
    (when (= y-min y-max)
      (decf y-min 1.0)
      (incf y-max 1.0))))
 

(defclass graph-limits-mixin (graph-auto-scale-mixin)
  ()
  (:documentation
    "Allows a graph or a dataset to restrict the limits
      of the graph after auto scaling."))

(defmethod limit-specs ((self graph-limits-mixin))
  "Returns a list of limits-specs that specify how the auto scaled limit of the
  LEFT, RIGHT, BOTTOM, and TOP (respectively) of the graph are restricted.
  A limit-spec can have the form:

  (min max) - (<= min limit max)
  (nil max) - (<= limit max)
  (min nil) - (<= min limit)
  number    - (= limit number)
  nil       - limit is unconstrained.

  If the list is (), the graph limits are unrestricted."
  ())

(defun limit-value (value limit)
  (cond ((null limit) value)
	((listp limit)
	 (when (first limit)
	   (setq value (max (first limit) value)))
	 (when (second limit)
	   (setq value (min (second limit) value)))
	 value)
	(t limit)))

(defmethod graph-auto-scale-limits :around ((self graph-limits-mixin))
  "Constrain graph edges to be within limits."
  (let ((the-limits (limit-specs self)))
    (multiple-value-bind (xmin xmax ymin ymax)
	(call-next-method)
      (when the-limits
	(multiple-value-bind (left right bottom top)
	    (apply #'values the-limits)
	  (when xmin (setq xmin (limit-value xmin left)))
	  (when xmax (setq xmax (limit-value xmax right)))
	  (when ymin (setq ymin (limit-value ymin bottom)))
	  (when ymax (setq ymax (limit-value ymax top)))))
      (values xmin xmax ymin ymax))))


(defclass GRAPH-AUTO-SCALE-EXTENSIONS-MIXIN (graph-auto-scale-mixin)
  ((auto-scale-extensions		; list of (left right bottom top) %
    :initform (list 5.0 5.0 5.0 5.0)	;of X or Y axis to extend when auto
    :initarg :auto-scale-extensions)	;scaling.
   ))

(defmethod graph-auto-scale-limits :around ((self graph-auto-scale-extensions-mixin))
  "Extend limits needed by the data."
  (multiple-value-bind (xmin xmax ymin ymax)
      (call-next-method)
    (with-slots (auto-scale-extensions) self
      (when auto-scale-extensions
      (multiple-value-bind (left right bottom top)
	  (apply #'values auto-scale-extensions)
	(when (and xmin xmax)
	  (let ((range (abs (- xmax xmin))))
	    (when xmin (setq xmin (- xmin (* 0.01 left   range))))
	    (when xmax (setq xmax (+ xmax (* 0.01 right  range))))))
	(when (and ymin ymax)
	  (let ((range (abs (- ymax ymin))))
	    (when ymin (setq ymin (- ymin (* 0.01 bottom range))))
	    (when ymax (setq ymax (+ ymax (* 0.01 top    range))))))))
      (values xmin xmax ymin ymax))))




;;; Clim 1.0 requires this be defined before any presentation type
;;; that depends on it.  Hence moved here from present.lisp.

(defclass presentable-graph-mixin ()
	  ((presentation :initform nil :accessor presentation)
	   (tick :initform 0 :accessor redisplay-tick)))

(defmacro with-temporary-cursor-position ((stream x y) &body body)
  `(with-output-truncation (,stream)
     (multiple-value-bind (.x. .y.) (stream-cursor-position ,stream)
       (unwind-protect
            (progn (setf (stream-cursor-position ,stream) (values ,x ,y)) ,@body)
         (setf (stream-cursor-position ,stream) (values .x. .y.))))))

(defmethod display :around ((self presentable-graph-mixin) STREAM)
  "Display the graph as a presentation."
  ;; Lessons learned.
  ;; 1. Don't do ERASE type operations inside of with-output-as-presentation.
  ;;     DISPLAY generates output; removing it should be done elsewhere.
  ;; 2. Enable output truncation to prevent unwanted viewport scrolling.
  ;; 3. Move the cursor to within the bounds of the graph, since cursor position
  ;;     affects mouse-sensitive area (clim 0.9 bug).
  (with-output-truncation (stream)
    (setf (presentation self)
      (with-redisplayable-output (:stream stream 
					  :unique-id self
					  :cache-value (redisplay-tick self)
					  :cache-test #'=)
        (multiple-value-bind (x0 y0 x1 y1) (rectangle-edges* (outside-box self))
	  (declare (ignore x1 y1))
	  (with-temporary-cursor-position (stream x0 y0)
	    (with-output-as-presentation
		(STREAM self (graph-presentation-type self self)
			 :single-box t
			 :allow-sensitive-inferiors 
			 (graph-present-inferiors-p self))
          (call-next-method self STREAM)))))))
  (force-output stream))

(defun incrementally-redisplayable-presentation (presentation)
  "Determine if a presentation is a part of an incremental redisplay."
  (if (not presentation) nil
    (or 
     (typep presentation 'standard-updating-output-record)
     (incrementally-redisplayable-presentation
      (clim:output-record-parent presentation)))))

(defmethod erase ((self presentable-graph-mixin) stream)
  (with-output-truncation (stream)
    (let ((presentation (presentation self)))
      (when presentation
        (clim:erase-output-record presentation stream nil)
        (setq presentation nil)))))

(defmethod refresh :around ((self presentable-graph-mixin) stream)
  "By default, graphs refresh by erasing and then drawing.
   This breaks incremental redisplay, so watch out!"
  (let ((p (presentation self)))
    (cond ((and p (incrementally-redisplayable-presentation 
		   (clim:output-record-parent p)))
	   (incf (redisplay-tick self))
	   ;; Do nothing.  Expect redisplay-frame-panes to do the rest.
	   nil)
	  (t (call-next-method self stream)))))

(defmethod graph-presentation-type ((self presentable-graph-mixin) graph)
  (declare (ignore self graph))
  'graph)
(defmethod graph-present-inferiors-p ((self presentable-graph-mixin)) 't)
(defmethod present-self-p ((any t)) nil)
