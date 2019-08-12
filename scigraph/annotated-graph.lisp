;; -*- Syntax: Common-lisp; Package: GRAPH -*-
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

;;;ANNOTATED-GRAPH-MIXIN provides a graph with the ability to be annotated.
;;;   1) annotation           (put some arbitrary text on the graph)
;;;   2) point annotation     (draw a line from the text to some point on the graph)
;;;   3) region annotation    (draw a line from the text to some region of the graph)
;;;
;;;ANNOTATED-BORDERS-MIXIN turns the axis labels and the title of a graph into
;;;  "border" annotations.  A border annotation is like an ordinary annotation except
;;;  that it does the right thing when you zoom or rescale.

;;; The value of this depends on the lisp implementation.
(defconstant *return* #.(elt (format nil "~%") 0))

(define-presentation-type annotation ()
  :description "an annotation")

(define-presentation-method present (object (type annotation) stream (view textual-view) &key)
  (flet ((shorten-string
		     (string &optional (longest 15))
		     (let ((str (substitute #\space *return* string)))
		       (if (> (length str) longest)
			   (format nil "~A..." (subseq str 0 (1- longest)))
			   str))))
	      (write-string (shorten-string (annotation-text object)) stream)))

(define-presentation-method accept ((type annotation) stream (view textual-view) &key)
  (read-char stream)
  (error "You must select an annotation with the mouse."))

(define-presentation-to-command-translator
  com-move-annotation
  (annotation :command-name com-move-object
	      :command-table :graph
	      :gesture :select
	      :menu t :documentation "Move")
  (object &key WINDOW)
  `(,object ,WINDOW))

(define-graph-command com-delete-annotation ((object 'annotation) (window 'sheet))
  "Removes an annotation from a graph."
  (kill object window))

(define-presentation-to-command-translator com-delete-annotation
   (annotation :command-name com-delete-annotation
	       :command-table :graph
	       :gesture nil :documentation "Delete")
   (object &key window)
  (list object window))

(define-graph-command com-change-annotation-style ((object 'annotation) (window 'sheet))
  "Set the text style (or font) of a given annotation."
  (let ((style (choose-character-style)))
     (when style
       (erase object window)
       (setf (style object) style)
       (display object window))))

(define-presentation-to-command-translator com-change-annotation-style 
   (annotation :command-name com-change-annotation-style
	       :command-table :graph
	       :gesture nil :documentation "Change Text Style")
   (object &key window)
  (list object window))

(define-graph-command com-edit-annotation ((object 'annotation) (window 'sheet))
  "Edit the text of the annotation."
   (edit object window))

(define-presentation-to-command-translator com-edit-annotation
   (annotation :command-name com-edit-annotation 
	       :gesture nil
	       :command-table :graph
	       :tester ((object) (editable object))
	       :documentation "Edit Annotation Text")
   (object &key WINDOW)
  (list object window))

(defclass ANNOTATED-GRAPH-MIXIN (essential-graph-margin-mixin) 
    ((annotations :initform () :initarg :annotations :accessor annotations)))

(defmethod annotated-graph-p ((object t)) nil)
(defmethod annotated-graph-p ((object annotated-graph-mixin)) t)

(defun annotate-something (graph STREAM)
  (let ((choice
	 (menu-choose
	  `(("Plain Text" :value border-annotation
                      :documentation "Add your own text to this graph")
	    ("Plain Text + Pointer" :value point-annotation
	     :documentation "Text with a pointer to a point"))
	  :label "Choose Type of Annotation"
      :associated-window *standard-input*)))
    (when choice (annotate graph STREAM choice))))

(define-graph-command com-annotations-menu ((object 'graph) (WINDOW 'sheet))
  "Create an annotation and prompt the user for the annotation text."
  (annotate-something object WINDOW))

(define-presentation-to-command-translator
  com-annotations-menu
  (graph :command-name com-annotations-menu
	 :command-table :graph
	 :documentation "Add Free Text..."
	 :gesture nil 
	 :tester ((object) (annotated-graph-p object)))
  (object &key WINDOW)
  (list object window))

(define-graph-command com-graph-identify-point ((graph 'graph) (window 'sheet))
  "Prompt the user to select a data point, and annotate it."
  (let* ((datasets (datasets graph))
	 (dataset (if (cdr datasets)
		      (menu-choose (mapcar #'(lambda (d) `(,(name d) :value ,d))
					   datasets)
                           :label "Choose a Dataset"
                           :associated-window *standard-input*)	 
		    (car datasets))))
    (when dataset
      (annotate-data-point dataset window graph))))

(define-presentation-to-command-translator
  com-graph-identify-point
  (graph :command-name com-graph-identify-point
	 :command-table :graph
	 :documentation "Identify Data Point..."
	 :gesture nil 
	 :tester ((object) (annotated-graph-p object)))
  (object &key WINDOW)
  (list object window))

(define-graph-command com-graph-identify-region ((graph 'graph) (window 'sheet))
  "Prompt the user to select a data region, and annotate the points
   contained in the region with descriptive statistics."
  (let* ((datasets (datasets graph))
	 (dataset (if (cdr datasets)
		      (menu-choose (mapcar #'(lambda (d) `(,(name d) :value ,d))
					   datasets)
                           :label "Choose a Dataset:"
                           :associated-window *standard-input*)
		    (car datasets))))
    (when dataset
      (annotate-data-region dataset graph window))))

(define-presentation-to-command-translator
  com-graph-identify-region
  (graph :command-name com-graph-identify-region
	 :command-table :graph
	 :documentation "Identify Data Region..."
	 :gesture nil 
	 :tester ((object) (annotated-graph-p object)))
  (object &key WINDOW)
  (list object window))

(defmethod rescale-annotation progn ((self annotated-graph-mixin))
  (dolist (a (annotations self)) (rescale-annotation a)))

(defmethod rescale :after ((self annotated-graph-mixin))
  (rescale-annotation self))

(defmethod display-annotations ((self annotated-graph-mixin) stream)
  (dolist (annotation (annotations self)) (display annotation stream)))

(defmethod display :after ((self annotated-graph-mixin) stream)
  (display-annotations self stream))

(defmethod erase :before ((self annotated-graph-mixin) stream)
  ;; After an annotation has been moved, it is no longer a part of the graph
  ;; presentation tree, it becomes a separate presentation.
  ;; So we have to take care of the annotations explicitly.
  (dolist (a (annotations self)) (erase a stream)))

(defmethod annotate ((self annotated-graph-mixin) GRAPH-WINDOW type)
  "Create an annotation and prompt the user for the text."
  ;; User interface provided by annotated-graph-mixin
  (let ((annotation (make-instance type :graph self)))
    (when
	(create annotation GRAPH-WINDOW) ; User can abort create with mouse-middle.
      (pushnew annotation (annotations self)))))


;;; BORDER-ANNOTATION
;;;
;;; Annotation with internal slot r s and solidal to r-s coordinate system

(defclass BORDER-ANNOTATION-MIXIN ()
  ()
  (:documentation "Annotation with internal slot x y solidal to r-s coordinate system"))

(defmethod rs-position ((self border-annotation-mixin))
  (with-slots (x y) self (values x y)))

(defmethod set-rs-position ((self border-annotation-mixin) r s &optional constrain-p)
  (declare (ignore constrain-p))
  (with-slots (x y) self (setq x r y s)))

(defmethod xy-position ((self border-annotation-mixin))
  (multiple-value-bind (r s) (rs-position self)
    (rs-to-xy (graph self) r s)))

(defmethod set-xy-position ((self border-annotation-mixin) newx newy &optional constrain-p)
  (declare (ignore constrain-p))
  (multiple-value-setq (r s) (rs-to-xy (graph self) newx newy))
  (set-rs-position self r s))

(defmethod set-stream-position ((self border-annotation-mixin) stream x y)
  (multiple-value-setq (r s) (stream-to-rs (graph self) x y))
  (set-rs-position self r s))

(defmethod stream-position ((self border-annotation-mixin) stream)
  (multiple-value-bind (r s) (rs-position self)
    (rs-to-stream (graph self) r s)))

;;; Border annotations never get clipped.
(defmethod display-p ((self border-annotation-mixin)) t)

(defmethod annotation-single-box ((self border-annotation-mixin)) t)


(defclass BORDER-ANNOTATION (border-annotation-mixin annotation) ())

(defun MAKE-BORDER-ANNOTATION (graph STREAM text r s &optional
			       (type 'border-annotation)
			       (angle 0) (display t))
  "Noninteractively add an annotation."
  (let ((annotation (make-instance type
				   :angle angle                   
				   :graph graph)))
    (set-text annotation STREAM text)
    (setf (style annotation) (default-text-style graph stream))
    (set-rs-position annotation r s)
    (setf (annotations graph) (cons annotation (annotations graph)))
    (if display (display annotation stream))
    annotation))

;;; The classes X-LABEL, Y-LABEL, and TITLE are all types of border annotations that
;;; are coupled to the corresponding strings stored on the graph.  These methods
;;; solve the problem that the string is stored in two places.

(defclass x-label (border-annotation) ()
  )

(defmethod display :before ((self x-label) (stream t))
  (setf (annotation-text self) (x-label (graph self))))

(defmethod set-text :after ((self x-label) stream string)
  (declare (ignore stream))
  (let ((graph (graph self)))
    (when graph (setf (x-label graph) string))))

(defclass y-label (border-annotation) ()
  )

(defmethod display :before ((self y-label) (stream t))
  (setf (annotation-text self) (y-label (graph self))))

(defmethod set-text :after ((self y-label) stream string)
  (declare (ignore stream))
  (let ((graph (graph self)))
    (when graph (setf (y-label graph) string))))

(defclass title (border-annotation) ()
  )

(defmethod display :before ((self title) (stream t))
  (setf (annotation-text self) (title (graph self))))

(defmethod set-text :after ((self title) stream string)
  (declare (ignore stream))
  (let ((graph (graph self)))
    (when graph (setf (title graph) string))))


(defclass ANNOTATED-BORDERS-MIXIN (annotated-graph-mixin graph-border-mixin basic-graph)
    ((x-annotation :initform nil :initarg :x-annotation
		   :accessor x-annotation)
     (y-annotation :initform nil :initarg :y-annotation
		   :accessor y-annotation)
     (title-annotation :initform nil :initarg :title-annotation
		       :accessor title-annotation))
  (:documentation "A mixin for graphs that turns titles and labels into annotations."))

(defmethod x-label-text-style ((graph ANNOTATED-BORDERS-MIXIN) stream)
  (default-text-style graph stream))
  
(defmethod y-label-text-style ((graph ANNOTATED-BORDERS-MIXIN) stream)
  (default-text-style graph stream))
  
(defmethod title-text-style ((graph ANNOTATED-BORDERS-MIXIN) stream)
  (default-text-style graph stream))

(defmethod compute-x-annotation ((self annotated-borders-mixin) STREAM)
  (with-slots (x-annotation width height x-label) self
    (when x-label
      (when (not x-annotation)
        (LET* ((rmid (/ width 2))
               (annotation (make-border-annotation self STREAM x-label
                                                   rmid 0 'x-label 0 nil))
               (width nil))
          (setf (style annotation) (x-label-text-style self stream))
          (setq width (width annotation))
          (set-rs-position annotation
                           (- rmid
                              (/ width 2))
                           (+ height (* (stream-line-height stream) 2)))
          (setq x-annotation annotation))))))

(defmethod display-x-label ((self annotated-borders-mixin) STREAM)
  (compute-x-annotation self STREAM))


(defmethod compute-y-annotation ((self annotated-borders-mixin) STREAM)
  (with-slots (y-annotation width height y-label y-digits) self
    (when y-label
      (when (not y-annotation)
        (let* ((smid (/ height 2))
               (annotation (make-border-annotation
                            self STREAM y-label
                            0 smid
                            'y-label #.(/ pi -2) nil))
               (height nil))
	  (setf (style annotation) (y-label-text-style self stream))
	  (setq height (* (length y-label) (stream-line-height stream)))
	  (set-rs-position annotation
	    	   (- 0 (* (stream-character-width stream)
	    		     (+ y-digits 1)))
	    	   (+ smid
	    	      (truncate height 2)))
	  (setq y-annotation annotation))))))

(defmethod display-y-label ((self annotated-borders-mixin) STREAM)
  (compute-y-annotation self STREAM))

(defmethod compute-title-annotation ((self annotated-borders-mixin) STREAM)
  (with-slots (title-annotation width) self
    (when (title self)
      (when (not title-annotation)        
        (let* ((rmid (/ width 2))
               (annotation (make-border-annotation self STREAM (title self) 
                                                   rmid 0 'title
                                                   0 nil))
               (width nil)
               (height nil))
          (setf (style annotation) (title-text-style self stream))
          (setq width (width annotation))
          (setq height (height annotation))
          (set-rs-position annotation
                           (- rmid (/ width 2))
                           (- 0 height))
          (setq title-annotation annotation))))))

(defmethod display-title ((self annotated-borders-mixin) STREAM)
  (compute-title-annotation self STREAM))


(defclass legend-annotation (border-annotation-mixin basic-annotation)
    ((margin :initform 10 :initarg :margin :accessor margin)
     (width :initform 0 :initarg :width :accessor width)
     (height :initform 0 :initarg :height :accessor height))
  (:documentation "An annotation displaying an iconic description of each dataset."))

(defmethod annotation-text ((self legend-annotation))
  ;; Called by PRESENT method for annotations.
  ;; This text may appear in click-right menus.
  "Dataset Legend")

(defmethod draw-outline ((self legend-annotation) stream ink)
  "Draw a rectangle marking the edges of the annotation."
  (let ((fudge (margin self)))
    (multiple-value-bind (width height) 
        (legend-size (graph self) stream (style self))
      (setf (width self) width)
      (setf (height self) height)
      (multiple-value-bind (left top) (rs-position self)
        (let ((right (+ left width))
              (bottom (+ top height)))
          (decf left fudge)
          (incf right fudge)
          (decf top fudge)
          (incf bottom fudge)
      (with-rs-coordinates ((graph self) stream)
        (draw-rectangle* stream left top right bottom :ink ink :filled nil)))))))

(defmethod display ((self legend-annotation) stream)
  (let* ((line-height (truncate (stream-line-height stream)))
         (ink (ink self))
         (graph (graph self))
         (datasets (datasets graph))
         (hidden (hidden-datasets graph)))
    (when (show-graph-legend graph)
      (multiple-value-bind (width height)
          (legend-size graph stream (style self))
        (setf (width self) width)
        (setf (height self) height)
        (setq height (values (/ height 
                                       (max 1 
                                            (- (length datasets) 
                                               (length hidden))))))
        (multiple-value-bind (left top) (rs-position self)
          (draw-outline self stream ink)
          (dolist (dataset datasets)
            (unless (or (member dataset hidden)
                        (not (show-legend dataset)))
              (display-legend-dataset dataset STREAM graph
                                      left top width height)
              (incf top line-height))))))))

(defmethod mark ((self legend-annotation) stream)
  "Draw a rectangle marking the edges of the annotation."
  (draw-outline self stream +flipping-ink+))

(defmethod kill :before ((self legend-annotation) stream)
  (declare (ignore stream))
  ;; make sure the legend doesnt come back if you kill it.
  (setf (show-legend (graph self)) nil))

(defclass annotated-legend-mixin () ()
  (:documentation
   "Convert the legend of the graph into a movable annotation.
    Place the legend inside the graph, but take care to place it
    where it won't obscure any data."))

(defvar *legend-positions*		; relative coordinates
  '((0.0 0.25)				; lower left
    (0.75 1.0)				; upper right
    (0.0 1.0)				; upper left
    (0.75 0.25)				; lower right
    (0.25 1.0)
    (0.5 1.0)
    (0.25 0.25)
    (0.5 0.25)
    (0.0 0.5)
    (0.0 0.75)
    (0.75 0.5)
    (0.75 0.75)
    (0.25 0.5)
    (0.25 0.75)
    (0.5 0.5)
    (0.5 0.75)))

(defun legend-positions (width height left top right bottom)
  "Make a list of xy positions where we might consider putting the legend."
  (let ((relative *legend-positions*)
	(dx (- right left))
	(dy (- top bottom))
        (xy nil))
    (dolist (p relative)
      (let ((x (+ left (* (first p) dx)))
	    (y (+ bottom (* (second p) dy))))
	(setq x (min x (- right width)))
	(setq y (max y (+ bottom height)))
	(push (list x y) xy)))
    (nreverse xy)))

(defun count-points-in-xy-rectangle (graph left top right bottom)
  (let ((count 0))
    (dolist (dataset (datasets graph))
      (map-data-xy dataset
		   #'(lambda (x y)
		       (when (and (<= left x right)
				  (<= bottom y top))
			 (incf count)))
		   (data dataset)))
    count))

(defmethod default-annotation-position (graph &optional (width 20) (height 20))
  "Find a place (RS) on the graph where the annotation won't obscure any data."
  (setf width (/ width (x-scale graph)) height (/ height (y-scale graph)))
  (with-slots (x-min y-min x-max y-max) graph
    (let* ((positions (legend-positions width height x-min y-max x-max y-min))
           smallest choice)
      (dolist (position positions)
        (let* ((left (car position))
               (top (second position))
               (right (+ left width))
               (bottom (- top height))
               (count (count-points-in-xy-rectangle graph left top right bottom)))
          (if (or (not smallest) (< count smallest))
              (setq smallest count choice position))))
      (apply #'xy-to-rs graph choice))))

(defmethod legend-text-style ((self annotated-legend-mixin) (stream t))
  (merge-text-styles (parse-text-style '(nil :roman :normal))
		     (medium-text-style stream)))

(defmethod create-legend ((self annotated-legend-mixin) stream)
  "Make a legend annotation and position it."
  (let* ((legend (make-instance 'legend-annotation :graph self))
         (fudge nil))
    (setf (style legend) (legend-text-style self stream))
    (setq fudge (margin legend))
    (push legend (annotations self))
    (multiple-value-bind (width height) (legend-size self stream (style legend))
      (multiple-value-bind (le te)
          (default-annotation-position self
              (+ width (* 4 fudge)) (+ height (* 4 fudge)))
	(set-rs-position legend (+ le (* 2 fudge)) (+ te (* 2 fudge)))))
    legend))

(defmethod legend-exists-p ((self annotated-legend-mixin))
  (dolist (ann (annotations self))
    (if (typep ann 'legend-annotation) (return-from legend-exists-p t))) ;;;NLC
  nil)

(defmethod display-annotations :before ((self annotated-legend-mixin) stream)
  (when (and (show-graph-legend self) (not (legend-exists-p self)))
    (create-legend self stream)))

(defmethod display-legend ((self annotated-legend-mixin) stream)
  ;; Cancel the usual way of displaying a legend.  Now the legend is an annotation,
  ;; therefore display is taken care of separately.
  (declare (ignore stream))
  nil)

(defmethod legend-compute-margins ((self annotated-legend-mixin) STREAM left top right bottom)
  ;; Don't use up valuable real-estate.  Stay out of the margins.
  (declare (ignore stream))
  (values left top right bottom))


(defclass ANNOTATED-GRAPH
    (annotated-legend-mixin
     annotated-borders-mixin
     annotated-graph-mixin
     graph)
  ())
