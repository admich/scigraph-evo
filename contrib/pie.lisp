(in-package :graph)

;;;; pie chart

(export '(pie-graph pie-graph-datum))

(defclass pie-graph-mixin () ()
  (:default-initargs :visible-borders '() :x-min -2 :x-max 2 :y-min -2 :y-max 2 :auto-scale nil))

(defclass pie-graph (pie-graph-mixin
                      annotated-graph)
  ())

(defclass pie-graph-datum ()
  ((value :initarg :value :accessor datum-value)))

(clim:define-presentation-type pie-graph-datum ())

(defmethod datum-presentation-type ((self presentable-data-mixin) (datum pie-graph-datum))
  (declare (ignore self datum))
  'pie-graph-datum)

(defclass circular-sector (standard-sequence-output-record) ())

(defmethod output-record-refined-position-test ((record circular-sector) x y)
  (let ((ellipse-record (elt (output-record-children record) 0)))
    (with-slots ((start-angle climi::start-angle)
                 (end-angle climi::end-angle)
                 (center-x climi::center-x)
                 (center-y climi::center-y)) ellipse-record
      (let ((dx (- x center-x))
            (dy (- center-y y)))
        (if (and  (< dx 0) (> dy 0))
            (< start-angle (- (atan dy dx) (* 2 pi)) end-angle)
            (< start-angle (atan dy dx) end-angle))))))

(define-presentation-method highlight-presentation ((type pie-graph-datum) record stream state)
  (let ((record (elt (output-record-children record) 0)))
    (highlight-output-record  record stream state)))

(defmethod highlight-output-record ((record circular-sector) stream (state (eql :highlight))) 
  (let* ((ellipse-record (elt (output-record-children record) 0))
         (ink (displayed-output-record-ink ellipse-record)))
    (multiple-value-bind (i h s) (color-ihs ink)
      (let ((new-ink (make-ihs-color i h (* 0.4 s))))
        (with-slots ((start-angle climi::start-angle)
                 (end-angle climi::end-angle)
                 (radius climi::radius-1-dx)
                 (center-x climi::center-x)
                 (center-y climi::center-y)) ellipse-record
          (draw-circle* stream center-x center-y radius :start-angle start-angle :end-angle end-angle :ink new-ink))))))

(defmethod graph-display-data ((self pie-graph-mixin) STREAM)
  (let ((last-angle (/ pi 2))
        (total (reduce #'+ (datasets self) :key (lambda (x) (data x))))
        (radius (* (x-scale self) 1)))
    (declare (special total radius last-angle))
    (dolist (dataset (datasets self))
      (display-data dataset stream self))))

(defmethod display-data ((self essential-graph-data-map-mixin)  stream (graph pie-graph-mixin))
  (with-ink (stream (ink self))
    (let ((displayer (datum-displayer self graph)))
      (declare (compiled-function displayer))
      (funcall displayer stream nil nil (make-instance 'pie-graph-datum :value (data self))))))

(defmethod datum-displayer ((self essential-graph-data-map-mixin) (graph pie-graph-mixin))
  "Returns a function that expects a stream, UV coordinates of next datum,
   and the datum itself."
  #'(lambda (stream u v datum)
      (multiple-value-bind (cx cy) (xy-to-rs graph 0 0)
        (let ((angle (* 2 pi (/ (datum-value datum) total))))
          (with-new-output-record (stream 'circular-sector)
            (draw-circle* stream cx cy radius :filled t :start-angle  (- last-angle angle) :end-angle last-angle))
          (setf last-angle (- last-angle angle))))))

(defmethod default-annotation-position ((graph pie-graph-mixin) &optional (width 20) (height 20))
  "Find a place (RS) on the graph where the annotation won't obscure any data."
  (xy-to-rs graph 1 2))

(defmethod display-legend-datums ((self graph-data-legend-mixin) STREAM
                                  (graph pie-graph-mixin) left top width height)
  "Display some points in the legend area to show the current symbology settings."
  (with-ink (stream (ink self))
    (draw-rectangle* stream left top (+ left width) (+ top height))))

(defmethod datum-position (dataset (datum pie-graph-datum))
  (values 0 0))

(defmethod nearest-datum ((graph pie-graph)  dataset r s)
  "Return the x,y datum nearest the given r,s coordinates."
  ;; Do this in uv coordinates, because what matters is what looks close on the
  ;; screen, not what seems close in x,y space.
  (values 0 0 (make-instance 'pie-graph-datum :value (data dataset))))

(define-presentation-to-command-translator com-identify-2
  (pie-graph-datum :command-name com-identify
	      :command-table :graph
	      :gesture :select
	      :menu t
	      :documentation "Identify Data Point"
	      :tester ((object &key presentation)
                   t
		       ;; (and (datum-presentation-p object presentation)
               ;;      (dataset-sensitive-p presentation))
                   )
          )
    (object &key presentation window)
  (list (dataset-under-presentation presentation) 
	(graph-under-presentation presentation)
	object window))

#|
----------------------------------------------------------------------
eliinare??
(defmethod datum-presentation-p ((object t) presentation)
  ;; Check that it's a part of a graph and not part of the legend.
  (log:error "b")
  (and (eql (presentation-type presentation) 'expression)
       (graph-under-presentation presentation)
       (not (graph-under-annotation-under-presentation presentation)))
  t)

(defun dataset-sensitive-p (presentation)
  (let ((dataset (dataset-under-presentation presentation)))
      (log:error "a" (and dataset (present-self-p dataset)))
    (and dataset (present-self-p dataset))))


;;; Clim 0.9 seems to be missing a bunch of presentation types,
;;; probably because AND and OR are missing.  Here we kludge up 
;;; a solution until CLIM gets better.
;;; CLIM IS BETTER NOW (CLIM 2.0.BETA).  LETS GET RID OF THIS.  JPM.
(define-presentation-type string-or-none ()
  :description "a string or None"
  :printer ((object stream)
	    (if (or (not object) (equal object ""))
		(write-string "None" stream)
	      (present object 'string :stream stream)))
  :parser ((stream)
	   (let ((string (accept 'string :stream stream :prompt nil :default nil)))
	     (setq string (string-trim '(#\space) string))
	     (if (or (string= string "") 
		     (string-equal string "None"))
		 (values nil 'string-or-none)
	       (values string 'string-or-none)))))

(define-presentation-type number-or-none ()
  :description "a number or None"
  :printer ((object stream)
	    (if object
		(present object 'number :stream stream)
		(write-string "None" stream)))
  :parser ((stream)
	   (let ((string (read-token stream)))
	     (if (string-equal (string-trim '(#\space) string) "None")
		 (values nil 'number-or-none)
		 (let ((number (read-from-string string)))
		   (if (numberp number)
		       (values number 'number-or-none)
		       (input-not-of-required-type stream string 'number-or-none)))))))

(defun input-not-of-required-type (stream object type)
  "Use this to signal a parser failure and cause backtracking."
  (declare (ignore stream))
  ;; Used by faes expression editor.  Don't use the one from clim or dw,
  ;; it's so fancy that it outsmarts itself.
  (when *%%ready-to-catch%%*
      (throw 'catch-parser-failures t))
  (if (eq object :failure)
      (clim:simple-parse-error "The input read was not of the required type.")
      (clim:input-not-of-required-type object type)))

------------------------------------------------------------------------
(let ((gr (make-instance 'pie-graph
                         :datasets (list
                                    (make-instance 'graph-data
                                      :name "cat1" 
                                      :data 45)
                                     (make-instance 'graph-data
                                      :name "cat2" :data 25)
                                     (make-instance 'graph-data
                                      :name "cat3" :data 15)
                                     (make-instance 'graph-data
                                      :name "cat4" :data 15)))))
  (view-graphs (list gr) :wait-until-done nil :create t))
|#
