(in-package :graph)

;;;; pie chart

(export '(pie-graph pie-graph-datum dataset-summary-function))

(defclass pie-graph-mixin ()
  ((total-value :initarg :total :accessor total-value)
   (radius :initarg :radius :accessor radius :initform 120)
   (pie-datums :initarg :datums :initform nil :accessor pie-datums))
  (:default-initargs :visible-borders '() :x-min -2 :x-max 2 :y-min -2 :y-max 2 :auto-scale nil ))

(defclass pie-graph (pie-graph-mixin
                      annotated-graph)
  ())

(defclass pie-graph-datum (datum)
  ((value :initarg :value :accessor datum-value)
   (start-angle :initarg :start-angle :reader start-angle)
   (end-angle :initarg :end-angle :reader end-angle)))

(defgeneric dataset-summary-function (graph dataset)
  (:documentation "A function that extract from a dataset a single value used by some statistical plot like pie graph or bar graph"))

(defmethod dataset-summary-function (graph dataset)
  (data dataset))

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
        (total (reduce #'+ (datasets self) :key (lambda (x) (dataset-summary-function self x)))))
    (setf (total-value self) total)
    (dolist (dataset (datasets self))
      (let* ((value (dataset-summary-function self dataset))
             (angle (* 2 pi (/ value total)))
             (datum (make-instance 'pie-graph-datum :value value
                                  :start-angle (- last-angle angle) :end-angle last-angle)))
        (setf (getf (pie-datums self) dataset) datum
              last-angle (start-angle datum)))
      (display-data dataset stream self))))

(defmethod display-data ((self essential-graph-data-map-mixin)  stream (graph pie-graph-mixin))
  (with-ink (stream (ink self))
    (let ((displayer (datum-displayer self graph))
          (datum (getf (pie-datums graph) self)))
      (declare (compiled-function displayer))
      (funcall displayer stream nil nil datum))))

(defmethod datum-displayer ((self essential-graph-data-map-mixin) (graph pie-graph-mixin))
  "Returns a function that expects a stream, UV coordinates of next datum,
   and the datum itself."
  #'(lambda (stream u v datum)
      (multiple-value-bind (cx cy) (xy-to-rs graph 0 0)
        (with-new-output-record (stream 'circular-sector)
          (draw-circle* stream cx cy (radius graph) :filled t :start-angle  (start-angle datum) :end-angle (end-angle datum))))))

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
  (let* ((datum (getf (pie-datums graph) dataset))
         (angle (/ (+ (start-angle datum) (end-angle datum)) 2))
         (r (* (cos angle) (/ (radius graph) 2)))
         (s (* (sin angle) (/ (radius graph) 2))))
    (multiple-value-bind (cr cs) (xy-to-rs graph 0 0)
      (multiple-value-bind (x y) (rs-to-xy graph (+ cr r) (- cs s))
        (values x y datum)))))

(defmethod text-for-datum (graph dataset (datum pie-graph-datum))
  (let* ((total (and graph (total-value graph)))
         (value  (datum-value datum))
         (percent (* 100 (/ value total)))
         (*print-circle* nil))
    (format nil "~A~%~f~%~,1f%" (name dataset) value percent)))

#|
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
