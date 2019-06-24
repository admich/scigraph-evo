(in-package :graph)

(export '(pie-graph))

;;;; pie chart

(defmethod output-record-refined-position-test ((record climi::draw-ellipse-output-record) x y)
  (with-slots ((start-angle climi::start-angle)
               (end-angle climi::end-angle)
               (center-x climi::center-x)
               (center-y climi::center-y)) record
    (let ((dx (- x center-x))
          (dy (- center-y y)))        
      (if (and  (< dx 0) (> dy 0))
          (< start-angle (- (atan dy dx) (* 2 pi)) end-angle)
          (< start-angle (atan dy dx) end-angle)))))

(defmethod highlight-output-record ((record climi::draw-ellipse-output-record)  stream (state (eql :highlight)))
  (let* ((ink (displayed-output-record-ink record)))
    (multiple-value-bind (i h s) (color-ihs ink)
      (let ((new-ink (make-ihs-color i h (* 0.4 s))))
        (with-slots ((start-angle climi::start-angle)
                 (end-angle climi::end-angle)
                 (radius climi::radius-1-dx)
                 (center-x climi::center-x)
                 (center-y climi::center-y)) record
          (draw-circle* stream center-x center-y radius :start-angle start-angle :end-angle end-angle :ink new-ink))))))

(defmethod highlight-output-record ((record climi::draw-ellipse-output-record)  stream (state (eql :unhighlight)))
  (let* ((ink (displayed-output-record-ink record)))
    (multiple-value-bind (i h s) (color-ihs ink)
      (with-slots ((start-angle climi::start-angle)
                   (end-angle climi::end-angle)
                   (radius climi::radius-1-dx)
                   (center-x climi::center-x)
                   (center-y climi::center-y)) record
        (draw-circle* stream center-x center-y radius :start-angle start-angle :end-angle end-angle :ink ink)))))


(defclass pie-graph-mixin () ()
  (:default-initargs :visible-borders '() :x-min -2 :x-max 2 :y-min -2 :y-max 2 :auto-scale nil))

(defclass pie-graph (pie-graph-mixin
                      annotated-graph)
  ())

(defmethod display-data ((self essential-graph-data-map-mixin)  stream (graph pie-graph-mixin))
      (multiple-value-bind (cx cy) (xy-to-rs graph 0 0)
        (let ((angle (* 2 pi (/ (data self) total))))
          (with-ink (stream (ink self))
            (draw-circle* stream cx cy radius :filled t :start-angle  (- last-angle angle) :end-angle last-angle))
          (setf last-angle (- last-angle angle)))))

(defmethod graph-display-data ((self pie-graph-mixin) STREAM)
  (let ((last-angle (/ pi 2))
        (total (reduce #'+ (datasets self) :key (lambda (x) (data x))))
        (radius (* (x-scale self) 1)))
    (declare (special total radius last-angle))
    (dolist (dataset (datasets self))
      (display-data dataset stream self))))

(defmethod default-annotation-position ((graph pie-graph-mixin) &optional (width 20) (height 20))
  "Find a place (RS) on the graph where the annotation won't obscure any data."
  (xy-to-rs graph 1 2))

(defmethod display-legend-datums ((self graph-data-legend-mixin) STREAM
                                  (graph pie-graph-mixin) left top width height)
  "Display some points in the legend area to show the current symbology settings."
  (with-ink (stream (ink self))
    (draw-rectangle* stream left top (+ left width) (+ top height))))

#|
(let ((gr (make-instance 'pie-graph
                         :datasets (list
                                    (make-instance 'graph-data
                                      :name "cat1" :data 45)
                                     (make-instance 'graph-data
                                      :name "cat2" :data 25)
                                     (make-instance 'graph-data
                                      :name "cat3" :data 15)
                                     (make-instance 'graph-data
                                      :name "cat4" :data 15)))))
  (view-graphs (list gr) :wait-until-done nil :create t))
|#
