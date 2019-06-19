(in-package :graph)

(export '(pie-graph))

;;;; pie chart

(defclass pie-graph-mixin () ()
  (:default-initargs :visible-borders '() :x-min -2 :x-max 2 :y-min -2 :y-max 2 :auto-scale nil))

(defclass pie-graph (pie-graph-mixin
                      annotated-graph)
  ())


(defmethod graph-display-data ((self pie-graph-mixin) STREAM)
  (let ((last-angle (/ pi 2))
        (total (reduce #'+ (datasets self) :key (lambda (x) (data x))))
        (radius (* (x-scale self) 1)))
    (multiple-value-bind (cx cy) (xy-to-rs self 0 0)
      (mapcar  #'(lambda (dataset)
                         (let ((angle (* 2 pi (/ (data dataset) total))))
                           (with-ink (stream (ink dataset))
                             (draw-circle* stream cx cy radius :filled t :start-angle  (- last-angle angle) :end-angle last-angle))
                           (setf last-angle (- last-angle angle))))
                (datasets self)))))

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
