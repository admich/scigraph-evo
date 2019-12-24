(in-package :graph)

(defclass categorical-graph-mixin ()
  ()
  (:documentation "Each dataset is a category.
The graph set the x position of each category")
  (:default-initargs :x-tick-numbering :each
    :x-dtick 1 :x-auto-tick nil))

(defclass categorical-graph (categorical-graph-mixin
                             annotated-graph)
  ())

(defmethod display :around ((self categorical-graph-mixin) stream)
  (loop for dataset in (datasets self)
       for x = 0 then (1+ x) do
       (setf (category-number dataset) x))
  (call-next-method))

(defmethod DISPLAY-BOTTOM-BORDER ((self categorical-graph-mixin) STREAM line-drawer)
  (with-slots (x-min x-max y-min tick-size x-tick-numbering) self
    (let* ((dtick (x-tick-spacing self))
           (tick-size (/ tick-size (y-scale self))))
      (draw-linear-axis self stream x-min x-max y-min :x dtick tick-size x-tick-numbering
                        #'(lambda (r s number)
                            (let* ((dataset (nth (truncate number) (datasets self)))
                                   (text (format nil "~a" dataset))) 
                              (draw-text* stream text r s :align-x :center :align-y :top)))))))

(defclass categorical-sample-dataset (graph-sample-data-mixin
                                      graph-data)
  ((category-number :initform 0 :accessor category-number
                    :documentation "This slot is set from the graph")))

(defmethod compute ((self categorical-sample-dataset))
  '())

(defmethod data ((self categorical-sample-dataset))
  (loop for d in (sample-data self) collect
       (list (category-number self) d)))

;;;; STATISTICS GRAPH
(defclass statistical-graph-mixin ()
  ())

(defclass statistical-graph (statistical-graph-mixin
                             categorical-graph-mixin
                             graphics-style-mixin
                             annotated-graph)
  ()
  (:default-initargs :x-tick-numbering :each))

(defclass statistics-dataset (categorical-sample-dataset)
  ((statistic :initarg :statistic :accessor dataset-statistic :initform :samples)))

(defmethod data ((self statistics-dataset))
  (data-from-statistic self (dataset-statistic self)))

(defgeneric data-from-statistic (self statistic)
  (:method ((self statistics-dataset) statistic)
    (loop for d in (sample-data self) collect
         (list (category-number self) d))))

(defmethod data-from-statistic ((self statistics-dataset) (statistic (eql :mean)))
  (loop for d in (sample-data self) summing d into tot
       finally
       (return `((,(category-number self) ,(/ tot (length (sample-data self))))))))

(defmethod data-from-statistic ((self statistics-dataset) (statistic (eql :box-whiskers)))
  "return (x mean min 1q 2q 3q max)"
  (let ((data (sample-data self)))
    (list
     (list (category-number self)
           (cl-mathstats:mean data)
           (cl-mathstats:minimum data)
           (cl-mathstats:quantile data 0.25)
           (cl-mathstats:quantile data 0.5)
           (cl-mathstats:quantile data 0.75)
           (cl-mathstats:maximum data)))))

(defmethod datum-style-displayer ((self statistics-dataset) graph (type (eql :box-whiskers)))  
  #'(lambda (stream r s datum)
      (let* ((width 10)
             (hwidth 5)
             (x (pop datum))
             (mean (second (multiple-value-list (xy-to-rs graph x (pop datum)))))
             (min (second (multiple-value-list (xy-to-rs graph x (pop datum)))))
             (q1 (second (multiple-value-list (xy-to-rs graph x (pop datum)))))
             (q2 (second (multiple-value-list (xy-to-rs graph x (pop datum)))))
             (q3 (second (multiple-value-list (xy-to-rs graph x (pop datum)))))
             (max (second (multiple-value-list (xy-to-rs graph x (pop datum))))))
        (draw-line* stream r min r q1)
        (draw-line* stream (- r hwidth) min (+ r hwidth) min)
        (draw-circle* stream r mean 5)
        (draw-rectangle* stream (- r width) q1 (+ r width) q3 :filled nil)
        (draw-line* stream (- r width) q2 (+ r width) q2)
        (draw-line* stream r max r q3)
        (draw-line* stream (- r hwidth) max (+ r hwidth) max))))

(defmethod display-data ((self essential-graph-data-map-mixin) STREAM (graph statistical-graph))
  "Display the data on graph GRAPH using DATUM-DISPLAYER."
  ;;; review i define only to use the graph ink instead of data ink
  (with-ink (stream (ink graph))
    ;; Fixup the ink just once, since its the same for every datum.
    (let ((displayer (datum-displayer self graph))
          (trans (xy-to-rs-transformation graph)))
      (declare (compiled-function displayer))
      (map-data self #'(lambda (datum)
                         (multiple-value-bind (x y) (datum-position self datum)
                           (multiple-value-setq (x y) (transform-position trans x y))
                           (funcall displayer stream x y datum)
                           ;; Forcing the x buffer is nice here, but it is
                           ;; extremely expensive.  It slows drawing by 4x.
                           ;;  (force-output stream)
                           ))
                (data self)))))

(export '(categorical-graph categorical-sample-dataset statistical-graph statistics-dataset))
