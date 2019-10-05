(in-package :graph)


(defclass categorical-graph-mixin ()
  ())

(defclass categorical-graph (categorical-graph-mixin
                             annotated-graph)
  ()
  (:default-initargs :x-tick-numbering :each))

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

;; tmp methods
(defmethod compute ((self categorical-sample-dataset))
  '())

(defmethod data ((self categorical-sample-dataset))
  (loop for d in (sample-data self) collect
       (list (category-number self) d)))

(export '(categorical-graph categorical-sample-dataset))
