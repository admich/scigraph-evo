(in-package :graph)

(export '(x-timestamp-graph))

(defclass x-timestamp-graph-mixin ()
  ((x-format-timestring :initarg :x-format-timestring
                        :accessor x-format-timestring
                        :initform '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2))
                        :documentation "The value of this slot is passed as :format in local-time:format-timestring function")))

(defclass x-timestamp-graph (x-timestamp-graph-mixin annotated-graph)
  ()
  (:default-initargs :y-tick-numbering :each :x-tick-numbering :each :x-dtick #.(* 365 86400) :x-auto-tick t))

(defmethod x-tick-spacing ((self x-timestamp-graph-mixin))
  (with-slots (x-auto-tick x-min x-max x-dtick) self
    (if x-auto-tick (time-autotick x-min x-max) x-dtick)))

(defmethod DISPLAY-BOTTOM-BORDER ((self x-timestamp-graph-mixin) STREAM line-drawer)
  (with-slots (x-min x-max y-min tick-size x-tick-numbering x-format-timestring) self
    (let* ((dtick (x-tick-spacing self))
           (tick-size (/ tick-size (y-scale self))))
      (draw-linear-axis self stream x-min x-max y-min :x dtick tick-size x-tick-numbering
                        #'(lambda (r s number)
                            (let ((number-string (local-time:format-timestring nil (local-time:universal-to-timestamp (round number)) :format x-format-timestring))) 
                              (draw-text* stream number-string r s :align-x :left :align-y :top
                                        :transform-glyphs t
                                        :transformation (make-rotation-transformation* (/ pi 5) (round r) (round s)))))))))


