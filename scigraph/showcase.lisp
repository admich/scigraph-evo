(in-package :graph)

(define-application-frame showcase ()
  ((graphs :initarg graph
           :initform nil
           :accessor application-graphs))
  (:panes
   (display :application
	        :display-function 'display-main
            :display-time t
	        :incremental-redisplay nil)
   (inter :interactor :max-height 20))
  (:command-table (showcase :inherit-from (:graph)))
  (:layouts
   (:default (vertically () display inter))))

(defun display-main (frame pane)
  (fill-window-with-graphs (application-graphs frame) :columns 2 :stream pane)) 

(define-showcase-command (com-quit :name t) () ()
                         (frame-exit *application-frame*))

(define-showcase-command (com-refresh :name t) () ()
                         (fill-window-with-graphs (application-graphs *application-frame*) :columns 2 :stream (find-pane-named *application-frame* 'display)))

(define-presentation-to-command-translator
    com-change-xy-axis
    (graph :command-name com-change-xy-axis
			  :command-table :graph
			  :gesture nil
              :tester ((object) (typep object 'graph-with-reselectable-axes))
			  :documentation "Change axis")
    (object &key window)
  (list object window))

(define-showcase-command com-change-xy-axis ((graph 'graph-with-reselectable-axes) (stream 'sheet))
  (let ((axis
         (menu-choose
          `(("Change x" :value :x
                        :documentation "Change x axis accessor")
            ("Change y" :value :y
                        :documentation "Change y axis accessor"))
          :label "Choose axis to change"
          :associated-window *standard-input*)))
    (when axis
      (let* ((accessor (menu-choose '(("First" :value #'first :documentatio "First")
                                     ("Second" :value #'second :documentatio "Second")
                                     ("Third" :value #'third :documentatio "Third"))
                                    :label "Choose accessor"))
             (x-accessor (if (eql axis :x) (eval accessor) (x-accessor graph)))
             (y-accessor (if (eql axis :y) (eval accessor) (y-accessor graph))))
        (when accessor
          (set-axes graph x-accessor y-accessor)
          (setf (auto-scale-needed graph) t)
          (refresh graph stream))))))

(defun init-showcase (frame)
  (let ((g1 (make-instance 'gr::annotated-graph :title "Annotated Graph"
                           :x-label "X"
                           :y-label "Y"))
        (g2 (make-instance 'gr::graph :title "Simple Graph with zero axis"
                           :x-min -10 :x-max 10
                           :y-min -5 :y-max 5                                       
                           :visible-borders '(:zero-abcissa :zero-ordinate)))
        (g3 (make-instance 'gr::annotated-graph :title "Bar graph"
                           :auto-scale nil
                           :x-min -0.5 :x-max 2.5
                           :y-min 0 :y-max 10))
        (g4 (make-instance 'gr::annotated-graph :title "Histogram"
                           ;:auto-scale nil
                           :x-min -10 :x-max 10
                           :y-min 0 :y-max 10))
        (g5 (make-instance 'gr::contour-graph :title "Contour Graph"
                           :auto-scale nil
                           :x-min 0 :x-max 50.0
                           :y-min 0 :y-max 50.0))
        (g6 (make-instance 'graph-with-reselectable-axes :title "Graph with reselectable axis: try command Change axis from menu."
                           :x-accessor #'first
                           :y-accessor #'second
                           :auto-scale :both))
        (d1 (make-instance 'gr:graph-data
			                 :symbologies '(:line-symbol)
                             :line-style 2
                             :color :orange
			                 :data '((0 0) (.1 .5) (.2 .3) (.3 .2) (.4 .8) (.5 .2))))
        (d2 (make-instance 'gr:graph-data
			                 :symbologies '(:step)
                             :color :sienna
			                 :data '((0 .1) (.05 .2) (.2 .3) (.4 .4) (.5 .5) (.6 .8))))
        (d3 (make-instance 'gr:equation-data
                           :symbologies '(:line)
                           :color :red
                           :equation '(* 2 (cos x))
                           :variable 'x :min (* -2 pi) :max (* 2 pi) :increment .01
                           :line-style 0))
        (d4 (make-instance 'gr:graph-data
                           :name "Categorical Data"
                           :data '((0.0 3.0) (1.0 4.0) (2.0 3.5))
                           :color :red
                           :bar-width 0.9
                           :symbologies '(:bar)))
        (d5 (make-instance 'gr:histogram-data
                           :name "100 gaussian random sample"
                           :color :orange                           
                           :sample-data (gaussian-random-sample 100)
                           :symbologies '(:bar)))
        (d6 (make-instance 'contour-data
                           :contour-surface (lambda (x y) (* (sqrt (abs (* (- x 15) (abs (- y 35)) (- x 25) (- y 20)))) 1e-1))))
        (d7 (make-instance 'multidimensional-data :name "xyz"
                           :data (loop for n from 0.0 to 10.0 collect (list n (* n n) (sqrt n))))))
    (add-dataset g1 d1)
    (add-dataset g1 d2)
    (add-dataset g2 d3)
    (add-dataset g3 d4)
    (add-dataset g4 d5)
    (add-dataset g5 d6)
    (add-dataset g6 d7)
    (setf (application-graphs frame) (list g1 g2 g3 g4 g5 g6))
    ))

(defun scigraph-showcase (&key new-process)
  (let ((frame (make-application-frame 'showcase :width 1600 :height 1000)))
    (init-showcase frame)
    (if new-process
        (clim-sys:make-process (lambda () (run-frame-top-level frame)) :name "Scigraph Showcase")
        (run-frame-top-level frame))))



