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

(define-presentation-type graph ()
  :description "a graph")

(define-presentation-method present (object (type graph) stream (view textual-view) &key)
  (format stream "~A" (name object)))

(define-presentation-method accept ((type graph) stream (view textual-view) &key)
  (read-char stream)
  (error "You must select a graph with the mouse."))

(define-presentation-method highlight-presentation ((type graph) record stream state)
  (declare (ignore type))
  (with-bounding-rectangle* (left top right bottom) (outside-box (presentation-object record))
    (draw-rectangle* stream 
                     left top right bottom
                     :ink +flipping-ink+ :filled nil)))

(define-presentation-type graph-data ()
  :description "a graph dataset")

(define-presentation-method present (object (type graph-data) stream (view textual-view) &key)
  (format stream "~A" (name object)))

(define-presentation-method accept ((type graph-data) stream (view textual-view) &key)
  (read-char stream)
  (error "You must select a graph dataset with the mouse."))


(defmethod display-graph ((graph basic-graph)
                          &key
                            (stream *standard-output*)                            
                            top
                            left
                            (reference :outside) ; :inside or :outside
                            (width 500)
                            (height 300))
  "Displays graph with upper-left corner starting at current cursor position
or at top left."
  (let ((*standard-output* stream))
    (unless (and top left)
      (multiple-value-bind (cursor-x cursor-y) (stream-cursor-position stream)
        (multiple-value-setq (left top) (values (or left cursor-x) (or top cursor-y)))))
    (if  (eql reference :inside)
         (set-inside-box graph (make-rectangle* left top (+ left width) (+ top height)) stream)
         (set-outside-box graph (make-rectangle* left top  (+ left width) (+ top height)) stream))
    (with-bounding-rectangle* (x1 y1 x2 y2) (outside-box graph)
      (draw-rectangle* stream x1 y1 x2 y2 :filled t :ink +background-ink+))
    (display graph stream)
    (setf (stream-cursor-position stream) (values left (+ top height)))))

(defun save-postscript-graph (graph filename &key (width 400) (height 400))
  (with-open-file (s filename :direction :output)
    (clim:with-output-to-postscript-stream (stream s)
      (display-graph graph :stream stream :width width :height height))))

(defun display-graphs (graphs &key
			      (stream *standard-output*)
			      (width 500)
			      (height 500))
  "Display a column of graphs"
  (let ((h (ROUND height (length graphs)))) 
    (dolist (graph graphs)
      (display-graph graph
	         :stream stream
	         :height h :width width)
      (stream-increment-cursor-position stream 0 50))))

(defun window-reverse-video (window &optional (fore +white+) (back +black+))
  "Change the foreground/background colors of the window."
  (setf (medium-foreground window) fore
        (medium-background window) back))

(defun autoscale-graphs (graphs autoscale-type)
  "Let the graphs mutually decide what scaling limits to use.
   Use this when many different graphs should have the same scale."
  (when (> (length graphs) 1)
    (let* ((minx 1.0e+30)
	   (maxx -1.0e+30)
	   (miny minx)
	   (maxy maxx))
      (dolist (graph graphs)
	(setf (auto-scale graph) :both)
	;; I can't tell if i have to do this, so i will:
	(do-auto-scale graph)
	(multiple-value-bind (x0 x1 y0 y1)
	    (graph-auto-scale-limits graph)
	  (when (member autoscale-type '(:x :both))
	    (setq minx (min minx x0))
	    (setq maxx (max maxx x1)))
	  (when (member autoscale-type '(:y :both))
	    (setq miny (min miny y0))
	    (setq maxy (max maxy y1)))))
      (dolist (graph graphs)
	(when (member autoscale-type '(:x :both))
	  (setf (x-min graph) minx)
	  (setf (x-max graph) maxx))
	(when (member autoscale-type '(:y :both))
	  (setf (y-min graph) miny)
	  (setf (y-max graph) maxy))
	(setf (auto-scale graph)
	      (case autoscale-type
		(:x :y)
		(:y :x)
		(:both nil)
		(otherwise :both)))))
    t))

(defun fill-window-with-graphs (graphs
				&key
				autoscale ; :X, :Y, :BOTH, or NIL
				(right-margin 0)
				(columns 1)
				(stream *standard-output*)
				(reverse-video :own-color))
  "Fill the window with columns graphs."
  (when (and (not (eql reverse-video :own-color)))
    (if reverse-video
	(window-reverse-video stream +white+ +black+)
        (window-reverse-video stream +black+ +white+)))
  (if autoscale (autoscale-graphs graphs autoscale))
  (window-clear stream)
  (when graphs
    (multiple-value-bind (w h) (stream-viewport-size stream)
      (decf w right-margin)
      (with-output-truncation (stream) ; don't wrap or scroll
	(if (= columns 1)
	    (display-graphs graphs :stream stream :height h :width w)
	  (let ((rows (values (ceiling (length graphs) columns))))
	    (dotimes (column columns)
	      (let ((g nil))
		(dotimes (row rows)
		  (let ((temp (pop graphs)))
		    (and temp (push temp g))))
		(setf (stream-cursor-position stream)
              (values (* (values (truncate w columns)) column) 0))
		(display-graphs (nreverse g)
				:stream stream
				:height h
				:width (values (truncate w columns)))))))))))

;;;
;;; Manipulating presentations
;;;

(defun graph-under-presentation (presentation)
  (when (presentation-p presentation)
    (let ((object (presentation-object presentation)))
      (if (graph-p object) object
	  (let ((superior (clim:output-record-parent presentation)))
	    (when superior (graph-under-presentation superior)))))))

(defun dataset-under-presentation (presentation)
  (when (presentation-p presentation)
    (let ((object (presentation-object presentation)))
      (if (graph-data-p object) object
	  (let ((superior (clim:output-record-parent presentation)))
	    (when superior (dataset-under-presentation superior)))))))

(defun graph-under-annotation-under-presentation (presentation)
  (when (presentation-p presentation)
    (let ((object (presentation-object presentation)))
      (if (annotation-p object) (graph object)
	  (let ((superior (clim:output-record-parent presentation)))
	    (when superior
	      (graph-under-annotation-under-presentation superior)))))))


;;;
;;; CP commands for graphs and graph-data.
;;;
(define-graph-command com-zoom-in ((graph 'graph) (WINDOW 'sheet))
  "Zoom in on a selected rectangle of the graph."
  (zoom-in graph window))

(define-presentation-to-command-translator zoom-in
  (graph com-zoom-in :graph
	 :gesture nil :documentation "Zoom In...")
  (object window)
  (list object window))

(define-graph-command com-zoom-out ((graph 'graph) (WINDOW 'sheet))
  "Undo the results of the most recent zoom-in command."
  (zoom-out graph WINDOW))

(define-presentation-to-command-translator zoom-out
  (graph com-zoom-out :graph
	 :tester ((object) (and (graph-p object) (zoom-stack object)))
	 :gesture nil :documentation "Zoom Out")
  (object window)
  (list object window))

(define-graph-command com-slider-crosshairs ((graph 'graph) (WINDOW 'sheet))
  "Display crosshairs on the graph at the current pointer position."
  (multiple-value-bind (x y) (stream-pointer-position window)
    (multiple-value-bind (x y) (transform-position (stream-to-xy-transformation graph) x y)
      (multiple-value-setq (x y) (slider-interact graph WINDOW x y t))
      (and x y (describe-point graph x y)))))

(define-presentation-to-command-translator slider-crosshairs
  (graph com-slider-crosshairs :graph
	 :gesture nil :documentation "Crosshairs")
  (object window)
  (list object window))

(define-graph-command com-redraw-graph ((graph 'graph) (window 'sheet))
  "Erase and then redraw a graph."
  (refresh graph window))

(define-presentation-to-command-translator com-redraw-graph
   (graph com-redraw-graph :graph
	  :gesture nil :documentation "Redraw Graph")
   (object window)
  (list object window))

(define-graph-command com-reveal-datasets ((graph 'graph) (WINDOW 'sheet))
  "Reveals any data previously hidden by 'Remove Dataset'."
  (setf (hidden-datasets graph) nil)
  (refresh graph window))

(define-presentation-to-command-translator com-reveal-datasets
    (graph com-reveal-datasets :graph
	 :gesture nil :documentation "Reveal Hidden Data"
	 :tester ((object)
		  (and (graph-p object)
		       (not (null (hidden-datasets object))))))
  (object window)
  (list object window))

(define-graph-command com-remove-dataset ((dataset 'graph-data) (window 'sheet) (presentation 't))
  "Hides some graph data.  Unhide it using 'Reveal Datasets.'"
  (let ((g (graph-under-presentation presentation)))
    (when g
      (push dataset (hidden-datasets g))
      (refresh g window))))

(define-presentation-to-command-translator com-remove-dataset
   (graph-data com-remove-dataset :graph
	       :gesture nil :documentation "Hide Data"
	       :tester ((object presentation)
			(declare (ignore object))
			(or (graph-under-presentation presentation)
			    (graph-under-annotation-under-presentation
			     presentation))))
   (object window presentation)
  (list object window presentation))


(defun draw-dash-sample (stream dash-pattern pretty-name selected-p)
  "Show what this dash pattern looks like."
  (declare (ignore pretty-name))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    ;; (format stream "(~A ~A)" x y)
    (let ((width (* 3 (stream-character-width stream)))
	  (height (* 6 (stream-line-height stream)))
	  (thick (+ 2 %thickness))
	  (thin %thickness)
	  (fudge 2))
      (draw-line* stream (+ x fudge) (+ y fudge)
			(- (+ x width) fudge) (- (+ y height) fudge)
			:thickness (if selected-p thick thin)
			:dash-pattern dash-pattern
			:transform nil
			:ink +foreground-ink+)
      (draw-rectangle* stream x y (+ x width) (+ y height)               
		      :filled nil
		      :ink (if selected-p +foreground-ink+ +background-ink+))
      (force-output stream))))

(define-presentation-type-abbreviation dash-pattern ()
  `((member ,@(let ((numbers nil))
		(dotimes (i 7) (push i numbers))
		(nreverse numbers)))
    :name-key princ-to-string
    :printer present-line-style
    :highlighter highlight-line-style))

(defun present-line-style (object stream &key acceptably)
  (declare (ignore acceptably))
  (if (stringp object) (setq object (read-from-string object)))
  (with-room-for-graphics (stream)
    (draw-dash-sample stream object (princ-to-string object) nil)))

(defun highlight-line-style (continuation object stream)
  (clim:surrounding-output-with-border
   (stream)
   (funcall continuation object stream)))
  
