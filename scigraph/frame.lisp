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

#|

This file implements the function VIEW-GRAPHS as a way of getting a generic
program frame for viewing a list of graphs.  The program can be either 
temporary or persistent depending upon the keyword :wait-until-done.
The program consists of a single, main display pane where graphs get drawn.

In CLIM, the scigraph frame can also be made a "slave" of a second "master" frame
by providing the :master keyword.  The slave is like an extension of the master,
where presentations on the slave are mouse-sensitive, but the master's command
loop is responsible for reading and executing all commands.  For this to work,
the master must inherit the graph command table.

|#

;;;
;;; Make a pane that runs the redisplayer when asked to repaint, so that
;;; frame resizing also resizes the graphs.
;;;

#|
How to get a pane to redraw its contents when it's been reshaped:

The generic function to specialize on, as I told you, is
WS::SHEET-REGION-CHANGED (which should have been exported from CLIM,
but wasn't).  However, that generic function is invoked on a
clim-stream-pane every time something is added to the output history.
It's only invoked on its viewport when the actual space taken up really 
changes, but there's no easy way for you to specialize on the viewport
class.  So you should do the following (in the ws package, 'natch):
|#


;;;
;;; Now define the scigraph viewer frame.
;;;

(define-application-frame graph-viewer ()
  ((graphs :initform nil :accessor frame-graphs)
   (display-settings :initform nil :accessor display-settings))
  (:panes
   (display :application
	    :display-function 'redisplay-graphs
	    :display-time t
	    :text-style (parse-text-style '(:fix :roman :normal))
	    :scroll-bars t))
  (:pointer-documentation t)
  (:layouts
   (default (vertically () display)))
  (:command-table (graph-viewer :inherit-from (:graph)))
  (:top-level (scigraph-top-level)))

(defun scigraph-top-level (self)
  (let* ((*package* (find-package :graph)))
    (loop
      (with-simple-restart (scigraph-top-level "Abort to SCIGRAPH Top Level")
	    (redisplay-frame-pane self (get-frame-pane self 'display))
	    (default-frame-top-level self)))))

(defun redisplay-graphs (self stream)
  ;; Vertically stack the graphs to fill the pane.
  (apply #'fill-window-with-graphs
	 (frame-graphs self)
	 :stream stream
	 (display-settings self)))

(defun view-graphs
    (graphs
     &key
       (columns 1)
       autoscale
       (reverse-video :own-color)
       (create t) ; nil t :force view find-application-frame
       master
       (type 'graph-viewer)
       (title "View Graphs")
       (left 0) (bottom 0)
       (width 600) (height 400)
       (wait-until-done nil)
       &allow-other-keys)
  "Display a list of graphs in an interactive program frame."
  (let* ((manager (if master (frame-manager master) (find-frame-manager)))
         (frame
          (find-application-frame type
                                  :create create
                                  :activate nil
                                  :frame-manager manager
                                  :left (max 10 left)
                                  :top (max 10 (- height bottom))
                                  :width width
                                  :height height
                                  :pretty-name title)))
    (setf (clim:frame-pretty-name frame) title)
	(clim:reset-frame frame)
    (let ((graft (graft (port manager))))
      (clim:layout-frame frame (min width (clim:graft-width graft :units :device))
                         (min height (clim:graft-width graft :units :device))))

    (setf (frame-graphs frame) graphs)
    (setf (display-settings frame)
    	  `(:columns ,columns
    		         :reverse-video ,reverse-video
    		         :autoscale ,autoscale))
    	    ;; Now we need to make sure the panes get sized BEFORE
    	    ;; the pane displayer gets run.  By default, this happens
    	    ;; in the opposite order.  Order is important because
    	    ;; scigraph asks the pane how big it is before drawing
    	    ;; the graph.
    (resize-sheet (frame-top-level-sheet frame) width height)
    ;; start frame
    (cond (master
	       (let ((b (clim:stream-input-buffer
		             (clim:frame-top-level-sheet master)))
		         (top-level-window (clim:frame-top-level-sheet frame)))
	         (labels ((set-input-buffer (window buffer)
			            (setf (clim:stream-input-buffer window) buffer)
			            (dolist (w (clim:sheet-children window))
			              (set-input-buffer w buffer))))
	           (set-input-buffer top-level-window b)
	           (clim:enable-frame frame)
	           (clim:redisplay-frame-panes frame :force-p t)
	           ;; return the window just created
	           (values top-level-window))))
	      (T
           (if wait-until-done
               (run-frame-top-level frame)
               (clim-sys:make-process (lambda () (run-frame-top-level frame))
                                      :name "Scigraph Frame Top Level"))))))




