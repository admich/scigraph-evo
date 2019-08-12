;;; -*- Syntax: Common-lisp; Package: User -*-
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

(in-package :common-lisp-user)

(defpackage #:tool
  (:use #:clim-lisp #:clim)
  (:export #:named-mixin
           #:name
           #:name-string
           #:make-name
           #:declare-required-method
           #:several-choose
           #:choose-character-style
           #:window-edit-text
           #:string-size)

  (:shadowing-import-from #:dwim
                          #:stream-line-height
                          #:stream-character-width
                          #:stream-viewport
                          #:stream-viewport-size))

(defpackage #:statistics
  (:nicknames #:stat #:st)
  (:use #:clim-lisp #:clim)
  (:export #:random-seed
           #:with-seed
           #:uniform
           #:uniform-0-1
           #:uniform-between
           #:gaussian-random
           #:gaussian
           #:random-yes-no
           #:erf))

(defpackage #:graph
  #-allegro (:nicknames gr)           ; "GR" names something already.
  (:shadow variable)                  ; shouldn't be inherited but is
  (:use #:clim-lisp #:clim #:tool #:statistics)
  (:import-from #:clim
                #:window-clear
                #:redisplay-frame-pane
                #:parse-text-style
                #:medium-text-style
                #:stream-string-width
                #:stream-increment-cursor-position
                #:beep
                #:make-command-table
                #:find-command-table)
  (:export
   #:scigraph-showcase
   #:make-demo-frame
   #:view-graphs
   #:display-graph
   #:save-postscript-graph
   #:display-graphs
   #:graph-presentation-type
   #:presentation
   #:graph-under-presentation
   #:present-self-p
   #:fill-window-with-graphs
   #:graphs-for-slider
   #:autoscale-graphs
   #:auto-scale-needed
   #:auto-scale
   #:graph-auto-scale-limits
   #:display-data
   #:display-datum
   #:displayed?
   #:datum-position
   #:line-style
   #:thickness
   #:symbologies
   #:graph-p
   #:graph-data-p
   #:map-data
   #:map-data-xy
   #:missing-data-threshold
   #:display
   #:erase
   #:move
   #:refresh
   #:zoom-stack
   #:pop-accept-items
   #:pop-accept-label
   #:popup-accept-forms
   #:popup-accept
   #:popup-accept-standard-loop
   #:add-dataset
   #:datasets
   #:data
   #:define-graph-command
   #:xy-inside
   #:set-xy-inside
   #:xy-to-stream
   #:name
   #:annotation
   #:point-annotation
   #:interval-annotation
   #:annotate
   #:annotate-graph
   #:annotate-interval
   #:annotate-point
   #:annotate-data-interval
   #:annotate-data-point
   #:description-choices
   #:default-text-style
   #:x-label
   #:y-label

   #:color-presentation
   #:device-filled-p
   #:device-fill-pattern

   #:graph-data
   #:timeseries-data
   #:presentable-data-mixin
   #:graph-data-limits-mixin
   #:graph-data-auto-scale-mixin
   #:graph-data-auto-color-mixin
   #:graph-data-symbology-mixin
   #:graph-data-add-datum-mixin
   #:presentable-graph-data-legend-mixin
   #:graph-data-legend-mixin
   #:basic-list-datum-mixin
   #:graph-data-list-map-mixin
   #:essential-graph-data-map-mixin
   #:basic-graph-data
   #:equation-data
   #:sample-data
   #:histogram-data
   #:MULTIDIMENSIONAL-DATA

   #:graph
   #:annotated-graph
   #:presentable-graph-mixin   
   #:graph-datasets-mixin
   #:graph-legend-mixin
   #:graph-relative-size-mixin
   #:graph-zoom-mixin
   #:graph-slider-interaction-mixin
   #:graph-slider-mixin
   #:graph-handle-mouse-mixin
   #:graph-mouse-resolution-mixin   
   #:graph-auto-scale-extensions-mixin
   #:graph-limits-mixin
   #:graph-auto-scale-mixin   
   #:graph-grid-mixin   
   #:graph-border-mixin   
   #:basic-graph
   #:graph-with-reselectable-axes)

  (:shadowing-import-from #:dwim
                          #:present
                          #:present-to-string
                          #:presentation-type
                          #:presentation-p
                          #:present-to-string
                          #:redisplayable-format
                          #:accept
                          #:accepting-values
                          #:accept-variable-values
                          #:input-position
                          #:insertion-pointer
                          #:catching-parser-failures
                          #:suggest
                          #:sheet
                          #:accept-values-choose-from-sequence
                          #:alist-subset
                          #:invisible-object
                          #:window-under-mouse
                          #:change-size
                          #:stream-line-height
                          #:stream-character-width
                          #:stream-viewport
                          #:stream-viewport-size
                          #:printing-random-object
                          #:with-stack-list
                          #:define-presentation-to-command-translator
                          #:define-presentation-type
                          #:with-output-truncation
                          #:with-redisplayable-output
                          #:alist-member
                          #:command
                          #:status-pane
                          #:status-line
                          #:set-status-line
                          #:mouse-documentation-pane
                          #:*include-machine-name-in-status-line-p*
                          #:*frame-for-status-line*
                          #:*time-type*
                          #:initialize-status-line
                          #:make-status-line
                          #:refresh-status-line
                          #:noting-progress
                          #:note-progress))

(in-package :graph)

(clim:define-command-table :graph)
