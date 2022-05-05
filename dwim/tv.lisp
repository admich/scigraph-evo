;;; -*- Syntax: Common-lisp; Package: DWIM -*-
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

(in-package :dwim)

;;;
;;; Operations associated with tv windows (mostly).
;;;

(defun window-under-mouse ()
  (clim:pointer-sheet
   (clim:port-pointer
    (clim:port clim:*application-frame*))))

(defun stream-line-height (stream &optional TEXT-STYLE)
  (if TEXT-STYLE
      (truncate (clim:stream-line-height stream
                                         :text-style TEXT-STYLE))
      (truncate (clim:stream-line-height stream))))

(defun stream-character-width (stream &optional (char #\m))
  ;; "m" is the usual character (the term "ems" is often used in typesetting
  ;; to indicate units of width).
  (if (clim:extended-output-stream-p stream)
      (clim:stream-character-width STREAM char)
      8))

(defmethod stream-viewport (stream)
  ;;(declare (values left top right bottom))
  (cond ((not (clim:extended-output-stream-p stream)))
        ((or (and (type-specifier-p 'clim-postscript::postscript-stream)
                  (typep stream 'clim-postscript::postscript-stream))
             (and (type-specifier-p 'clim-pdf::clim-pdf-stream)
                  (typep stream 'clim-pdf::clim-pdf-stream)))
         ;; width  = inches x 72
         ;; height = inches x 72
         (values 0 0 #.(* 72 7) #.(* 72 10)))
        (t
         (let ((v (clim:window-viewport stream)))
           (if v (clim:rectangle-edges* v)
               (values 0 0
                       (clim:bounding-rectangle-width stream)
                       (clim:bounding-rectangle-height stream)))))))

(defmethod stream-viewport-size (stream)
  ;;(declare (values width height))
  (multiple-value-bind (left top right bottom) (stream-viewport stream)
      (values (- right left) (- bottom top)))) 

