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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :clim-2 *features*))

(defpackage dwim
  (:use #:cl)
  (:import-from #:clim
                #:present-to-string
                #:presentation-type
                #:present
                #:port
                #:boolean
                #:frame-manager
                #:find-frame-manager
                #:suggest
                #:accept)
  (:shadow #:presentation-p
           #:redisplayable-format
           #:accepting-values
           #:accept-variable-values
           #:window-under-mouse
           #:change-size
           #:stream-line-height
           #:stream-character-width
           #:stream-viewport
           #:stream-viewport-size

           #:printing-random-object
           #:with-stack-list
           #:with-output-truncation
           #:with-redisplayable-output
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
           #:note-progress)
  (:export #:present
           #:present-to-string
           #:presentation-type
           #:presentation-p
           #:present-to-string
           #:redisplayable-format
           #:accepting-values
           #:accept-variable-values
           #:suggest
           #:window-under-mouse
           #:change-size
           #:stream-line-height
           #:stream-character-width
           #:stream-viewport
           #:stream-viewport-size
           #:stream-set-pointer-position*
           #:printing-random-object
           #:with-stack-list
           #:with-output-truncation
           #:with-redisplayable-output
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
