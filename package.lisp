;;;; package.lisp

(defpackage #:stumpd
  (:use #:cl #:mpd #:iolib)
  (:import-from #:stumpwm
                #:defcommand
                #:make-sparse-keymap
                #:message
                #:define-key
                #:kbd
                #:*root-map*
                #:current-screen
                #:select-from-menu
                #:menu-state-selected
                #:menu-state-table)
  (:export #:*mpd-playback-map*
           #:file
           #:file-directory
           #:directory))
