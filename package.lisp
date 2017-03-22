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
                #:menu-state-table)
  (:export #:*mpd-playback-map*
           #:mpd-file
           #:mpd-directory
           #:mpd-playlist))
