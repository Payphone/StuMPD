;;;; package.lisp

(defpackage #:stumpd
  (:use #:cl #:mpd #:iolib)
  (:import-from #:stumpwm
                #:defcommand
                #:make-sparse-keymap
                #:message
                #:define-key
                #:kbd
                #:*root-map*)
  (:export #:*mpd-playback-map*))
