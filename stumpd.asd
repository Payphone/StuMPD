;;;; stumpd.asd

(asdf:defsystem #:stumpd
  :description "MPD bindings for the StumpWM."
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :depends-on (#:stumpwm
               #:mpd
               #:iolib)
  :serial t
  :components ((:file "package")
               (:file "stumpd")))
