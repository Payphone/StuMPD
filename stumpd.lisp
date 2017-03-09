;;;; stumpd.lisp

(in-package #:stumpd)

;; Connection Settings

(defvar *socket* (initialize-connection "localhost" 6600))

(defmacro auto-reconnect (socket &body body)
  "Reconnects socket when it closes. MPD automatically closes clients after a
  period of time."
  `(handler-case
       (progn ,@body)
     (error ()
       (setf ,socket (initialize-connection "localhost" 6600))
       ,@body)))

(defmacro define-mpd-command (name (&rest args) (&rest interactive-args)
                              &body body)
  "Defines a mpd command using the StumpWM defcommand wrapped with an
  auto-reconnect."
  `(defcommand ,name (,@args) (,@interactive-args)
     (auto-reconnect *socket*
       ,@body)))

;; Playback

(define-mpd-command set-volume (volume) ((:string "Volume (1-100): "))
  (playback-set-volume *socket* volume))

(define-mpd-command pause/resume () ()
  (let ((state (state (query-status *socket*))))
    (if (string= state "pause")
        (playback-pause *socket* nil)
        (playback-pause *socket* t))))

(define-mpd-command play (&optional song-position) ((:string "Position: "))
  (playback-play *socket* song-position))

(define-mpd-command previous-song () ()
  (playback-previous *socket*))

(define-mpd-command next-song () ()
  (playback-next *socket*))

(define-mpd-command stop () ()
  (playback-stop *socket*))

;; Playlist

(define-mpd-command add-song (song) ((:string "File: "))
  (playlist-add-song *socket* song))

(define-mpd-command clear-songs () ()
  (playlist-clear-songs *socket*))

(define-mpd-command delete-song (song) ((:string "File: "))
  (playlist-delete-song *socket* song))

;; Queries

(define-mpd-command current-song () ()
  (let ((song (query-song *socket*)))
    (message "~A~%~A~%~A" (or (title song) "Unknown") (or (artist song) "")
             (or (album song) ""))
    (or (title song) "Unknown")))

;; Browsing
(defclass file ()
  ((last-modified :initarg :last-modified :accessor last-modified)
   (size :initarg :size :accessor size)
   (name :initarg :name :accessor name)))

(defclass file-directory ()
  ((last-modified :initarg :last-modified :accessor last-modified)
   (name :initarg :name :accessor name)))

(defun escape-spaces (string)
  "Inserts a backslash before every space in a string."
  (coerce (loop for char across string
             if (char= char #\Space) collect #\\
             collect char) 'string))

(defun browse-directory (directory)
  (database-list-files *socket*(format nil "\"~A\"" (escape-spaces directory))))
#|
(select-from-menu (current-screen) options title
                  (or initial-selection 0)
                  keymap)
|#
;; Keybindings

(defvar *mpd-playback-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") "reconnect")
    (define-key map (kbd "v") "set-volume")
    (define-key map (kbd "p") "pause/resume")
    (define-key map (kbd "P") "play")
    (define-key map (kbd "<") "previous-song")
    (define-key map (kbd ">") "next-song")
    (define-key map (kbd "s") "stop")
    (define-key map (kbd "a") "add-song")
    (define-key map (kbd "c") "clear-songs")
    (define-key map (kbd "d") "delete-song")
    (define-key map (kbd "S") "current-song")
    map))
