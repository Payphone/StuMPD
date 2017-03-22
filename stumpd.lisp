;;;; stumpd.lisp

(in-package #:stumpd)

;; Connection Settings

(defvar *socket* (initialize-connection "localhost" 6600))

(defcommand reconnect () ()
  (setf *socket* (initialize-connection "localhost" 6600)))

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

(define-mpd-command set-volume (volume) ((:number "Volume (1-100): "))
  (playback-set-volume *socket* volume)
  (message "Volume set."))

(define-mpd-command pause/resume () ()
  (if (string= (state (query-status *socket*)) "pause")
      (progn (playback-pause *socket* nil) (message "Resumed."))
      (progn (playback-pause *socket* t) (message "Paused."))))

(define-mpd-command play () ()
  (playback-play *socket* "0")
  (message "Playing."))

(define-mpd-command previous-song () ()
  (playback-previous *socket*)
  (message "Previous song."))

(define-mpd-command next-song () ()
  (playback-next *socket*)
  (message "Next song."))

(define-mpd-command stop () ()
  (playback-stop *socket*)
  (message "Stopped."))

;; Playlist

(define-mpd-command load-playlist (playlist) ((:string "Playlist: "))
  (playlist-load *socket* playlist)
  (message "Loaded playlist."))

(define-mpd-command add-song (song) ((:string "File: "))
  (playlist-add-song *socket* song)
  (message "Added song."))

(define-mpd-command clear-songs () ()
  (playlist-clear-songs *socket*)
  (message "Cleared playlist."))

(define-mpd-command delete-song (song) ((:string "File: "))
  (playlist-delete-song *socket* song)
  (message "Deleted song."))

;; Queries

(define-mpd-command current-song () ()
  (let ((song (query-song *socket*)))
    (message "~A~%~A~%~A" (or (title song) "Unknown") (or (artist song) "")
             (or (album song) ""))
    (or (title song) "Unknown")))

;; Database

(define-mpd-command update-database () ()
  (database-update *socket* "")
  (message "Starting database update."))

;; Browsing

(defclass mpd-file ()
  ((path :initarg :path :accessor path)))

(defclass mpd-playlist ()
  ((path :initarg :path :accessor path)))

(defclass mpd-directory ()
  ((path :initarg :path :accessor path)))

(defun group-files (lst &optional acc)
  "Groups together files/directories/playlists in the format (path instance)"
  (if lst
      (let ((key (car lst))
            (value (cadr lst)))
        (group-files (cddr lst)
                     (cons (list value (make-instance
                                          (intern (concatenate 'string "MPD-"
                                                               (string key)))
                                          :path value)) acc)))
      (reverse acc)))

(define-mpd-command search-database () ()
  (let* ((files (group-files (database-list-all *socket* "")))
         (selection (second (select-from-menu (current-screen) files))))
    (typecase selection
      (mpd-file (add-song (path selection)))
      (mpd-playlist (load-playlist (path selection)))
      (mpd-directory (add-song (path selection)))
      (t (message "Abort.")))))

;; Keybindings

(defvar *mpd-playback-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") "reconnect")
    (define-key map (kbd "u") "update-database")
    (define-key map (kbd "v") "set-volume")
    (define-key map (kbd "p") "pause/resume")
    (define-key map (kbd "P") "play")
    (define-key map (kbd "<") "previous-song")
    (define-key map (kbd ">") "next-song")
    (define-key map (kbd "s") "stop")
    (define-key map (kbd "c") "clear-songs")
    (define-key map (kbd "S") "current-song")
    (define-key map (kbd "b") "search-database")
    map))
