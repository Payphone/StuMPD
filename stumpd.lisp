;;;; stumpd.lisp

(in-package #:stumpd)

;; Utilities

(defun escape-spaces (string)
  "Inserts a backslash before every space in a string."
  (coerce (loop for char across string
             if (char= char #\Space) collect #\\
             collect char) 'string))

(defun 2cons (elt1 elt2 lst)
  "Conses two elements to a list."
  (cons elt1 (cons elt2 lst)))

(defun quote-string (string)
  "Encloses a string in double quotes."
  (concatenate 'string "\"" string "\""))

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
  (if (string= (state (query-status *socket)) "pause")
      (progn (playback-pause *socket* nil) (message "Resumed."))
      (progn (playback-pause *socket* t) (message "Paused."))))

(define-mpd-command play (&optional song-position)
    ((:string "Position (Optional): "))
  (playback-play *socket* song-position))

(define-mpd-command previous-song () ()
  (playback-previous *socket*)
  (current-song))

(define-mpd-command next-song () ()
  (playback-next *socket*)
  (current-song))

(define-mpd-command stop () ()
  (playback-stop *socket*)
  (message "Stopped."))

;; Playlist

(define-mpd-command add-song (song) ((:string "File: "))
  (playlist-add-song *socket* (quote-string song))
  (message "Song added."))

(define-mpd-command clear-songs () ()
  (playlist-clear-songs *socket*)
  (message "Songs cleared."))

(define-mpd-command delete-song (song) ((:string "File: "))
  (playlist-delete-song *socket* song)
  (message "Song deleted."))

;; Queries

(define-mpd-command current-song () ()
  (let ((song (query-song *socket*)))
    (message "~A~%~A~%~A" (or (title song) "Unknown") (or (artist song) "")
             (or (album song) ""))
    (or (title song) "Unknown")))

;; Browsing

(defclass mpd-file ()
  ((last-modified :initarg :last-modified :accessor last-modified)
   (size :initarg :size :accessor size)
   (name :initarg :name :accessor name)))

(defclass mpd-file-directory ()
  ((last-modified :initarg :last-modified :accessor last-modified)
   (size :initarg :size :accessor size)
   (name :initarg :name :accessor name)))

(defun group-files (lst &optional build acc)
  "Creates a list of parameter lists containing a file/directory."
  (if lst
      (let ((key (car lst))
            (value (cadr lst)))
        (if (or (string= key 'file) (string= key 'directory))
            (group-files (cddr lst) nil
                         (append (list (2cons key value build)) acc))
            (group-files (cddr lst) (2cons key value build) acc)))
      (reverse acc)))

(defun browse-directory (directory)
  (let ((files (group-files (database-list-files *socket*
                              (quote-string (escape-spaces directory))))))
    (select-from-menu (current-screen)
      (loop for file in files
         collect
           (let* ((filep (getf file 'file))
                  (name (if filep (getf file 'file) (getf file 'directory)))
                  (last-modified (getf file 'last-modified))
                  (size (getf file 'size)))
             (cons name (make-instance (if filep 'mpd-file 'mpd-file-directory)
                                       :last-modified last-modified
                                       :size size
                                       :name name))))
      "Browse: ")))

(define-mpd-command browse-menu-directory (&optional path) ((:string "Path: "))
  (let* ((selection (browse-directory path))
         (filename (car selection))
         (file (cdr selection)))
    (cond ((string= (type-of file) 'mpd-file-directory)
           (browse-menu-directory
            (concatenate 'string (if (string= path "") path
                                     (concatenate 'string path "/")) filename)))
          ((string= (type-of file) 'mpd-file)
           (add-song (concatenate 'string path "/" filename))
           (message "Added song to playlist."))
          (t (message "Abort.")))))

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
    (define-key map (kbd "b") "browse-menu-directory")
    map))
