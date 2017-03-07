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

;; Playback

(defcommand set-volume (volume) ((:string "Volume (1-100): "))
  "Sets the volume."
  (auto-reconnect *socket*
    (playback-set-volume *socket* volume)))

(defcommand pause/resume () ()
  "Pauses the music."
  (auto-reconnect *socket*
    (let ((state (state (query-status *socket*))))
      (if (string= state "pause")
          (playback-pause *socket* nil)
          (playback-pause *socket* t)))))

(defcommand play (&optional song-position) ((:string "Position: "))
  "Plays at the song position if specified."
  (auto-reconnect *socket*
    (playback-play *socket* song-position)))

(defcommand previous-song () ()
  "Plays the previous track."
  (auto-reconnect *socket*
    (playback-previous *socket*)))

(defcommand next-song () ()
  "Plays the next track."
  (auto-reconnect *socket*
    (playback-next *socket*)))

(defcommand stop () ()
  "Stops the playback."
  (auto-reconnect *socket*
    (playback-stop *socket*)))

;; Playlist

(defcommand add-song (song) ((:string "File: "))
  "Adds the file to the playlist."
  (auto-reconnect *socket*
    (playlist-add-song *socket* song)))

(defcommand clear-songs () ()
  "Clears all the songs in the current playlist."
  (auto-reconnect *socket*
    (playlist-clear-songs *socket*)))

(defcommand delete-song (song) ((:string "File: "))
  "Removes the song from the playlist."
  (auto-reconnect *socket*
    (playlist-delete-song *socket* song)))

;; Queries

(defcommand current-song () ()
  "Shows the current song."
  (auto-reconnect *socket*
    (let ((song (query-song *socket*)))
      (message "~A~%~A~%~A" (or (title song) "Unknown") (or (artist song) "")
               (or (album song) "")))))

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
