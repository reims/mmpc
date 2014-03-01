;; mmpc.el --- Minibuffer Music Player Client

;; Copyright (C) 2014 Reimer Backhaus

;; Author: Reimer Backhaus <rbackhaus@gmail.com>
;; Created: 24 Jan 2014
;; Keywords: mpd
;; Version: 0.1
;; Package-Requires: ((libmpdee "2.1"))

;; This file is not part of GNU Emacs.

;; ----------------------------------------------------------------------------
;; "THE BEER-WARE LICENSE" (Revision 42):
;; <rbackhaus@gmail.com> wrote this file. As long as you retain this notice you
;; can do whatever you want with this stuff. If we meet some day, and you think
;; this stuff is worth it, you can buy me a beer in return. Reimer Backhaus
;; ----------------------------------------------------------------------------

;;; Commentary:
;; This file defines commands to interact with the Music Player Daemon
;; (http://www.musicpd.org/). It does not create or use any buffers
;; (except ones that may be created by Emacs completion). All interaction
;; is done via the minibuffer. Hence the name.

;;; Code:

(require 'libmpdee)
(require 'cl-lib)

(defconst mmpc-version "0.1"
  "mmpc's version")

(defvar mmpc-connection nil
  "mmpc's connection to the mpd")

(defun mmpc-connect (&optional host port)
  "Create a connection to the mpd at the host HOST on port PORT.

HOST defaults to 127.0.0.1 and PORT to 6600. The connection is created
with auto-connect. Thus it tries to connect to mpd if it is
disconnected and a command is executed."
  (interactive (list
		(read-string "Host: " "127.0.0.1")
		(read-minibuffer "Port: " (format "%d" 6600))))
 (let ((host (or host "127.0.0.1")) ;; default values for non-interactive call
       (port (or port 6600)))
   (when mmpc-connection
       (ignore-errors
	 (mpd-close-connection mmpc-connection)))
   (setq mmpc-connection (mpd-conn-new host port 0))))

(defmacro when-mmpc-connected (&rest body)
  "Evaluate BODY in a `progn' if mmpc is connected. 
Print \"not connected to mpd\" otherwise."
  (declare (indent defun))
  `(if mmpc-connection
       (progn ,@body)
     (message "not connected to mpd")))

(defun mmpc-play ()
  "Start mpd playback."
  (interactive)
  (when-mmpc-connected
    (mpd-play mmpc-connection)))

(defun mmpc-pause ()
  "Pause mpd playback."
  (interactive)
  (when-mmpc-connected
    (mpd-pause mmpc-connection)))

(defun mmpc-display-current-song ()
  "Display the title, album and artist of the current song."
  (interactive)
  (when-mmpc-connected
    (let ((current-song (mpd-get-current-song mmpc-connection)))
      (message (format "MPD Current Song: %s - %s from %s"
		       (plist-get current-song 'Artist)
		       (plist-get current-song 'Title)
		       (plist-get current-song 'Album))))))

(defun mmpc-next-song ()
  "Play the next song in the mpd playlist."
  (interactive)
  (when-mmpc-connected
    (mpd-next mmpc-connection)))

(defun mmpc-prev-song ()
  "Play the previous song the int mpd playlist."
  (interactive)
  (when-mmpc-connected
    (mpd-prev mmpc-connection)))

(defun mmpc-read-symbol (prompt symbols)
  "Read one symbol contained in SYMBOLS"
  (let ((symbol-map (cl-map 'list 
			    (lambda (s) (cons (symbol-name s) s)) 
			    symbols))) 
    (cdr (assoc (completing-read prompt 
				 symbol-map
				 nil
				 t)
		symbol-map))))

(defconst mmpc-get-entities-methods
  '(path artist album)
  "List of symbols representing the different methods to get entities
  from the mpd db.")

(defun mmpc-read-get-entities-method ()
  "Read a symbol contained in `mmpc-get-entities-methods'."
  (mmpc-read-symbol "Get entities by: " mmpc-get-entities-methods))

(defun mmpc-get-all-entries-for-tag (tag)
  "Return a list containing all entries for tag TAG in the mpd db."
  (when-mmpc-connected
    (let ((command-result (mpd-execute-command mmpc-connection (format "list %s" tag))))
      (when (car command-result)
	(cl-map 'list 'cdr (cdr command-result))))))

(defun mmpc-get-all-artists ()
  "Return a list of strings that contains all artist names in the mpd db."
  (mmpc-get-all-entries-for-tag "artist"))

(defun mmpc-get-all-albums ()
  "Return a list of strings that contains all album names in the mpd db."
  (mmpc-get-all-entries-for-tag "album"))

(defun mmpc-read-artist ()
  "Read an artist from the mpd db."
  (completing-read "Artist: "
		   (mmpc-get-all-artists)
		   nil
		   t))

(defun mmpc-read-album ()
  "Read an album from the mpd db."
  (completing-read "Album: "
		   (mmpc-get-all-albums)
		   nil
		   t))

(defun mmpc-get-path-from-resources (resources)
  "Helper function to extract the file paths in RESOURCES"
  (cl-remove-if 'null
		(cl-map 'list (lambda (res)
				(cond
				 ((stringp res) res)
				 ((listp res) (plist-get res 'file))
				 (t nil)))
			resources)))

(defun mmpc-get-entities-for-artist (artist)
  "Get all files from ARTIST with `mpd-search'"
  (when-mmpc-connected
    (mmpc-get-path-from-resources (mpd-search mmpc-connection 'artist artist))))

(defun mmpc-get-entities-for-album (album)
  "Get all files from ALBUM with `mpd-search'"
  (when-mmpc-connected
    (mmpc-get-path-from-resources (mpd-search mmpc-connection 'album album))))

(defun mmpc-path-completion (partial)
  "Helper function to auto-complete path arguments for mmpc commands"
  (let* ((parts (split-string partial "/"))
	 (path (combine-and-quote-strings (butlast parts) "/"))
	 (rest (or (last parts) ""))
	 (result nil))
    (when-mmpc-connected
      (mpd-get-directory-info mmpc-connection
			      path
			      (lambda (object type)
				(cl-ecase type
				  ('file (setf result (cons (plist-get object 'file) result)))
				  ('directory (setf result (cons object result)))
				  ('playlist nil))))
      (message nil)) ; to suppress the "connected to mpd" message
    result))

(defun mmpc-read-path ()
  (list
    (completing-read "Path: "
		     (dynamic-completion-table 'mmpc-path-completion)
		     nil
		     t)))

(defconst mmpc-entities-to-read-function
  '((artist . mmpc-read-artist)
    (album . mmpc-read-album)
    (path . mmpc-read-path))
  "Alist associating entities type with read functions")

(defconst mmpc-entities-to-get-function
  '((artist . mmpc-get-entities-for-artist)
    (album . mmpc-get-entities-for-album)
    (path . identity))
  "Alist associating entities type with get functions")

(defun mmpc-read-entities ()
  "Reads mpd entities from user input.
First it reads the type of entity and then calls the corresponding
read and get function."
  (let* ((entity-type (mmpc-read-get-entities-method))
	 (read-function (cdr (assoc entity-type mmpc-entities-to-read-function)))
	 (get-function (cdr (assoc entity-type mmpc-entities-to-get-function))))
    (when (and read-function get-function)
      (funcall get-function (funcall read-function)))))

(defun mmpc-clear-playlist ()
  "Clear the mpd playlist."
  (interactive)
  (when-mmpc-connected
    (mpd-clear-playlist mmpc-connection)))

(defun mmpc-add (resources)
  "Add RESOURCES to the mpd playlist."
  (interactive (list (mmpc-read-entities)))
  (when-mmpc-connected
    (mpd-enqueue mmpc-connection resources)))

(defun mmpc-replace (resources)
  "Replace mpd playlist with RESOURCES."
  (interactive (list (mmpc-read-entities)))
  (mmpc-clear-playlist)
  (mmpc-add resources))

(defun mmpc-replace-and-play (resources)
  "Replace mpd playlist with RESOURCES and play."
  (interactive (list (mmpc-read-entities)))
  (mmpc-replace resources)
  (mmpc-play))

(defun mmpc-add-files (path)
  "Add all files in PATH to the mpd playlist.

PATH can be a directory to add all files in the directory 
or a single file."
  (interactive 
   (list
    (completing-read "Path: "
		     (dynamic-completion-table 'mmpc-path-completion)
		     nil
		     t)))
  (when-mmpc-connected
    (mpd-enqueue mmpc-connection path)))

(defun mmpc-replace-files (path)
  "Replace the current playlist with the files in PATH.

This is equivalent to calling `mmpc-add-files' with PATH after
`mmpc-clear-playlist'"
  (interactive 
   (list
    (completing-read "Path: " 
		     (dynamic-completion-table 'mmpc-path-completion)
		     nil
		     t)))
  (mmpc-clear-playlist)
  (mmpc-add-files path))

(defun mmpc-replace-files-and-play (path)
  "Replace the current playlist with the files in PATH and play them.

This is equivalent to calling `mmpc-replace-files' with PATH and then
`mmpc-play'"
  (interactive 
   (list
    (completing-read "Path: " 
		     (dynamic-completion-table 'mmpc-path-completion)
		     nil
		     t)))
  (mmpc-replace-files path)
  (mmpc-play))

;; thought I could wrap this in a function, but somehow this leads to a strange error:
;; "error in process filter: invalid function (lambda)"
;; maybe some strange effect caused by dynamic scope?? No idea.
;; The error only comes up after the first use.
(defmacro mmpc-read-pl-pos-by-title (&optional prompt-arg)
  (let ((prompt-value (or prompt-arg "Playlist entry:"))
	(prompt (gensym))
	(playlist-entries (gensym))
	(playlist-count (gensym))
	(add-to-entries (gensym))
	(chosen-entry (gensym)))
    `(let* ((,prompt ,prompt-value)
	    (,playlist-entries nil)
	    (,playlist-count 0)
	    (,add-to-entries (lambda (entry)
			      (setf ,playlist-entries (cons (cons (plist-get entry 'Title) ,playlist-count)
							   ,playlist-entries))
			      (incf ,playlist-count))))
       (when-mmpc-connected
	 (mpd-get-playlist-entry mmpc-connection nil ,add-to-entries)
	 (let ((,chosen-entry (completing-read ,prompt ,playlist-entries nil t)))
	   (cdr (assoc ,chosen-entry ,playlist-entries)))))))

(defun mmpc-play-pl-entry (pos)
  (interactive
   (list (mmpc-read-pl-pos-by-title)))
  (when-mmpc-connected
    (mpd-play mmpc-connection pos)))

(defun mmpc-update (&optional path)
  (interactive
   (list
    (completing-read "Path: " 
		     (dynamic-completion-table 'mmpc-path-completion)
		     nil
		     t)))
  (when-mmpc-connected
    (mpd-update mmpc-connection path)))

(provide 'mmpc)
