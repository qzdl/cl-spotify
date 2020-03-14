;;;; cl-spotify.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-spotify)



(defparameter *debug-print-stream* nil
  "Debug setting to print low level HTTP results.")

(defparameter *global-connection* nil
  "The global Spotify connection.")



(defun has-scope-p (scope &optional (connection *global-connection*))
  (find scope (scope connection)))

(defun add-scope (new-scope &optional (connection *global-connection*))
  (cond ((has-scope-p new-scope connection )
         (format *debug-print-stream* "Already have scope ~a~%" new-scope)
         connection)
        (t
         (push new-scope (scope connection))
         (disconnect connection)
         ;; Set *initializing-connection* so that the HTTP callback can access the connection
         (setf *initializing-connection* connection)

         ;; Start HTTP listener to respond to callback
         (setf (auth-server connection)
               (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port (listen-port connection))))
         ;; Open the URL authorization URL in the user's browser.
         ;; TODO: Don't use Swank...
         (swank:eval-in-emacs '(require 'eww))
         (swank:eval-in-emacs (list 'eww-browse-with-external-browser (get-auth-url connection)))
         ;; Return the new connection
         connection)))

(defun rm-scope (rm-scope &key (connection *global-connection*))
  (cond ((not (has-scope-p connection rm-scope))
         (format *debug-print-stream* "Don't have scope ~a~%" rm-scope)
         connection)
        (t
         (setf (scope connection) (remove rm-scope (scope connection)))
         (disconnect connection)
         (connect))))



(defun user-id (&optional (connection *global-connection*))
  (getjso "id" (user-info connection)))

(defun user-info (&optional (connection *global-connection*))
  (with-slots (user-info) connection
    (when (null user-info)
      (setf user-info (spotify-get-json connection "https://api.spotify.com/v1/me")))
    user-info))

(defun get-playlists (&key (connection *global-connection*))
  (sget
   (format nil "https://api.spotify.com/v1/users/~a/playlists" (user-id connection))
   connection))

(defun now-playing (&key (connection *global-connection*))
  (sget
   "https://api.spotify.com/v1/me/player/currently-playing"
   connection))

(defun collect-fields (object field-names)
  (loop for field-name in (ensure-list field-names)
       collecting (getjso* field-name object)))

(defun play (&key (connection *global-connection*))
  (sput "https://api.spotify.com/v1/me/player/play" connection))

(defun pause (&key (connection *global-connection*))
  (sput "https://api.spotify.com/v1/me/player/pause" connection))

(defun next (&key (connection *global-connection*))
  (spost "https://api.spotify.com/v1/me/player/next" connection))

(defun previous (&key (connection *global-connection*))
  (spost "https://api.spotify.com/v1/me/player/previous" connection))

(defun iter-backward (iterator &key (connection *global-connection*))
  (when-let (previous (getjso "previous" iterator))
    (sget connection previous)))

(defun iter-forward (iterator &key (connection *global-connection*))
  (when-let (forward (getjso "next" iterator))
    (sget connection forward)))

(defun iter-items (iterator)
  (getjso "items" iterator))
