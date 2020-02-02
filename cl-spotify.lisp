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

(defparameter *client-file* (asdf:system-relative-pathname
                             :cl-spotify ".spotify-client"))

(defparameter *auth-file* (asdf:system-relative-pathname
                           :cl-spotify ".spotify-auth"))

(defparameter *close-html* (asdf:system-relative-pathname
                            :cl-spotify "close.html"))

(defparameter *auth-server* nil)
(defparameter *auth-state* nil)
(defparameter *auth-redirect-url* "http://localhost:4040/")

(defparameter *auth-code* nil)
(defparameter *auth-json* nil)


(defun auth-request-header ()
  (with-input-from-file (ins *client-file*)
    (let ((json (read-json ins)))
      (cons
       "Authorization"
       (format
        nil
        "Basic ~a"
        (base64:string-to-base64-string
         (format nil "~a:~a"
                 (getjso "client_id" json)
                 (getjso "client_secret" json))))))))

(defun get-client-id ()
  (with-input-from-file (ins *client-file*)
    (let ((json (read-json ins)))
      (getjso "client_id" json))))

(hunchentoot:define-easy-handler (register :uri "/") (code state)
  (when (not (string= *auth-state* state))
    (error "State is not equal!"))

  (setf (hunchentoot:content-type*) "text/html")
  (setf *auth-code* code)
  (format t "Auth code is: ~a~%" *auth-code*)

  (let* ((res (flexi-streams:octets-to-string
               (drakma:http-request "https://accounts.spotify.com/api/token"
                                    :external-format-in  :utf-8
                                    :decode-content  t
                                    :method :post
                                    :additional-headers (list (auth-request-header))
                                    :content (drakma::alist-to-url-encoded-string
                                              (list (cons "grant_type" "authorization_code")
                                                    (cons "code" *auth-code*)
                                                    (cons "redirect_uri" *auth-redirect-url*)
                                                    )
                                              :utf-8
                                              #'drakma:url-encode))))
         (json-token (read-json-from-string res)))

    (setf (getjso "expire_time" json-token)
          (format-timestring nil
                             (timestamp+
                              (local-time:now)
                              (- (getjso "expires_in" json-token) 5)
                              :sec)))
    (format t "~a~%" json-token)
    (with-output-to-file (outs *auth-file*)
      (format outs "~a" json-token))
    (setf *auth-json* json-token)
    (alexandria:read-file-into-string *close-html*)))


(defun authorize ()
  (when (not (uiop:file-exists-p *client-file*))
    (error (format nil "Cannot read client information from ~s" *client-file*)))

  (setf *auth-state* (format nil "~a" (random 10000000000)))
  (let ((auth-url (format nil
                          "https://accounts.spotify.com/authorize?~a"
                          (drakma::alist-to-url-encoded-string
                           (list (cons "response_type" "code")
                                 (cons "client_id" (get-client-id))
                                 (cons "state" *auth-state*)
                                 (cons "redirect_uri" "http://localhost:4040/"))
                           :utf-8
                           #'drakma:url-encode))))
    (setf *auth-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4040)))
    (swank:eval-in-emacs (list 'eww-browse-with-external-browser auth-url))))

(defun check-expiration (auth)
  (let ((exp-time (parse-timestring (getjso "expire_time" auth)))
        (ref-token (getjso "refresh_token" auth)))

    (cond ((timestamp< exp-time (local-time:now))
           (let* ((req-data (drakma::alist-to-url-encoded-string
                             (list (cons "grant_type" "refresh_token")
                                   (cons "refresh_token" ref-token))
                             :utf-8
                             #'drakma:url-encode))
                  (res (flexi-streams:octets-to-string
                        (drakma:http-request "https://accounts.spotify.com/api/token"
                                             :external-format-in  :utf-8
                                             :additional-headers (list (auth-request-header))
                                             :decode-content  t
                                             :method :post
                                             :content req-data)))
                  (json-token (read-json-from-string res)))
             (setf (getjso "expire_time" json-token)
                   (format-timestring nil
                                      (timestamp+
                                       (local-time:now)
                                       (- (getjso "expires_in" json-token) 5)
                                       :sec)))
             (format t "~a~%" json-token)
             (with-output-to-file (outs *auth-file* :if-exists :supersede)
               (format outs "~a" json-token))
             (setf *auth-json* json-token)))
          (t
           auth))))

(defun ensure-authorized ()
  (cond
    ;; Auth object exists, so just check for expiration and continue
    (*auth-json*
     (setf *auth-json* (check-expiration *auth-json*))
     *auth-json*)

    ;; No authorization file or JSON object
    ;; Not authorized yet, request authorization
    ((and (null *auth-json*)
          (not (uiop:file-exists-p *auth-file*)))
     (authorize))

    ;; Authorization file exists, but no object
    ;; Possibly this is the first call after a Lisp restart or right after authorizing
    ;; Load auth file and check for expiration
    ((and (null *auth-json*)
          (uiop:file-exists-p *auth-file*))
     ;; If authorization just finished the server listening for redirects may still
     ;; be around, so stop it
     (when *auth-server*
       (hunchentoot:stop *auth-server*)
       (setf *auth-server* nil))

     ;; Read authorization file and check for expiration
     (with-input-from-file (ins *auth-file*)
       (setf *auth-json* (check-expiration (read-json ins))))
     *auth-json*)

    (t
     (error "Unusual authorization state!"))))

(defun auth-header ()
  (cons
   "Authorization"
   (format
    nil
    "~a ~a"
    (getjso "token_type" *auth-json*)
    (getjso "access_token" *auth-json*))))

(defun get-with-token (url)
  (ensure-authorized)
  (st-json:read-json-from-string
   (flexi-streams:octets-to-string
    (drakma:http-request url
                         :external-format-in  :utf-8
                         :additional-headers (list (auth-header))
                         :method :get))))

(defun get-user-info ()
  (get-with-token "https://api.spotify.com/v1/me"))


