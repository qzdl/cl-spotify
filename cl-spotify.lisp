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

;; Spotify client information
;; This file contains the client information received when registering the application
;; with Spotify.  It is required to access the Spotify API.
(defparameter *client-file* (asdf:system-relative-pathname :cl-spotify ".spotify-client")
  "Path to a JSON file containing an object with client_id and client_secret entries.")

;; Cached authorization
;; This file contains a cached version of the JSON object received during authorization.
(defparameter *auth-file* (asdf:system-relative-pathname :cl-spotify ".spotify-auth")
  "Path to the auth token cache file.")

;; HTML shown to the user after authentication.
(defparameter *close-html* (asdf:system-relative-pathname
                            :cl-spotify "html/close.html")
  "Path to the HTML file that the user is redirected to after authenticating.")


(defparameter *debug-print-stream* nil
  "Debug setting to print low level HTTP results.")

(defparameter *initializing-connection* nil
  "The spotify-connection that initiated authentication.  This object will be updated after \
access is granted and Spotify redirects the user to our local server.")

(defun auth-request-header ()
  "Read client ID and secret from .spotify-client and return an authorization header."
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
  "Return the client_id from .spotify-client."
  (with-input-from-file (ins *client-file*)
    (let ((json (read-json ins)))
      (getjso "client_id" json))))

(defclass spotify-connection ()
  ((auth-state :initarg :auth-state :initform (format nil "~a" (random 10000000000)) :accessor auth-state)
   (listen-port :initarg :listen-port :initform 4040 :accessor listen-port)
   (redirect-url :initarg :redirect-url :accessor redirect-url)
   (scope :initarg :scope :initform "" :accessor scope)
   (auth-server :initform nil :accessor auth-server)
   (stream :initform nil)
   (cookies :initform (make-instance 'drakma:cookie-jar) :accessor cookies)
   (expiration :initform nil :accessor expiration)
   (auth-header :initarg :auth-header :initform nil :type cons :accessor auth-header)
   (auth-token :initarg :auth-token :initform nil :accessor auth-token)
   (user-info :initform nil))
  (:documentation "A connection to the Spotify API."))

(defun create-auth-header (json-token)
  "Create an HTTP authentication header from a JSON authentication token."
  (cons "Authorization"
        (format
         nil
         "~a ~a"
         (getjso "token_type" json-token)
         (getjso "access_token" json-token))))

(defun connect (&key (scope "") (port 4040))
  "Create an authenticated Spotify connection.  Initiate authentication, if necessary."

  (cond ((not (uiop:file-exists-p *client-file*))
         ;; Can't find client identification file
         (error (format nil "Cannot read client information from ~s" *client-file*)))

        ((uiop:file-exists-p *auth-file*)
         ;; Already authenticated, so a connection using the cached token
         (let ((atoken (read-auth-token)))
           (refresh-connection (make-instance 'spotify-connection
                                              :scope scope
                                              :listen-port port
                                              :redirect-url (format nil "http://localhost:~a/" port)
                                              :auth-token atoken
                                              :auth-header (cons
                                                            "Authorization"
                                                            (format nil
                                                                    "~a ~a"
                                                                    (getjso "token_type" atoken)
                                                                    (getjso "access_token" atoken)))))))

        (t
         ;; No cached token, so initiate authentication
         (let* (
                ;; The new connection
                (spotify-conn (make-instance 'spotify-connection
                                             :scope scope
                                             :listen-port port
                                             :redirect-url (format nil "http://localhost:~a/" port)))

                ;; Headers required to initiate authentication.   See this page:
                ;; https://developer.spotify.com/documentation/general/guides/authorization-guide/
                ;; for more information
                (header-list (list (cons "response_type" "code")
                                   (cons "client_id" (get-client-id))
                                   (cons "scope" (scope spotify-conn))
                                   (cons "state" (auth-state spotify-conn))
                                   (cons "redirect_uri" (redirect-url spotify-conn))))

                (auth-url (format nil "https://accounts.spotify.com/authorize?~a"
                                  (drakma::alist-to-url-encoded-string header-list
                                                                       :utf-8
                                                                       #'drakma:url-encode))))

           ;; Set *initializing-connection* so that the HTTP callback can access the connection
           (setf *initializing-connection* spotify-conn)

           ;; Start HTTP listener to respond to callback
           (setf (auth-server spotify-conn)
                 (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port (listen-port spotify-conn))))

           ;; Open the URL authentication URL in the user's browser.
           ;; TODO: Don't use Swank...
           (swank:eval-in-emacs '(require 'eww))
           (swank:eval-in-emacs (list 'eww-browse-with-external-browser auth-url))

           ;; Return the new connection
           spotify-conn))))

(define-condition authentication-error (error)
  ((error :initarg :error :reader auth-error)
   (error-description :initarg :description :reader description))
  (:documentation "An authentication error."))

(define-condition regular-error (error)
  ((status :initarg :status :reader status)
   (message :initarg :message :reader message))
  (:documentation "Generic Spotify API error."))

(define-condition http-error (error)
  ((code :initarg :code :reader code)
   (headers :initarg :headers :reader headers)
   (url :initarg :url :reader url)
   (message :initarg :message :reader message))
  (:documentation "An HTTP error."))


(defun save-auth-token (json-token)
  "Save json-token to the authentication token cache file."
  (with-output-to-file (outs *auth-file* :if-exists :supersede)
    (format outs "~a" json-token))
  json-token)

(defun read-auth-token ()
  "Read a saved authentication token from the cache file."
  (with-input-from-file (ins *auth-file*)
    (st-json:read-json ins)))

(defun get-auth-token (code connection)
  "Request an authentication token from Spotify."
  (with-slots (redirect-url) connection
    (let ((content (drakma::alist-to-url-encoded-string
                    (list (cons "grant_type" "authorization_code")
                          (cons "code" code)
                          (cons "redirect_uri" redirect-url))
                    :utf-8
                    #'drakma:url-encode)))
      (spotify-get-json connection
                        "https://accounts.spotify.com/api/token"
                        :type :post
                        :extra-headers (list (auth-request-header))
                        :content content
                        :in-init t))))

(hunchentoot:define-easy-handler (authorize :uri "/") (code state)
  "Spotify redirects the user here when authentication is granted.  This handler \
finishes initialization of *initializing-connection*."

  (with-slots (redirect-url auth-state auth-token auth-header) *initializing-connection*

    ;; Check state variable, used to validate requests
    (when (not (string= auth-state state))
      (error "State does not match!"))
    (let ((json-token (get-auth-token code *initializing-connection*)))
    ;; Convert relative expires_in into absolute expire_time and store it in the JSON object

      (setf (getjso "expire_time" json-token)
            (format-timestring nil
                               (timestamp+
                                (local-time:now)
                                (- (getjso "expires_in" json-token) 5)
                                :sec)))
      (save-auth-token json-token)
      (setf auth-token json-token)
      (setf auth-header (create-auth-header json-token)))
    (setf *initializing-connection* nil)

    ;; Return HTML telling the user they are authorized and can close the browser window.
    (setf (hunchentoot:content-type*) "text/html")
    (alexandria:read-file-into-string *close-html*)))

(defun user-id (connection)
  (getjso "id" (user-info connection)))

(defun http-error-lookup (code)
  (assoc-value
   '((200 . "The request has succeeded. The client can read the result of \
the request in the body and the headers of the response.")

    (201 . "Created - The request has been fulfilled and resulted in a \
new resource being created.")

    (202 . "Accepted - The request has been accepted for processing, but \
the processing has not been completed.")

    (204 . "No Content - The request has succeeded but returns no message \
body.")

    (304 . "Not Modified. See Conditional requests.")

    (400 . "Bad Request - The request could not be understood by the server \
due to malformed syntax. The message body will contain more information;
see Response Schema.")

    (401 . "Unauthorized - The request requires user authentication or, if \
the request included authorization credentials, authorization has been \
refused for those credentials.")

    (403 . "Forbidden - The server understood the request, but is refusing \
to fulfill it.")

    (404 . "Not Found - The requested resource could not be found. This error \
can be due to a temporary or permanent condition.")

    (429 . "Too Many Requests - Rate limiting has been applied.")

    (500 . "Internal Server Error. You should never receive this error \
because our clever coders catch them all … but if you are unlucky enough \
to get one, please report it to us through a comment at the bottom of this \
page.")

    (502 . "Bad Gateway - The server was acting as a gateway or proxy and \
received an invalid response from the upstream server.")

    (503 . "Service Unavailable - The server is currently unable to handle \
the request due to a temporary condition which will be alleviated after some \
delay. You can choose to resend the request again."))

   code))

(defun check-error (json-response)
  (cond
    ((getjso "status" json-response)
     (error 'regular-error
            :status (getjso "status" json-response)
            :message (getjso "message" json-response)))
    ((getjso "error" json-response)
         (error 'authentication-error
                :error (getjso "error" json-response)
                :description (getjso "error_description" json-response)))
    (t
     json-response)))

(defun check-cleanup (spotify-connection)
  (with-slots (auth-server stream) spotify-connection
    (when auth-server
      (hunchentoot:stop auth-server)
      (setf auth-server nil)
      (close stream)
      (setf stream nil))))

(defun spotify-get-json (connection url &key extra-headers keep-alive (type :get) (content nil) (in-init nil))
  (when (null in-init)
    (check-cleanup connection)
    (refresh-connection connection))
  (loop
     for attempts below 3
     do
       (with-slots (auth-header stream cookies retries) connection
         (handler-case
             (multiple-value-bind (body resp-code headers url req-stream must-close response)
                 (drakma:http-request
                  url
                  :method type
                  :keep-alive keep-alive
                  :close nil
                  :accept "application/json"
                  :additional-headers (concatenate 'list (list auth-header) (ensure-list extra-headers))
                  :user-agent "cl-spotify"
                  :want-stream nil
                  :content content
                  :stream stream
                  :cookie-jar cookies)
               (declare (ignorable body))
               (when *debug-print-stream*
                 (format *debug-print-stream*
                         "Headers:~%~a~%Response Code: ~a~%Response: ~a~%URL: ~a~%~%"
                         headers resp-code response url))
               (unwind-protect
                    (cond ((= resp-code 200)
                           (setf stream req-stream)
                           (let ((json-response (read-json-from-string (flexi-streams:octets-to-string body :external-format :utf-8))))
                             (check-error json-response)
                             (return-from spotify-get-json json-response)))
                          (t
                           (error 'http-error
                                  :code resp-code
                                  :headers headers
                                  :url url
                                  :message (http-error-lookup resp-code))))
                 (when must-close
                   (close req-stream)
                   (setf stream nil))))
           (drakma:drakma-error (err)
             (declare (ignorable err))
             (when *debug-print-stream*
               (format *debug-print-stream* "Keep alive expired!~%"))
             (reset-connection connection))))))

(defun expired-p (connection)
  (with-slots (auth-token) connection
    (let ((exp-time (parse-timestring (getjso "expire_time" auth-token))))
      (timestamp< exp-time (local-time:now)))))

(defun reset-connection (connection)
  (with-slots (stream) connection
    (when stream
      (close stream)
      (setf stream nil))))

(defun refresh-connection (connection)
  (with-slots (auth-token auth-header) connection
    (let ((exp-time (parse-timestring (getjso "expire_time" auth-token)))
          (ref-token (getjso "refresh_token" auth-token)))

      (when (timestamp< exp-time (local-time:now))
        (reset-connection connection)
        (let* ((req-data (drakma::alist-to-url-encoded-string
                          (list (cons "grant_type" "refresh_token")
                                (cons "refresh_token" ref-token))
                          :utf-8
                          #'drakma:url-encode))
               (my-nil (when *debug-print-stream*
                         (format *debug-print-stream* "req-data: ~a~%" req-data)))
               (res (flexi-streams:octets-to-string
                     (drakma:http-request "https://accounts.spotify.com/api/token"
                                          :external-format-in  :utf-8
                                          :additional-headers (list (auth-request-header))
                                          :decode-content  t
                                          :method :post
                                          :content req-data)))
               (json-token (read-json-from-string res)))
          (declare (ignorable my-nil))
          (when (getjso "refresh_token" json-token)
            (setf (getjso "refresh_token" auth-token)
                  (getjso "refresh_token" json-token)))
          (when *debug-print-stream*
            (format *debug-print-stream* "Refresh token: ~a~%" json-token))
          (setf (getjso "expire_time" json-token)
                (format-timestring nil
                                   (timestamp+
                                    (local-time:now)
                                    (- (getjso "expires_in" json-token) 5)
                                    :sec)))
          (when *debug-print-stream*
            (format *debug-print-stream* "json-token: ~a~%" json-token)
            (format *debug-print-stream* "auth-token: ~a~%" auth-token))
          (save-auth-token auth-token)
          ;; (setf auth-token json-token)
          (setf auth-header (create-auth-header json-token))))))
  connection)

(defun disconnect (connection)
  (with-slots (auth-server stream) connection
    (when auth-server
      (hunchentoot:stop auth-server))
    (when stream
      (close  stream))))

(defun user-info (connection)
  (with-slots (user-info) connection
    (when (null user-info)
      (setf user-info (spotify-get-json connection "https://api.spotify.com/v1/me")))
    user-info))

(defun get-playlists (connection)
  (spotify-get-json
   connection
   (format nil "https://api.spotify.com/v1/users/~a/playlists" (user-id connection))))


(defun go-backward (connection iterator)
  (when-let (previous (getjso "previous" iterator))
    (spotify-get-json connection previous)))

(defun go-forward (connection iterator)
  (when-let (forward (getjso "next" iterator))
    (spotify-get-json connection forward)))

(defun items (iterator)
  (getjso "items" iterator))
