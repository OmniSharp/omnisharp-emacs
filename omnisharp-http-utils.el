;; -*- lexical-binding: t; -*-

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'dash)

(defun omnisharp--get-host ()
  "Makes sure omnisharp-host is ended by / "
  (if (string= (substring omnisharp-host -1 ) "/")
      omnisharp-host
    (concat omnisharp-host "/")))

(defun omnisharp--get-api-url (api-name)
  (concat (omnisharp--get-host) api-name))

(defun omnisharp-post-http-message (url callback &optional params async)
  "Post http request to server. Return result."
  (omnisharp--submit-request (omnisharp--get-api-url url) callback params async))

(defun omnisharp--submit-request (url callback &optional params async)
  (if (require 'request nil 'noerror)
      (lexical-let* ((c callback))
        (request url
                 :type "POST"
                 :parser 'json-read
                 :sync (not async)
                 :data (json-encode params)
                 :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                       (message "Error from %s : %S" url error-thrown)))
                 :complete
                 (lambda (&rest _)
                   (when omnisharp-debug
                     (message "Request completed")))
                 :success (cl-function (lambda (&key data &allow-other-keys)
                                         (progn
                                           (when c
                                             (funcall c data))
                                           (when omnisharp-debug
                                             (message "Request succeeded"))
                                           )))
                 :status-code '((404 . (lambda (&rest _) (message (format "Endpoint %s does not exist." url))))
                                (500 . (lambda (&rest _) (message (format "Error from  %s." url))))
                                )))
    (message "ERROR: You must install 'request-deferred' package")))

(provide 'omnisharp-http-utils)
