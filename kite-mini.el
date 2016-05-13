;;; kite-mini.el --- Remotely evaluate JavaScript in the WebKit debugger
;;
;; Copyright (c) 2014, 2015  Tung Dao <me@tungdao.com>
;;
;; Author: Tung Dao <me@tungdao.com>
;; URL: https://github.com/tungd/kite-mini.el
;; Keywords: webkit
;; Version: 0.2.0
;; Package-Requires: ((dash "2.11.0") (websocket "1.5"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Minor mode for remote evaluate JavaScript in WebKit debugger, with
;; a little icing. Included features are:
;; - Evaluate JavaScript (running at top level)
;; - Modify and update external JavaScript files (live)
;; - Reload
;;
;; Planned features includes:
;; - Live reload stylesheets (without reload)
;; - JavaScript console (REPL)
;;
;;; Code:

(require 'url)
(require 'json)
(require 'dash)
(require 'websocket)
(require 'sourcemap)


(defun kite-mini--get-url-content (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (prog1
        (buffer-string)
      (kill-buffer))))

(defcustom kite-mini-remote-host "127.0.0.1"
  "Default host for connection to WebKit remote debugging API."
  :group 'kite-mini)

(defcustom kite-mini-remote-port 9222
  "Default port for connection to WebKit remote debugging API."
  :group 'kite-mini)

(defvar kite-mini-socket nil
  "Websocket connection to WebKit remote debugging API.")

(defvar kite-mini-rpc-id 0)
(defvar kite-mini-rpc-callbacks nil)
(defvar kite-mini-rpc-scripts nil
  "List of JavaScript files available for live editing.")

(defvar kite-mini-project-root ""
  "Path to the project root. Used for sourcemap search.")

(defvar kite-mini-sourcemaps ()
  "List of sourcemap to track")

(defun kite-mini--project-path (path)
  (when (string-empty-p kite-mini-project-root)
    (warn "kite-mini-project-root is empty; use M-x kite-mini-set-project-root"))
  (concat kite-mini-project-root path))

(defun kite-mini-set-project-root (dir)
  "Set `kite-mini-project-root' for sourcemap search"
  ;; TODO: use user's default completion method
  (interactive (list (read-directory-name "Project root:" default-directory default-directory)))
  (setq kite-mini-project-root dir))

(defun kite-mini-encode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode data)))

(defun kite-mini-decode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string data)))

(defun kite-mini-next-rpc-id ()
  (setq kite-mini-rpc-id (+ 1 kite-mini-rpc-id)))


(defun kite-mini-register-callback (id fn)
  (let ((hook (intern (number-to-string id) kite-mini-rpc-callbacks)))
    (add-hook hook fn t)))

(defun kite-mini-dispatch-callback (id data)
  (let ((hook (intern (number-to-string id) kite-mini-rpc-callbacks)))
    (when hook
      (run-hook-with-args hook data)
      (unintern hook kite-mini-rpc-callbacks))))


(defun kite-mini-on-open (socket)
  (message "Kite: connected."))

(defun kite-mini-on-close (socket)
  (message "Kite: disconnected."))

(defun kite-mini-on-script-parsed (data)
  (let ((extension? (plist-get data :isContentScript))
        (url (plist-get data :url))
        (source-map-url (plist-get data :sourceMapURL))
        (id (plist-get data :scriptId)))
    (when (and (eq extension? :json-false) (not (string-equal "" url)))
      (setq kite-mini-rpc-scripts
            (--remove (equal (plist-get it :url) url)
                      kite-mini-rpc-scripts))
      (add-to-list 'kite-mini-rpc-scripts
                   (list :id id
                         :url url
                         :source-map (kite-mini--lazy-load-source-map source-map-url))))))
;; (concat (url-basepath url) source-map-url)

(defun kite-mini--lazy-load-source-map (url)
  ;; TODO: download url (currently very slow)
  ;; (sourcemap-from-string (kite-mini--get-url-content url))
  (let ((path (kite-mini--project-path url)))
    (when (file-regular-p path)
      (cons 'lazy-source-map path))))

(defun kite-mini--get-source-map (script)
  (let ((source-map (plist-get script :source-map)))
    (when (and (consp source-map) (eq (car source-map) 'lazy-source-map))
      (setq source-map (sourcemap-from-file (cdr source-map)))
      (plist-put script :source-map source-map))
    source-map))

(defun kite-mini-on-script-failed-to-parse (data)
  (kite-mini-console-append (format "%s" data)))

(defun kite-mini-on-message-added (data)
  (let* ((message (plist-get data :message))
         (url (plist-get message :url))
         (column (plist-get message :column))
         (line (plist-get message :line))
         (type (plist-get message :type))
         (level (plist-get message :level))
         (parameters (plist-get message :parameters))
         (stack (plist-get message :stack)))
    (when stack
      (let* ((frame (first (plist-get stack :callFrames)))
             (script (kite-mini-find-script (plist-get frame :scriptId)))
             (source-map (kite-mini--get-source-map script)))
        (when source-map
          (let ((info (sourcemap-original-position-for
                       source-map
                       :line (plist-get frame :lineNumber)
                       :column (- (plist-get frame :columnNumber) 1))))
            (setq url  (plist-get info :source)
                  line (plist-get info :line)
                  column (plist-get info :column))))))
    (kite-mini-console-append (propertize
                               (format "%s: %s\t%s (line: %s column: %s)"
                                       level
                                       (kite-mini--console-format parameters)
                                       url line column)
                               'font-lock-face (intern (format "kite-mini-log-%s" level))))))

(defun kite-mini-on-message (socket data)
  (let* ((data (kite-mini-decode (websocket-frame-payload data)))
         (method (plist-get data :method))
         (params (plist-get data :params)))
    (pcase method
      ("Debugger.scriptParsed" (kite-mini-on-script-parsed params))
      ;; we are getting an error in Console.messageAdded
      ;; ("Debugger.scriptFailedToParse" (kite-mini-on-script-failed-to-parse params))
      ("Console.messageAdded" (kite-mini-on-message-added params))
      ;; ;; TODO: do something usefull here, possibly great for REPL
      ("Console.messageRepeatCountUpdated")
      ;; nil -> These are return messages from RPC calls, not notification
      (_ (if method
             (message "Kite: %s" data) ; Generic fallback, only used in development
           (kite-mini-dispatch-callback (plist-get data :id)
                                        (plist-get data :result)))))))

(defun kite-mini-call-rpc (method &optional params callback)
  (unless (kite-mini--connected-p)
    (kite-mini-connect))
  (let ((id (kite-mini-next-rpc-id)))
    (when callback
      (kite-mini-register-callback id callback))
    (websocket-send-text
     kite-mini-socket
     (kite-mini-encode (list :id id
                             :method method
                             :params params)))))

(defun kite-mini-open-socket (url)
  (websocket-open url
                  :on-open #'kite-mini-on-open
                  :on-message #'kite-mini-on-message
                  :on-close #'kite-mini-on-close))

(defun kite-mini-get-json (url)
  (let* ((url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (json-array-type 'list)
         (json-object-type 'plist))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (not (eq 200 (url-http-parse-response)))
          (error "Unable to connect to host.")
        (goto-char (+ 1 url-http-end-of-headers))
        (json-read)))))

(defun kite-mini-get-tabs (host port)
  (let* ((url (url-parse-make-urlobj
               "http" nil nil host port "/json"))
         (tabs (kite-mini-get-json url)))
    (-filter (lambda (tab)
               (and (plist-get tab :webSocketDebuggerUrl)
                    (string-equal (plist-get tab :type) "page")))
             tabs)))

(defun kite-mini-tab-completion (tab)
  (let ((title (plist-get tab :title))
        (url (plist-get tab :url)))
    (cons (format "%s" title) tab)))

(defun kite-mini-select-tab (host port)
  (let* ((tabs (mapcar #'kite-mini-tab-completion
                       (kite-mini-get-tabs host port)))
         (selection (completing-read
                     "Tab: " tabs nil t "" nil (caar tabs)))
         (tab (cdr (assoc selection tabs))))
    (plist-get tab :webSocketDebuggerUrl)))


(defun kite-mini-connect ()
  (interactive)
  (kite-mini-disconnect)
  (let* ((socket-url (kite-mini-select-tab kite-mini-remote-host
                                           kite-mini-remote-port)))
    (setq kite-mini-socket (kite-mini-open-socket socket-url))
    (kite-mini-call-rpc "Console.enable")
    (kite-mini-call-rpc "Debugger.enable")
    (kite-mini-call-rpc "Network.setCacheDisabled" '(:cacheDisabled t))))

(defun kite-mini--connected-p ()
  (websocket-openp kite-mini-socket))

(defun kite-mini-disconnect ()
  (interactive)
  (when (kite-mini--connected-p)
    (websocket-close kite-mini-socket)
    (setq kite-mini-socket nil
          kite-mini-rpc-scripts nil)))


(defun kite-mini-send-eval (code &optional callback)
  (kite-mini-call-rpc
   "Runtime.evaluate"
   (list :expression code
         :returnByValue t)
   callback))

(defun kite-mini-remove-script (script)
  (setq kite-mini-rpc-scripts
        (delete script kite-mini-rpc-scripts)))

(defun kite-mini-script-id (file)
  (let* ((name (file-name-nondirectory file))
         (script (--find (string-suffix-p name (plist-get it :url))
                         kite-mini-rpc-scripts)))
    (when script (plist-get script :id))))

(defun kite-mini-find-script (id)
  (--find (equal (plist-get it :id) id) kite-mini-rpc-scripts))

(defun kite-mini--get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;;

(defun kite-mini-update ()
  (interactive)
  (if (functionp 'kite-mini-update-custom)
      (kite-mini-update-custom)
    (kite-mini--update)))

(defun kite-mini--update ()
  (let ((id (kite-mini-script-id (buffer-file-name)))
        (source (buffer-substring-no-properties
                 (point-min) (point-max))))
    (if id
        (kite-mini-call-rpc
         "Debugger.setScriptSource"
         (list :scriptId id :scriptSource source))
      (message "No matching script for current buffer."))))

(defun kite-mini--find-script-by-path (path line column)
  (cl-loop for script in kite-mini-rpc-scripts
           for source-map = (kite-mini--get-source-map script)
           when (sourcemap-generated-position-for source-map
                                                  :source path
                                                  :line line
                                                  :column column)
           return script))

(defun kite-mini-reload ()
  (interactive)
  (kite-mini-call-rpc
   "Page.reload"
   (list :ignoreCache t)))

(defun kite-mini-evaluate-region-or-line (&optional args)
  (interactive "*P")
  (let ((start (if (region-active-p)
                   (region-beginning)
                 (line-beginning-position)))
        (end (if (region-active-p)
                 (region-end)
               (line-end-position))))
    (kite-mini-send-eval (buffer-substring-no-properties start end))))


(defvar kite-mini-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-x C-e") #'kite-mini-evaluate-region-or-line)
      (define-key map (kbd "C-c C-k") #'kite-mini-update)
      (define-key map (kbd "C-c C-z") #'kite-mini-console)
      (define-key map (kbd "C-c C-r") #'kite-mini-reload)))
  "Keymap for Kite Mini mode.")

;;;###autoload
(defun turn-on-kite-mini-mode ()
  "Turn on Kite Mini mode.")

;;;###autoload
(defun turn-off-kite-mini-mode ()
  "Turn off Kite Mini mode.")

;;;###autoload
(define-minor-mode kite-mini-mode
  "Minor mode for interact with WebKit remote debugging API."
  :global nil
  :group 'kite-mini
  :init-value nil
  :lighter ""
  :keymap kite-mini-mode-map
  (if kite-mini-mode
      (turn-on-kite-mini-mode)
    (turn-off-kite-mini-mode)))

(provide 'kite-mini)
;;; kite-mini.el ends here
