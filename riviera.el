;;; riviera.el --- DBGp protocol frontend, a script debugger
;; $Id: riviera.el 118 2010-03-30 10:26:39Z fujinaka.tohru $
;;
;; Filename: riviera.el
;; Author: Jared Hankle <jdhankle@gmail.com>
;; Maintainer: Jared Hankle <jdhankle@gmail.com>
;; Version: 0.26
;; URL: https://github.com/JDHankle/riviera
;; Keywords: DBGp, debugger, PHP, Xdebug, Perl, Python, Ruby, Tcl, Komodo
;; Compatibility: Emacs 22.1
;;
;; This file is not part of GNU Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; RIVIERA is a software package that interfaces Emacs to DBGp protocol
;; with which you can debug running scripts interactive. At this present
;; DBGp protocol are supported in several script languages with help of
;; custom extensions.
;;
;;; Usage
;;
;; 1. Insert autoload hooks into your .Emacs file.
;;    -> (autoload 'riviera "riviera" "DBGp protocol frontend, a script debugger" t)
;; 2. Start RIVIERA. By default, M-x riviera will start it.
;;    RIVIERA starts to listening to DBGp protocol session connection.
;; 3. Run debuggee script.
;;    When the connection is established, RIVIERA loads the entry script
;;    file in riviera-mode.
;; 4. Start debugging. To see riviera-mode ey bindings, type ?.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Requirements:
;;
;; [Server side]
;; - PHP with Xdebug 2.0.3
;;    http://xdebug.org/
;; - Perl, Python, Ruby, Tcl with Komodo Debugger Extension
;;    http://aspn.activestate.com/ASPN/Downloads/Komodo/RemoteDebugging
;;
;; [Client side]
;; - Emacs 22.1 and later
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (when (or (not (boundp 'emacs-version))
            (string< emacs-version "22.1"))
    (error (concat "riviera.el: This package requires Emacs 22.1 or later."))))

(eval-and-compile
  (require 'cl)
  (require 'ido)
  (require 'xml)
  (require 'tree-widget)
  (require 'dbgp))

(defvar riviera-version "0.24")

;;--------------------------------------------------------------
;; customization
;;--------------------------------------------------------------

;; For compatibility between versions of custom
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable)
           ;; Some XEmacsen w/ custom don't have :set keyword.
           ;; This protects them against custom.
           (fboundp 'custom-initialize-set))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (if (boundp 'defgroup)
        nil
      (defmacro defgroup (&rest args)
        nil))
    (if (boundp 'defcustom)
        nil
      (defmacro defcustom (var value doc &rest args)
        `(defvar (,var) (,value) (,doc))))))

;; customize group

(defgroup riviera nil
  "A PHP Debugging environment."
  :group 'debug)

(defgroup riviera-highlighting-faces nil
  "Faces for RIVIERA."
  :group 'riviera
  :group 'font-lock-highlighting-faces)

;; display window behavior

(defvar riviera-dynamic-property-buffer-p nil)

(defcustom riviera-display-window-function 'pop-to-buffer
  "*Function to display a debuggee script's content.
Typically `pop-to-buffer' or `switch-to-buffer'."
  :group 'riviera
  :type 'function)

(defsubst riviera-dbgp-dynamic-property-bufferp (buf)
  (with-current-buffer buf
    (symbol-value 'riviera-dynamic-property-buffer-p)))

(defun riviera-dbgp-display-window (buf)
  "Display a buffer anywhere in a window, depends on the circumstance."
  (cond
   ((get-buffer-window buf)
    (select-window (get-buffer-window buf))
    (switch-to-buffer buf))
   ((or (eq 1 (count-windows))
        (not (riviera-dbgp-dynamic-property-buffer-visiblep)))
    (funcall riviera-display-window-function buf))
   (t
    (let ((candidates (make-vector 3 nil))
          (dynamic-p (riviera-dbgp-dynamic-property-bufferp buf)))
      (block finder
        (walk-windows (lambda (window)
                        (if (riviera-dbgp-dynamic-property-bufferp (window-buffer window))
                            (if dynamic-p
                                (unless (aref candidates 1)
                                  (aset candidates 1 window)))
                          (if (eq (selected-window) window)
                              (aset candidates 2 window)
                            (aset candidates 0 window)
                            (return-from finder))))))
      (select-window (or (aref candidates 0)
                         (aref candidates 1)
                         (aref candidates 2)
                         (selected-window)))
      (switch-to-buffer buf))))
  buf)

;;  (when (buffer-live-p buf)
;;    (or (eq buf (get-buffer riviera-context-buffer-name))
;;      (eq buf (get-buffer (riviera-dbgp-redirect-buffer-name session :stdout)))
;;      (eq buf (get-buffer (riviera-dbgp-redirect-buffer-name session :stderr))))))

(defun riviera-dbgp-dynamic-property-buffer-visiblep ()
  "Check whether any window displays any property buffer."
  (block walk-loop
    (walk-windows (lambda (window)
                    (if (riviera-dbgp-dynamic-property-bufferp (window-buffer window))
                        (return-from walk-loop t))))))


;;==============================================================
;; utilities
;;==============================================================

(defsubst riviera-flatten (x)
  "Make cons X to a flat list."
  (flet ((rec (x acc)
              (cond ((null x) acc)
                    ((atom x) (cons x acc))
                    (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defsubst riviera-what-line (&optional pos)
  "Get the number of the line in which POS is located.
If POS is omitted, then the current position is used."
  (save-restriction
    (widen)
    (save-excursion
      (if pos (goto-char pos))
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defmacro riviera-plist-push (plist prop value)
  `(let* ((plist ,plist)
          (l (plist-get plist ,prop)))
     (cond
      ((consp l)
       (plist-put plist ,prop
                  (cons ,value (plist-get plist ,prop))))
      ((null l)
       (plist-put plist ,prop (list ,value)))
      (t
       (error "riviera-plist-push: cannot add value; type of prop `%s' is not `list' but `%s'."
              ,prop (type-of ,value))))))

(defmacro riviera-plist-append (plist prop value)
  `(let* ((plist ,plist)
          (l (plist-get plist ,prop)))
     (cond
      ((consp l)
       (nconc l (list ,value)))
      ((null l)
       (plist-put plist ,prop (list ,value)))
      (t
       (error "riviera-plist-add: cannot add value; type of prop `%s' is not `list' but `%s'."
              ,prop (type-of ,value))))))

(defmacro riviera-lexical-bind (bindings &rest body)
  (declare (indent 1)
           (debug (sexp &rest form)))
  (cl-macroexpand-all
   (nconc
    (list 'lexical-let (mapcar (lambda (arg)
                                 (list arg arg))
                               bindings))
    body)))

(defun riviera-remove-directory-tree (basedir)
  (ignore-errors
    (mapc (lambda (path)
            (cond
             ((or (file-symlink-p path)
                  (file-regular-p path))
              (delete-file path))
             ((file-directory-p path)
              (let ((name (file-name-nondirectory path)))
                (or (equal "." name)
                    (equal ".." name)
                    (riviera-remove-directory-tree path))))))
          (directory-files basedir t nil t))
    (delete-directory basedir)))

(defun riviera-remote-p (ip)
  "Test whether IP refers a remote system."
  (not (or (equal ip "127.0.0.1")
           (and (fboundp 'network-interface-list)
                (member ip (mapcar (lambda (addr)
                                     (format-network-address (cdr addr) t))
                                   (network-interface-list)))))))

;;--------------------------------------------------------------
;;  cross emacs overlay definitions
;;--------------------------------------------------------------

(eval-and-compile
  (and (featurep 'xemacs)
       (require 'overlay))
  (or (fboundp 'overlay-livep)
      (defalias 'overlay-livep 'overlay-buffer)))

(defun riviera-overlay-make-line (lineno &optional buf)
  "Create a whole line overlay."
  (with-current-buffer (or buf (current-buffer))
    (save-excursion
      (widen)
      (goto-line lineno)
      (beginning-of-line)
      (make-overlay (point)
                    (save-excursion
                      (forward-line) (point))
                    nil t nil))))


;;==============================================================
;; DBGp related utilities
;;==============================================================

(defmacro* riviera-dbgp-sequence (cmd &rest callback)
  (declare (indent 1)
           (debug (form &rest form)))
  (list 'progn
        (list 'riviera-plist-append cmd
              :callback (car callback))))

(defmacro* riviera-dbgp-sequence-bind (bindings cmd callback)
  (declare (indent 1)
           (debug (sexp form lambda-expr)))
  (cl-macroexpand-all
   (list 'progn
         (list 'riviera-plist-append cmd
               :callback (if bindings
                             (list 'riviera-lexical-bind bindings callback)
                           callback)))))

(defun riviera-dbgp-decode-string (string data-encoding coding-system)
  "Decode encoded STRING."
  (when string
    (let ((s string))
      (when (consp s)
        (setq s (car s)))
      (when (stringp s)
        (setq s (cond
                 ((equal "base64" data-encoding)
                  (base64-decode-string s))
                 (t s)))
        (if coding-system
            (decode-coding-string s coding-system)
          s)))))


(defcustom riviera-temporary-file-directory (expand-file-name "riviera" "~/.emacs.d")
  "*Base directory path where RIVIERA creates temporary files and directories."
  :group 'riviera
  :type 'directory)

(defvar riviera-storages nil)
(defvar riviera-storage-loaded nil)

(defun riviera-storage-load ()
  (let ((storage-path (expand-file-name ".storage"
                                        riviera-temporary-file-directory)))
    (when (file-exists-p storage-path)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents storage-path)
          (setq riviera-storages (read (buffer-string))))))))

(defun riviera-storage-save ()
  (let ((storage-path (expand-file-name ".storage"
                                        riviera-temporary-file-directory)))
    (with-temp-buffer
      (pp riviera-storages (current-buffer))
      (with-temp-message ""
        (write-region (point-min) (point-max) storage-path)))))


;;==============================================================
;; session
;;==============================================================

;;--------------------------------------------------------------
;; constants
;;--------------------------------------------------------------

(defconst riviera-process-buffer-name "*RIVIERA<%s> process*"
  "Name for DBGp client process console buffer.")
(defconst riviera-backtrace-buffer-name "*RIVIERA<%s> backtrace*"
  "Name for backtrace buffer.")
(defconst riviera-breakpoint-list-buffer-name "*RIVIERA<%s> breakpoint list*"
  "Name for breakpoint list buffer.")
(defconst riviera-context-buffer-name "*RIVIERA<%s> context*"
  "Name for context buffer.")

(defvar riviera-sessions nil)
(defvar riviera-current-session nil)

;; riviera session start/finish hooks

(defcustom riviera-session-enter-hook nil
  "*Hook running at when the riviera debugging session is starting.
Each function is invoked with one argument, SESSION"
  :group 'riviera
  :type 'hook)

(defcustom riviera-session-exit-hook nil
  "*Hook running at when the riviera debugging session is finished."
  :group 'riviera
  :type 'hook)

(defcustom riviera-pause-at-entry-line t
  "*Specify whether debuggee script should be paused at the entry line.
If the value is t, RIVIERA will automatically pause the starting program
at the entry line of the script."
  :group 'riviera
  :type 'boolean)

(defstruct (riviera-session
            (:constructor nil)
            (:constructor riviera-session-make))
  "Represent a DBGp protocol connection session."
  storage
  process
  (tid 30000)
  (state :created)
  initmsg
  xdebug-p
  language
  feature
  redirect
  breakpoint
  cmd
  sending-p
  source
  stack
  context
  (cursor (list :overlay nil :position nil))
  tempdir
  )

(defun riviera-kill-buffers(&rest args)
  "Kills all buffers whose name start with *RIVIERA.
ARGS is so it can be used for `riviera-session-exit-hook'.

TODO: Standard buffer names so we don't have to do a string match."
  (mapc (lambda(buffer)
          (if (string-prefix-p "*RIVIERA" (buffer-name buffer))
              (kill-buffer buffer)))
        (buffer-list)))

(defmacro riviera-with-current-session (binding &rest body)
  (declare (indent 1)
           (debug (symbolp &rest form)))
  (cl-macroexpand-all
   `(let ((,binding riviera-current-session))
      (when ,binding
        ,@body))))

;; initialize

(defsubst riviera-session-init (session init-msg)
  "Initialize a session of a process PROC."
  (riviera-session-tempdir-setup session)
  (setf (riviera-session-initmsg session) init-msg)
  (setf (riviera-session-xdebug-p session)
        (equal "Xdebug" (car (xml-node-children
                              (car (xml-get-children init-msg 'engine))))))
  (setf (riviera-session-language session)
        (let ((lang (xml-get-attribute-or-nil init-msg 'language)))
          (and lang
               (intern (concat ":" (downcase lang))))))
  (setf (riviera-session-storage session) (or (riviera-session-storage-find session)
                                              (riviera-session-storage-create session)))
  (run-hook-with-args 'riviera-session-enter-hook session))

(defun riviera-session-storage-create (session)
  (let* ((initmsg (riviera-session-initmsg session))
         (process (riviera-session-process session))
         (listener (dbgp-plist-get process :listener))
         (storage (if (dbgp-proxy-p process)
                      (list :proxy t
                            :addr (xml-get-attribute initmsg 'hostname)
                            :idekey (xml-get-attribute initmsg 'idekey))
                    (list :proxy nil
                          :port (second (process-contact listener))))))
    (nconc storage (list :language (riviera-session-language session)
                         :fileuri (xml-get-attribute initmsg 'fileuri)))
    (add-to-list 'riviera-storages storage)
    storage))

(defun riviera-session-storage-find (session)
  (unless riviera-storage-loaded
    (riviera-storage-load)
    (setq riviera-storage-loaded t))
  (let* ((initmsg (riviera-session-initmsg session))
         (addr (xml-get-attribute initmsg 'hostname))
         (fileuri (xml-get-attribute initmsg 'fileuri))
         (idekey (xml-get-attribute initmsg 'idekey))
         (process (riviera-session-process session))
         (listener (dbgp-plist-get process :listener))
         (proxy-p (dbgp-proxy-p listener))
         (port (second (process-contact listener))))
    (find-if (lambda (storage)
               (and (eq (not proxy-p)
                        (not (plist-get storage :proxy)))
                    (eq (riviera-session-language session)
                        (plist-get storage :language))
                    (equal fileuri (plist-get storage :fileuri))
                    (if proxy-p
                        (and (equal addr (plist-get storage :addr))
                             (equal idekey (plist-get storage :idekey)))
                      (eq port (plist-get storage :port)))))
             riviera-storages)))

(defsubst riviera-session-release (session)
  "Initialize a session of a process PROC."
  (setf (riviera-session-process session) nil)
  (setf (riviera-session-cursor session) nil)
  (riviera-session-tempdir-remove session)
  (riviera-storage-save)
  (run-hook-with-args 'riviera-session-exit-hook session))

(defsubst riviera-session-active-p (session)
  (let ((proc (riviera-session-process session)))
    (and (processp proc)
         (eq 'open (process-status proc)))))

;; tid

(defsubst riviera-session-next-tid (session)
  "Get transaction id for next command."
  (prog1
      (riviera-session-tid session)
    (incf (riviera-session-tid session))))

;; buffer

(defsubst riviera-session-buffer-name (session format-string)
  (let* ((proc (riviera-session-process session))
         (idekey (plist-get (dbgp-proxy-get proc) :idekey)))
    (format format-string
            (concat (if idekey
                        (format "%s:" idekey)
                      "")
                    (format "%s:%s"
                            (dbgp-ip-get proc)
                            (dbgp-port-get (dbgp-listener-get proc)))))))

(defsubst riviera-session-buffer (session format-string)
  (get-buffer-create (riviera-session-buffer-name session format-string)))

(defsubst riviera-session-buffer-get (session format-string)
  (get-buffer (riviera-session-buffer-name session format-string)))

(defsubst riviera-session-buffer-live-p (session format-string)
  (buffer-live-p (get-buffer (riviera-session-buffer-name session format-string))))

(defsubst riviera-session-buffer-visible-p (session format-string)
  (let ((buf (get-buffer (riviera-session-buffer-name session format-string))))
    (and buf
         (buffer-live-p buf)
         (get-buffer-window buf))))

;; temporary directory

(defun riviera-session-tempdir-setup (session)
  "Setup temporary directory."
  (let* ((proc (riviera-session-process session))
         (rivieradir (file-truename riviera-temporary-file-directory))
         (leafdir (format "%d" (second (process-contact proc))))
         (tempdir (expand-file-name leafdir rivieradir)))
    (unless (file-directory-p rivieradir)
      (make-directory rivieradir t)
      (set-file-modes rivieradir #o1777))
    (setf (riviera-session-tempdir session) tempdir)))

(defun riviera-session-tempdir-remove (session)
  "Remove temporary directory."
  (let ((tempdir (riviera-session-tempdir session)))
    (when (file-directory-p tempdir)
      (riviera-remove-directory-tree tempdir))))

;; misc

(defsubst riviera-session-ip-get (session)
  "Get ip address of the host server."
  (let* ((proc (riviera-session-process session))
         (listener (dbgp-listener-get proc)))
    (format-network-address (dbgp-ip-get proc) t)))

(defun riviera-session-remote-p (session)
  "Get ip address of the host server."
  (riviera-remote-p (riviera-session-ip-get session)))


;;==============================================================
;; cmd hash
;;==============================================================

(defmacro riviera-cmd-param-for (key)
  `(plist-get '(:depth "-d"
                       :context-id "-c"
                       :max-data-size "-m"
                       :type "-t"
                       :page "-p"
                       :key "k"
                       :address "-a"
                       :name "-n"
                       :fileuri "-f"
                       :lineno "-n"
                       :class "-a"
                       :function "-m"
                       :state "-s"
                       :exception "-x"
                       :hit-value "-h"
                       :hit-condition "-o"
                       :run-once "-r"
                       :expression "--")
              ,key))

(defsubst riviera-cmd-param-get (cmd flag)
  "Get FLAG's parameter used in CMD.
For a DBGp command \`stack_get -i 1 -d 2\',
`(riviera-cmd-param-get cmd \"-d\")\' gets \"2\"."
  (cdr-safe (assoc flag (plist-get cmd :param))))

(defun riviera-cmd-expand (cmd)
  "Build a send command string for DBGp protocol."
  (mapconcat #'(lambda (x)
                 (cond ((stringp x) x)
                       ((integerp x) (int-to-string x))
                       ((atom (format "%S" x)))
                       ((null x) "")
                       (t x)))
             (riviera-flatten (list (plist-get cmd :operand)
                                    "-i"
                                    (plist-get cmd :tid)
                                    (plist-get cmd :param)))
             " "))

(defsubst riviera-session-cmd-make (session operand params)
  "Create a new command object."
  (list :session session
        :tid (riviera-session-next-tid session)
        :operand operand
        :param params))

(defsubst riviera-session-cmd-append (session cmd)
  (let ((cmds (riviera-session-cmd session)))
    (if cmds
        (nconc cmds (list cmd))
      (setf (riviera-session-cmd session) (list cmd)))))

(defun riviera-session-cmd-remove (session tid)
  "Get a command object from the command hash table specified by TID."
  (let ((cmds (riviera-session-cmd session)))
    (if (eq tid (plist-get (car cmds) :tid))
        (prog1
            (car cmds)
          (setf (riviera-session-cmd session) (cdr cmds)))
      (let (match-cmd)
        (setf (riviera-session-cmd session)
              (remove-if (lambda (cmd)
                           (and (eq tid (plist-get cmd :tid))
                                (setq match-cmd cmd)))
                         cmds))
        match-cmd))))


;;==============================================================
;; DBGp protocol handler
;;==============================================================

(defsubst riviera-dbgp-tid-read (msg)
  "Get a transaction id of MSG."
  (let ((tid (xml-get-attribute-or-nil msg 'transaction_id)))
    (and tid
         (string-to-number tid))))

(defun riviera-dbgp-entry (session msg)
  "Analyze MSG and dispatch to a specific handler."
  ;; remain session status ('connect, 'init, 'break, 'stopping, 'stopped)
  (let ((handler (intern-soft (concat "riviera-dbgp-handle-"
                                      (symbol-name (xml-node-name msg)))))
        (status (xml-get-attribute-or-nil msg 'status)))
    (and status
         (setf (riviera-session-state session) (intern (concat ":" status))))
    (and (functionp handler)
         (funcall handler session msg))))

(defvar riviera-dbgp-init-hook nil)

(defun riviera-dbgp-handle-init (session msg)
  "Handle a init message."
  (riviera-session-init session msg)
  (run-hook-with-args 'riviera-dbgp-init-hook session))

(defun riviera-dbgp-handle-response (session msg)
  "Handle a response message."
  (let* ((tid (riviera-dbgp-tid-read msg))
         (cmd (riviera-session-cmd-remove session tid))
         (err (dbgp-xml-get-error-node msg)))
    (riviera-dbgp-handle-status session msg)
    (riviera-dbgp-process-command-queue session)
    (cond
     (err
      (message "Command error: %s"
               (dbgp-xml-get-error-message msg)))
     (cmd
      (let* ((operand (replace-regexp-in-string
                       "_" "-" (xml-get-attribute msg 'command)))
             (func-name (concat "riviera-dbgp-response-" operand))
             (func (intern-soft func-name)))
        (and (functionp func)
             (funcall func session cmd msg)))))
    (mapc (lambda (callback)
            (funcall callback session cmd msg err))
          (plist-get cmd :callback))))

(defun riviera-dbgp-handle-status (session msg)
  "Handle status code in a response message."
  (let ((status (xml-get-attribute msg 'status)))
    (cond
     ((equal status "stopping")
      (accept-process-output)
      (and (riviera-session-active-p session)
           (riviera-dbgp-command-stop session))))))

;;; command sending

(defun riviera-dbgp-send-string (session string)
  (and (string< "" string)
       (riviera-session-active-p session)
       (dbgp-session-send-string (riviera-session-process session) string t)))

(defun riviera-send-raw-command (session fmt &rest arg)
  "Send a command string to a debugger engine.
The command string will be built up with FMT and ARG with a help of
the string formatter function `format'."
  (let ((cmd (apply #'format fmt arg)))
    (riviera-dbgp-send-string session cmd)))

(defun riviera-dbgp-send-command (session operand &rest params)
  "Send a command to a debugger engine.
Return a cmd list."
  (if (riviera-session-active-p session)
      (let ((cmd (riviera-session-cmd-make session operand params)))
        (riviera-session-cmd-append session cmd)
        (unless (riviera-session-sending-p session)
          (setf (riviera-session-sending-p session) t)
          (riviera-dbgp-process-command-queue session))
        cmd)))

(defun riviera-dbgp-process-command-queue (session)
  (let ((cmd (car (riviera-session-cmd session))))
    (if cmd
        (riviera-dbgp-send-string session (riviera-cmd-expand cmd))
      (setf (riviera-session-sending-p session) nil))))

(defvar riviera-dbgp-continuous-command-hook nil)

;;--------------------------------------------------------------
;; continuous commands
;;--------------------------------------------------------------

;; step_into

(defun riviera-dbgp-command-step-into (session)
  "Send \`step_into\' command."
  (riviera-dbgp-send-command session "step_into"))

(defun riviera-dbgp-response-step-into (session cmd msg)
  "A response message handler for \`step_into\' command."
  (run-hook-with-args 'riviera-dbgp-continuous-command-hook session))

;; step_over

(defun riviera-dbgp-command-step-over (session)
  "Send \`step_over\' command."
  (riviera-dbgp-send-command session "step_over"))

(defun riviera-dbgp-response-step-over (session cmd msg)
  "A response message handler for \`step_over\' command."
  (run-hook-with-args 'riviera-dbgp-continuous-command-hook session))

;; step_out

(defun riviera-dbgp-command-step-out (session)
  "Send \`step_out\' command."
  (riviera-dbgp-send-command session "step_out"))

(defun riviera-dbgp-response-step-out (session cmd msg)
  "A response message handler for \`step_out\' command."
  (run-hook-with-args 'riviera-dbgp-continuous-command-hook session))

;; run

(defun riviera-dbgp-command-run (session)
  "Send \`run\' command."
  (riviera-dbgp-send-command session "run"))

(defun riviera-dbgp-response-run (session cmd msg)
  "A response message handler for \`run\' command."
  (run-hook-with-args 'riviera-dbgp-continuous-command-hook session))

;;; stop

(defun riviera-dbgp-command-stop (session)
  "Send \`stop\' command."
  (riviera-dbgp-send-command session "stop"))

;;; eval

(defun riviera-dbgp-command-eval (session exp)
  "Send \`eval\' command."
  (riviera-dbgp-send-command
   session
   "eval"
   (format "-- {%s}" (base64-encode-string exp))))

(defun riviera-dbgp-response-eval (session cmd msg)
  "A response message handler for \`eval\' command."
  (message "result: %S"
           (riviera-dbgp-decode-value (car-safe (xml-get-children msg 'property)))))

(defun riviera-dbgp-decode-value (prop)
  "Decode a VALUE passed by debugger engine."
  (let ((type (xml-get-attribute prop 'type))
        result)
    (setq result
          (cond
           ((or (string= "array" type)
                (string= "object" type))
            (mapcar (lambda (value)
                      (riviera-dbgp-decode-value value))
                    (xml-get-children prop 'property)))
           ((string= "null" type)
            nil)
           (t
            (let ((value (car (last prop))))
              (assert (stringp value))
              (when (string= "base64" (xml-get-attribute prop 'encoding))
                (setq value (base64-decode-string value)))
              (if (string= "string" type)
                  (decode-coding-string value 'utf-8)
                (string-to-number value))))))
    (let ((name (xml-get-attribute-or-nil prop 'name)))
      (if name
          (cons name result)
        result))))

(eval-when-compile
  (require 'tramp))

;;==============================================================
;; source
;;==============================================================

;; file hooks

(defcustom riviera-source-visit-hook nil
  "*Hook running at when RIVIERA visits a debuggee script file.
Each function is invoked with one argument, BUFFER."
  :group 'riviera
  :type 'hook)

(defcustom riviera-close-mirror-file-after-finish t
  "*Specify whether RIVIERA should close fetched files from remote site after debugging.
Since the remote files is stored temporary that you can confuse
they were editable if they were left after a debugging session.
If the value is non-nil, RIVIERA closes temporary files when
debugging is finished.
If the value is nil, the files left in buffers."
  :group 'riviera
  :type 'boolean)

(defun riviera-source-find-file-handler ()
  (let* ((local-path (buffer-file-name))
         (session (and local-path (riviera-source-find-session local-path))))
    (if session
        (run-hook-with-args 'riviera-source-visit-hook session (current-buffer)))))

(add-hook 'find-file-hook #'riviera-source-find-file-handler)

;;--------------------------------------------------------------
;; source hash
;;--------------------------------------------------------------

(defcustom riviera-source-coding-system 'utf-8
  "Coding system for source code retrieving remotely via the debugger engine."
  :group 'riviera
  :type 'coding-system)

(defmacro riviera-source-make (fileuri local-path)
  "Create a new source object.
A source object forms a property list with three properties
:fileuri, :remotep and :local-path."
  `(list :fileuri ,fileuri :local-path ,local-path))

(defvar riviera-source-release-hook nil)

(defun riviera-source-release (source)
  "Release a SOURCE object."
  (let ((buf (find-buffer-visiting (or (plist-get source :local-path) ""))))
    (when buf
      (with-current-buffer buf
        (when (and (boundp 'riviera-mode)
                   (symbol-value 'riviera-mode))
          (run-hooks 'riviera-source-release-hook))
        ;;    Not implemented yet
        ;;    (and (buffer-modified-p buf)
        ;;         (switch-to-buffer buf)
        ;;         (yes-or-no-p "Buffer is modified. Save it?")
        ;;         (riviera-write-file-contents this buf))
        (when riviera-close-mirror-file-after-finish
          (set-buffer-modified-p nil)
          (kill-buffer buf))))))

(defsubst riviera-source-fileuri-regularize (fileuri)
  ;; for bug of Xdebug 2.0.3 and below:
  (replace-regexp-in-string "%28[0-9]+%29%20:%20runtime-created%20function$" ""
                            fileuri))

(defun riviera-source-fileuri (session local-path)
  "Guess a file uri string which counters to LOCAL-PATH."
  (let* ((tempdir (riviera-session-tempdir session))
         (templen (length tempdir))
         (tramp-spec (plist-get (riviera-session-storage session) :tramp))
         (tramp-spec-len (and tramp-spec (length tramp-spec))))
    (concat "file://"
            (cond
             ((and (< templen (length local-path))
                   (string= tempdir (substring local-path 0 templen)))
              (substring local-path
                         (- templen
                            (if (string< "" (file-name-nondirectory tempdir)) 0 1))))
             ((and tramp-spec
                   (< tramp-spec-len (length local-path))
                   (string= tramp-spec (substring local-path 0 tramp-spec-len)))
              (substring local-path tramp-spec-len))
             (t
              local-path)))))

(defun riviera-source-local-path (session fileuri)
  "Generate path string from FILEURI to store temporarily."
  (let ((local-path (riviera-source-local-path-in-server session fileuri)))
    (when local-path
      (expand-file-name (substring local-path (if (string-match "^[A-Z]:" local-path) 3 1))
                        (riviera-session-tempdir session)))))

(defun riviera-source-local-path-in-server (session fileuri &optional disable-completion)
  "Make a path string correspond to FILEURI."
  (when (string-match "^\\(file\\|https?\\):/+" fileuri)
    (let ((path (substring fileuri (1- (match-end 0)))))
      (require 'url-util)
      (setq path (url-unhex-string path))
      (when (string-match "^/[A-Z]:" path) ;; for HTTP server on Windows
        (setq path (substring path 1)))
      (if (and (not disable-completion)
               (string= "" (file-name-nondirectory path)))
          (expand-file-name (riviera-source-default-file-name session)
                            path)
        path))))

(defun riviera-source-default-file-name (session)
  (case (riviera-session-language session)
    (:php "index.php")
    (:python "index.py")
    (:perl "index.pl")
    (:ruby "index.rb")
    (t "index.html")))

(defun riviera-source-find-session (temp-path)
  "Find a session which may have a file at TEMP-PATH in its temporary directory tree."
  (find-if (lambda (session)
             (let ((tempdir (riviera-session-tempdir session)))
               (ignore-errors
                 (string= tempdir (substring temp-path 0 (length tempdir))))))
           riviera-sessions))

(defun riviera-source-visit (local-path)
  "Visit to a local source code file."
  (let ((buf (or (find-buffer-visiting local-path)
                 (if (file-exists-p local-path)
                     (let* ((session (riviera-source-find-session local-path))
                            (storage (and session
                                          (riviera-session-storage session)))
                            (coding-system (or (plist-get storage :source-coding-system)
                                               riviera-source-coding-system)))
                       (if coding-system
                           (let ((coding-system-for-read coding-system)
                                 (coding-system-for-write coding-system))
                             (find-file-noselect local-path))
                         (find-file-noselect local-path)))))))
    (when buf
      (riviera-dbgp-display-window buf)
      buf)))

;; session storage

(defun riviera-session-source-storage-add (session fileuri)
  (let* ((storage (riviera-session-storage session))
         (list (plist-get storage :source)))
    (if (and (string-match "^file:/" fileuri)
             (not (find list fileuri :test #'equal)))
        (if list
            (nconc list (list fileuri))
          (plist-put storage :source (list fileuri))))))

;; session

(defun riviera-session-source-init (session)
  "Initialize a source hash table of the SESSION."
  (setf (riviera-session-source session) (make-hash-table :test 'equal)))

(add-hook 'riviera-session-enter-hook #'riviera-session-source-init)

(defun riviera-session-source-add (session fileuri local-path content)
  "Add a source object to SESSION."
  (let ((tempdir (riviera-session-tempdir session)))
    (unless (file-directory-p tempdir)
      (make-directory tempdir t)
      (set-file-modes tempdir #o0700)))
  (riviera-session-source-write-file session local-path content)
  (puthash fileuri (riviera-source-make fileuri local-path) (riviera-session-source session))
  (riviera-session-source-storage-add session fileuri))

(defun riviera-session-source-release (session)
  "Release source objects."
  (maphash (lambda (fileuri source)
             (riviera-source-release source))
           (riviera-session-source session)))

(add-hook 'riviera-session-exit-hook #'riviera-session-source-release)
(add-hook 'riviera-session-exit-hook #'riviera-kill-buffers)

(defsubst riviera-session-source-get (session fileuri)
  (gethash fileuri (riviera-session-source session)))

(defsubst riviera-session-source-append (session fileuri local-path)
  (puthash fileuri (list :fileuri fileuri :local-path local-path)
           (riviera-session-source session)))

(defsubst riviera-session-source-local-path (session fileuri)
  "Find a known local-path that counters to FILEURI."
  (plist-get (gethash fileuri (riviera-session-source session))
             :local-path))

(defsubst riviera-session-source-fileuri (session local-path)
  "Find a known fileuri that counters to LOCAL-PATH."
  (block riviera-session-souce-fileuri
    (maphash (lambda (fileuri path)
               (and (equal local-path (plist-get path :local-path))
                    (return-from riviera-session-souce-fileuri fileuri)))
             (riviera-session-source session))))

(defsubst riviera-session-source-content-coding-system (session content)
  "Guess a coding-system for the CONTENT."
  (or (plist-get (riviera-session-storage session) :source-coding-system)
      riviera-source-coding-system
      (detect-coding-string content t)))

(defun riviera-session-source-write-file (session path content)
  "Write CONTENT to file."
  (make-directory (file-name-directory path) t)
  (ignore-errors
    (with-current-buffer (or (find-buffer-visiting path)
                             (create-file-buffer path))
      (let ((inhibit-read-only t)
            (coding-system (riviera-session-source-content-coding-system session content)))
        (buffer-disable-undo)
        (widen)
        (erase-buffer)
        (font-lock-mode 0)
        (unless (eq 'undecided coding-system)
          (set-buffer-file-coding-system coding-system))
        (insert (decode-coding-string content coding-system)))
      (with-temp-message ""
        (write-file path)
        (kill-buffer (current-buffer))))
    t))

;;; dbgp

(defun riviera-dbgp-command-source (session fileuri)
  "Send source command.
FILEURI is a uri of the target file of a debuggee site."
  (riviera-dbgp-send-command session "source" (cons "-f"
                                                    (riviera-source-fileuri-regularize fileuri))))

(defun riviera-dbgp-response-source (session cmd msg)
  "A response message handler for \`source\' command."
  (let* ((fileuri (riviera-cmd-param-get cmd "-f"))
         (local-path (riviera-source-local-path session fileuri)))
    (when local-path
      (riviera-session-source-add session fileuri local-path (base64-decode-string (third msg)))
      (riviera-source-visit local-path))))

(defun riviera-dbgp-source-fetch (session fileuri)
  "Fetch the content of FILEURI."
  ;;(let ((fileuri (riviera-dbgp-regularize-fileuri fileuri)))
  (unless (riviera-session-source-local-path session fileuri)
    ;; haven't fetched remote source yet; fetch it.
    (riviera-dbgp-command-source session fileuri)))

(defcustom riviera-visit-remote-file nil
  ""
  :group 'riviera
  :type 'function)

(defcustom riviera-get-tramp-spec-for nil
  "Function to retrieve TRAMP spec for a file path of a remove server.
This function is called when visiting a remote server file, with
a parameter `remote-path'. (e.g. \"/192.168.1.32:/var/www/index.php\")
If `remote-path' is unknown to the function, it should return nil.
Or return specific TRAMP spec. (e.g. \"/user@example.com:\""
  :group 'riviera
  :type 'function)

(defcustom riviera-read-file-name 'read-file-name
  "Function used to read the file name when debugging.
If one wanted to use ido:
`(custom-set-variables '(riviera-read-file-name 'ido-read-file-name))'"
:group 'riviera
:type 'function)

(defun riviera-session-source-visit-original-file (session fileuri &optional disable-completion)
  (let ((target-path (riviera-session-source-read-file-name session fileuri disable-completion)))
    (and target-path
         (prog1
             (find-file target-path)
           (message "visited: %s" target-path)))))

(defun riviera-session-source-read-file-name (session fileuri &optional disable-completion)
  (if (riviera-session-remote-p session)
      (riviera-session-source-read-file-name-remote session fileuri disable-completion)
    (riviera-session-source-read-file-name-local session fileuri disable-completion)))

(defun riviera-session-source-read-file-name-local (session fileuri &optional disable-completion)
  (let ((local-path (riviera-source-local-path-in-server session fileuri disable-completion)))
    ;; local file
    (unless (file-regular-p local-path)
      (while (not (file-regular-p (setq local-path
                                        (funcall riviera-read-file-name "Find local file: "
                                                 local-path local-path t ""))))
        (beep)))
    (expand-file-name local-path)))

(defun riviera-session-source-read-file-name-remote (session fileuri &optional disable-completion)
  (condition-case nil
      (if (fboundp 'riviera-visit-remote-file)
          (funcall riviera-visit-remote-file session fileuri)
        (let* ((ip (riviera-session-ip-get session))
               (local-path (riviera-source-local-path-in-server session fileuri disable-completion))
               (storage (riviera-session-storage session))
               (path-prefix (or (plist-get storage :tramp)
                                (and (fboundp 'riviera-get-tramp-spec-for)
                                     (funcall 'riviera-get-tramp-spec-for
                                              (format "/%s:%s" ip local-path)))))
               (find-file-default (if path-prefix
                                      (concat path-prefix local-path)
                                    (format "/%s:%s" ip local-path))))
          (while (not (tramp-handle-file-regular-p
                       (setq find-file-default (funcall riviera-read-file-name "Find remote file: "
                                                        (file-name-directory find-file-default)
                                                        find-file-default t
                                                        (file-name-nondirectory find-file-default)))))
            (beep))
          (require 'tramp)
          (when (tramp-tramp-file-p find-file-default)
            (plist-put storage :tramp (replace-regexp-in-string ":[^:]+$" ":" find-file-default)))
          find-file-default))
    (quit (beep))))


;;==============================================================
;; cursor
;;==============================================================

(defface riviera-cursor-arrow-face
  '((((class color))
     :inherit 'default
     :foreground "cyan"))
  "Face to displaying arrow indicator."
  :group 'riviera-highlighting-faces)

(defun riviera-session-cursor-update (session fileuri lineno)
  (let ((lineno (cond
                 ((numberp lineno)
                  lineno)
                 ((stringp lineno)
                  (string-to-number lineno))))
        (fileuri (riviera-source-fileuri-regularize fileuri)))
    (and lineno
         (floatp lineno)
         (setq lineno 1))             ; restrict to integer
    (plist-put (riviera-session-cursor session) :position (cons fileuri lineno)))
  (riviera-session-cursor-indicate session))

(defun riviera-session-cursor-indicate (session)
  "Display indication marker at the current breaking point.
if DISPLAY-BUFFERP is non-nil, the buffer contains the breaking point
will be displayed in a window."
  (let* ((cursor (riviera-session-cursor session))
         (position (plist-get cursor :position))
         (fileuri (car position))
         (lineno (cdr position))
         (local-path (riviera-session-source-local-path session fileuri)))
    (if local-path
        (riviera-session-cursor-overlay-update session)
      (riviera-dbgp-sequence
          (riviera-dbgp-command-source session fileuri)
        (lambda (session cmd msg err)
          (unless err
            (riviera-session-cursor-overlay-update session)))))))

(defun riviera-session-cursor-overlay-update (session)
  (let* ((cursor (riviera-session-cursor session))
         (overlay (plist-get cursor :overlay))
         (position (plist-get cursor :position))
         (fileuri (car position))
         (lineno (cdr position))
         (local-path (and fileuri
                          (riviera-session-source-local-path session fileuri))))
    (if (null position)
        (when (overlayp overlay)
          (delete-overlay overlay)
          (plist-put cursor :overlay nil))
      (let ((buf (riviera-source-visit local-path))
            pos)
        (when buf
          (with-current-buffer buf
            (ignore-errors
              (save-restriction
                (widen)
                (goto-line lineno)
                (setq pos (point))
                (if (overlayp overlay)
                    (move-overlay overlay pos pos buf)
                  (plist-put cursor :overlay
                             (setq overlay (make-overlay pos pos buf)))
                  (overlay-put overlay
                               'before-string
                               (propertize "x"
                                           'display
                                           (list
                                            '(margin left-margin)
                                            (propertize "=>"
                                                        'face 'riviera-cursor-arrow-face))))))
              (set-window-point (get-buffer-window buf) pos))))))))

(defun riviera-session-cursor-file-visit-handler (session buf)
  (let ((cursor (riviera-session-cursor session))
        (fileuri (riviera-session-source-fileuri session (buffer-file-name buf))))
    (and fileuri
         (equal fileuri (car (plist-get cursor :position)))
         (riviera-session-cursor-overlay-update session))))

(add-hook 'riviera-source-visit-hook #'riviera-session-cursor-file-visit-handler)


;;==============================================================
;; breakpoints
;;==============================================================

(defstruct (riviera-breakpoint
            (:constructor nil)
            (:constructor riviera-breakpoint-make))
  "Breakpoint setting.

types:
  Breakpoint types supported by the current debugger engine.

list:
  Break point list."
  (types '(:line :call :return :exception :conditional))
  list)

(defface riviera-breakpoint-face
  '((((class color))
     :foreground "white"
     :background "red1")
    (t :inverse-video t))
  "Face used to highlight various names.
This includes element and attribute names, processing
instruction targets and the CDATA keyword in a CDATA section.
This is not used directly, but only via inheritance by other faces."
  :group 'riviera-highlighting-faces)

(defcustom riviera-show-breakpoints-debugging-only t
  "*Specify breakpoint markers visibility.
If the value is nil, RIVIERA will always display breakpoint markers.
If non-nil, displays the markers while debugging but hides after
debugging is finished."
  :group 'riviera
  :type 'boolean)

;;--------------------------------------------------------------
;; breakpoint object
;;--------------------------------------------------------------

;; breakpoint object manipulators

(defun riviera-bp-make (session type &rest params)
  "Create a new line breakpoint object."
  (assert (riviera-session-p session))
  (let ((bp (append (list :type type) params)))
    ;; force :lineno and :hit-value value to be integer.
    (mapc (lambda (prop)
            (when (stringp (plist-get bp prop))
              (plist-put bp prop (string-to-number (plist-get bp prop)))))
          '(:lineno :hit-value))
    ;; setup overlay
    (when (and (plist-get params :fileuri)
               (plist-get params :lineno)
               (not (plist-get params :overlay)))
      (riviera-bp-overlay-setup bp))
    ;; Xdebug issue; generate :class and :method name from :function
    (let ((name (plist-get params :function)))
      (and name
           (riviera-session-xdebug-p session)
           (string-match "[:->]" name)
           (plist-put bp :class (replace-regexp-in-string "^\\([^:-]+\\).*" "\\1" name))
           (plist-put bp :method (replace-regexp-in-string "^.*[:>]+" "" name))))
    ;; make sure bp has :state.
    (unless (plist-get params :state)
      (plist-put bp :state "enabled"))
    bp))

(defsubst riviera-bp-finalize (bp)
  "Finalize a breakpoint object."
  (let ((overlay (plist-get bp :overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay)))
  bp)

(defsubst riviera-bp= (lhs rhs)
  "Return t if two breakpoint object point same thing."
  (and (eq (plist-get lhs :type)
           (plist-get rhs :type))
       (eq (plist-get lhs :lineno)
           (plist-get rhs :lineno))
       (equal (plist-get lhs :fileuri)
              (plist-get rhs :fileuri))
       (equal (plist-get lhs :function)
              (plist-get rhs :function))
       (equal (plist-get lhs :exception)
              (plist-get rhs :exception))
       (equal (plist-get lhs :expression)
              (plist-get rhs :expression))))

;; session storage

(defun riviera-session-breakpoint-storage-add (session bp)
  (let* ((storage (riviera-session-storage session))
         (list (plist-get storage :bp)))
    (unless (find bp list :test #'riviera-bp=)
      (let ((bp-copy (copy-sequence bp)))
        (plist-put bp-copy :overlay nil)
        (if list
            (nconc list (list bp-copy))
          (plist-put storage :bp (list bp-copy)))))))

(defun riviera-session-breakpoint-storage-remove (session bp)
  (let* ((storage (riviera-session-storage session))
         (list (plist-get storage :bp)))
    (when (find bp list :test #'riviera-bp=)
      (plist-put storage :bp (delete* bp list :test #'riviera-bp=)))))

(defun riviera-session-breakpoint-storage-restore (session)
  (let ((storage (riviera-session-storage session))
        (breakpoint (riviera-session-breakpoint session)))
    (setf (riviera-breakpoint-list breakpoint)
          (plist-get storage :bp))))

;; session

(defun riviera-session-breakpoint-add (session bp)
  "Add a breakpoint BP to session's breakpoint list."
  (unless (riviera-session-breakpoint-find session bp)
    (let* ((breakpoint (riviera-session-breakpoint session))
           (list (riviera-breakpoint-list breakpoint)))
      (if list
          (nconc list (list bp))
        (setf (riviera-breakpoint-list breakpoint) (list bp))))
    (riviera-session-breakpoint-storage-add session bp)))

(defun riviera-session-breakpoint-remove (session id-or-obj)
  "Remove breakpoints having specific breakpoint id or same meaning objects."
  (setf (riviera-breakpoint-list (riviera-session-breakpoint session))
        (remove-if (if (stringp id-or-obj)
                       (lambda (bp)
                         (when (string= (plist-get bp :id) id-or-obj)
                           (riviera-session-breakpoint-storage-remove session bp)
                           (riviera-bp-finalize bp)))
                     (lambda (bp)
                       (when (riviera-bp= id-or-obj bp)
                         (riviera-session-breakpoint-storage-remove session bp)
                         (riviera-bp-finalize bp))))
                   (riviera-breakpoint-list (riviera-session-breakpoint session)))))

(defun riviera-session-breakpoint-find (session id-or-obj)
  "Find a breakpoint.
id-or-obj should be either a breakpoint id or a breakpoint object."
  (find-if
   (if (stringp id-or-obj)
       (lambda (bp)
         (string= (plist-get bp :id) id-or-obj))
     (lambda (bp)
       (riviera-bp= id-or-obj bp)))
   (riviera-breakpoint-list (riviera-session-breakpoint session))))

;; dbgp

(defun riviera-dbgp-breakpoint-restore (session)
  "Restore breakpoints against new DBGp session."
  (let ((breakpoints (riviera-breakpoint-list (riviera-session-breakpoint session)))
        overlay)
    (setf (riviera-breakpoint-list (riviera-session-breakpoint session)) nil)
    (dolist (bp breakpoints)
      ;; User may edit code since previous debugging session
      ;; so that lineno breakpoints set before may moved.
      ;; The followings try to adjust breakpoint line to
      ;; nearly what user expect.
      (if (and (setq overlay (plist-get bp :overlay))
               (overlayp overlay)
               (overlay-livep overlay)
               (eq (overlay-buffer overlay)
                   (find-buffer-visiting (or (plist-get bp :local-path)
                                             ""))))
          (with-current-buffer (overlay-buffer overlay)
            (save-excursion
              (plist-put bp :lineno (progn
                                      (goto-char (overlay-start overlay))
                                      (riviera-what-line))))))
      (riviera-dbgp-sequence-bind (bp)
        (riviera-dbgp-command-breakpoint-set session bp)
        (lambda (session cmd msg err)
          (riviera-bp-finalize bp))))))

(defun riviera-breakpoint-remove (session bp-or-list)
  "Remove specified breakpoints."
  (dolist (bp (if (riviera-breakpoint-p bp-or-list)
                  (list bp-or-list)
                bp-or-list))
    (let ((bid (plist-get bp :id)))
      (if (and (riviera-session-active-p session)
               bid)
          (riviera-dbgp-sequence-bind (bid)
            (riviera-dbgp-send-command session "breakpoint_remove" (cons "-d" bid))
            (lambda (session cmd msg err)
              ;; remove a stray breakpoint from hash table.
              (when err
                (riviera-session-breakpoint-remove session bid))))
        (setf (riviera-breakpoint-list (riviera-session-breakpoint session))
              (delete-if (lambda (bp1)
                           (riviera-bp= bp bp1))
                         (riviera-breakpoint-list (riviera-session-breakpoint session))))))))

(defun riviera-breakpoint-clear (session)
  "Clear all breakpoints."
  (riviera-breakpoint-remove session
                             (riviera-breakpoint-list (riviera-session-breakpoint session))))

(defun riviera-breakpoint-find-at-pos (session buf pos)
  (with-current-buffer buf
    (remove-if 'null
               (mapcar (lambda (overlay)
                         (let ((bp (overlay-get overlay 'bp)))
                           (and (eq :line (plist-get bp :type))
                                bp)))
                       (overlays-at pos)))))

;; breakpoint list

(defface riviera-breakpoint-fileuri
  '((t (:inherit riviera-backtrace-fileuri)))
  "Face used to highlight fileuri in breakpoint list buffer."
  :group 'riviera-highlighting-faces)

(defface riviera-breakpoint-lineno
  '((t (:inherit riviera-backtrace-lineno)))
  "Face for displaying line numbers in breakpoint list buffer."
  :group 'riviera-highlighting-faces)

(defface riviera-breakpoint-function
  '((t (:inherit font-lock-function-name-face)))
  "Face for displaying line numbers in breakpoint list buffer."
  :group 'riviera-highlighting-faces)

(defun riviera-breakpoint-sort-pred (a b)
  (if (and (stringp (plist-get a :id))
           (equal (plist-get a :id)
                  (plist-get b :id)))
      nil
    (let ((type-rank '(:line 1
                             :call 2
                             :return 3
                             :exception 4
                             :conditional 5
                             :watch 6))
          ax bx cmp)
      (setq cmp (- (plist-get type-rank (plist-get a :type))
                   (plist-get type-rank (plist-get b :type))))
      (if (not (zerop cmp))
          (< cmp 0)
        (case (plist-get a :type)
          (:line
           (setq ax (plist-get a :fileuri))
           (setq bx (plist-get b :fileuri))
           (or (string< ax bx)
               (and (string= ax bx)
                    (< (plist-get a :lineno)
                       (plist-get b :lineno)))))
          (:call
           (string< (plist-get a :function)
                    (plist-get b :function)))
          (:return
           (string< (plist-get a :function)
                    (plist-get b :function)))
          (:exception
           (string< (plist-get a :exception)
                    (plist-get b :exception)))
          (:conditional
           (or (string< (plist-get a :fileuri)
                        (plist-get b :fileuri))
               (progn
                 (setq ax (plist-get a :lineno)
                       bx (plist-get b :lineno))
                 (if (null ax)
                     (not (null ax))
                   (if (null ax)
                       nil
                     (< ax bx))))
               (string< (plist-get a :expression)
                        (plist-get b :expression))))
          (:watch
           (string< (plist-get a :expression)
                    (plist-get b :expression))))))))

;;--------------------------------------------------------------
;; breakpoint list mode
;;--------------------------------------------------------------

(defcustom riviera-breakpoint-list-mode-hook nil
  "*Hook running at when RIVIERA's breakpoint list buffer is initialized."
  :group 'riviera
  :type 'hook)

(defvar riviera-breakpoint-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'riviera-breakpoint-list-mode-mouse-goto)
    (define-key map "\C-m" 'riviera-breakpoint-list-mode-goto)
    (define-key map "d" 'riviera-breakpoint-list-mark-delete)
    (define-key map "u" 'riviera-breakpoint-list-unmark)
    (define-key map "x" 'riviera-breakpoint-list-execute)
    (define-key map "q" 'riviera-quit-window)
    (define-key map "r" 'riviera-breakpoint-list-refresh)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "?" 'riviera-breakpoint-list-mode-help)
    map)
  "Keymap for `riviera-breakpoint-list-mode'")

(defun riviera-breakpoint-list-mode (session)
  "Major mode for RIVIERA's breakpoint list.
The buffer commands are:
\\{riviera-breakpoint-list-mode-map}"
  (unless (eq major-mode 'riviera-breakpoint-list-mode)
    (kill-all-local-variables)
    (use-local-map riviera-breakpoint-list-mode-map)
    (setq major-mode 'riviera-breakpoint-list-mode)
    (setq mode-name "RIVIERA breakpoints")
    (set (make-local-variable 'revert-buffer-function)
         (lambda (a b) nil))
    (and (fboundp 'font-lock-defontify)
         (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (if (fboundp 'run-mode-hooks)
        (run-mode-hooks 'riviera-breakpoint-list-mode-hook)
      (run-hooks 'riviera-breakpoint-list-mode-hook)))
  (set (make-local-variable 'riviera-current-session) session))

(defun riviera-breakpoint-list-mark-delete ()
  "Add deletion mark."
  (interactive)
  (when (eq major-mode 'riviera-breakpoint-list-mode)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert ?D)
      (forward-line 1))))

(defun riviera-breakpoint-list-unmark ()
  "Remove deletion mark."
  (interactive)
  (when (eq major-mode 'riviera-breakpoint-list-mode)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert " ")
      (forward-line 1))))

(defun riviera-breakpoint-list-execute ()
  "Execute breakpoint deletion."
  (interactive)
  (when (eq major-mode 'riviera-breakpoint-list-mode)
    (riviera-with-current-session session
      (let (candidates)
        (save-excursion
          (goto-char (point-min))
          (let ((buffer-read-only nil))
            (while (re-search-forward "^D" nil t)
              (add-to-list 'candidates (get-text-property (point) 'riviera-bp)))))
        (riviera-breakpoint-remove session candidates)
        (when candidates
          (riviera-breakpoint-list-display session))))))

(defun riviera-breakpoint-list-mode-goto (&optional event)
  "Move to the set point of the selected breakpoint."
  (interactive (list last-nonmenu-event))
  (when (eq major-mode 'riviera-breakpoint-list-mode)
    (riviera-with-current-session session
      (let ((bp
             (if (or (null event)
                     (not (listp event)))
                 ;; Actually `event-end' works correctly with a nil argument as
                 ;; well, so we could dispense with this test, but let's not
                 ;; rely on this undocumented behavior.
                 (get-text-property (point) 'riviera-bp)
               (with-current-buffer (window-buffer (posn-window (event-end event)))
                 (save-excursion
                   (goto-char (posn-point (event-end event)))
                   (get-text-property (point) 'riviera-bp)))))
            same-window-buffer-names
            same-window-regexps)
        (let ((fileuri (plist-get bp :fileuri))
              (lineno (plist-get bp :lineno)))
          (and fileuri lineno
               (riviera-session-cursor-update session fileuri lineno)))))))

(defun riviera-breakpoint-list-mode-help ()
  "Display description and key bindings of `riviera-breakpoint-list-mode'."
  (interactive)
  (describe-function 'riviera-breakpoint-list-mode))

(defun riviera-breakpoint-list-refresh (&optional force)
  "Display breakpoint list.
The breakpoint list buffer is under `riviera-breakpoint-list-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (riviera-with-current-session session
    (when (and (riviera-session-active-p session)
               (or force
                   (riviera-session-buffer-visible-p session
                                                     riviera-breakpoint-list-buffer-name)))
      (riviera-dbgp-sequence
          (riviera-dbgp-send-command session "breakpoint_list")
        (lambda (session cmd msg err)
          (riviera-breakpoint-recreate session cmd msg err)
          (riviera-breakpoint-list-display session))))))

(defun riviera-breakpoint-recreate (session cmd msg err)
  "Create breakpoint objects according to the result of `breakpoint_list'."
  (unless err
    (dolist (msg-bp (xml-get-children msg 'breakpoint))
      (let* ((id (xml-get-attribute-or-nil msg-bp 'id))
             (bp (riviera-session-breakpoint-find session id)))
        (unless bp
          (let* ((type (intern-soft (concat ":" (xml-get-attribute msg-bp 'type))))
                 (fileuri (xml-get-attribute-or-nil msg-bp 'filename))
                 (lineno (or (xml-get-attribute-or-nil msg-bp 'lineno)
                             (xml-get-attribute-or-nil msg-bp 'line)))
                 (function (xml-get-attribute-or-nil msg-bp 'function))
                 (class (xml-get-attribute-or-nil msg-bp 'class))
                 (method function)
                 (exception (xml-get-attribute-or-nil msg-bp 'exception))
                 (expression (xml-get-attribute-or-nil msg-bp 'expression))
                 (state (xml-get-attribute-or-nil msg-bp 'state))
                 (local-path (and fileuri
                                  (or (riviera-session-source-local-path session fileuri)
                                      (riviera-source-local-path session fileuri)))))
            (when (stringp lineno)
              (setq lineno (string-to-number lineno))
              (when (floatp lineno) ;; debugger engine may return invalid number.
                (setq lineno 1)))
            (when class
              (setq function (format "%s::%s" (or function "") class)))
            (when expression
              (setq expression (base64-decode-string expression)))
            (riviera-session-breakpoint-add
             session
             (setq bp (riviera-bp-make session type
                                       :id id
                                       :fileuri fileuri
                                       :lineno lineno
                                       :class class
                                       :method method
                                       :function function
                                       :exception exception
                                       :expression expression
                                       :state state
                                       :local-path local-path)))))
        (when bp
          (plist-put bp :hit-count (string-to-number (xml-get-attribute msg-bp 'hit_count)))
          (plist-put bp :hit-value (string-to-number (xml-get-attribute msg-bp 'hit_value))))))))

(defun riviera-breakpoint-list-display (session)
  (let ((buf (riviera-session-buffer session riviera-breakpoint-list-buffer-name))
        (breakpoints (riviera-breakpoint-list (riviera-session-breakpoint session)))
        pos)
    (with-current-buffer buf
      (riviera-breakpoint-list-mode session)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (or (not (listp breakpoints))
                (zerop (length breakpoints)))
            (insert "No breakpoints.\n")
          (setq breakpoints (sort (copy-list breakpoints)
                                  #'riviera-breakpoint-sort-pred))
          (mapc (lambda (bp)
                  (insert "  ")
                  (insert (format "%-11s"
                                  (or (case (plist-get bp :type)
                                        (:line "Line")
                                        (:exception "Exception")
                                        (:call "Call")
                                        (:return "Return")
                                        (:conditional "Conditional")
                                        (:watch "Watch"))
                                      "Unknown")))
                  (if (riviera-session-active-p session)
                      (insert (format "%2s/%-2s  "
                                      (or (plist-get bp :hit-count) "?")
                                      (let ((hit-value (plist-get bp :hit-value)))
                                        (cond
                                         ((null hit-value) "?")
                                         ((zerop hit-value) "*")
                                         (t hit-value)))))
                    (insert " "))
                  (when (plist-get bp :function)
                    (insert (propertize (plist-get bp :function)
                                        'face 'riviera-breakpoint-function))
                    (insert " "))
                  (when (plist-get bp :exception)
                    (insert (propertize (plist-get bp :exception)
                                        'face 'riviera-breakpoint-function))
                    (insert " "))
                  (when (plist-get bp :expression)
                    (insert (format "\"%s\" " (plist-get bp :expression))))
                  (when (plist-get bp :fileuri)
                    (insert (format "%s:%s"
                                    (propertize (plist-get bp :fileuri)
                                                'face 'riviera-breakpoint-fileuri)
                                    (propertize (format "%s" (or (plist-get bp :lineno) "*"))
                                                'face 'riviera-breakpoint-lineno))))
                  (insert "\n")
                  (put-text-property (save-excursion (forward-line -1) (point))
                                     (point)
                                     'riviera-bp bp))
                breakpoints))
        (setq header-line-format
              (concat "  Type        "
                      (if (riviera-session-active-p session) "Hits  " "")
                      "Property"))
        (goto-char (point-min))))
    (save-selected-window
      (riviera-dbgp-display-window buf))))

;; overlay

(defun riviera-bp-overlay-setup (bp)
  "Create an overlay for a breakpoint BP."
  (riviera-bp-finalize bp)
  (let* ((local-path (plist-get bp :local-path))
         (overlay (and (stringp local-path)
                       (find-buffer-visiting local-path)
                       (riviera-overlay-make-line (plist-get bp :lineno)
                                                  (find-buffer-visiting local-path)))))
    (when overlay
      (overlay-put overlay 'face 'riviera-breakpoint-face)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'bp bp)
      (overlay-put overlay 'modification-hooks '(riviera-bp-overlay-modified))
      (overlay-put overlay 'insert-in-front-hooks '(riviera-bp-overlay-inserted-in-front))
      (plist-put bp :overlay overlay)))
  bp)

(defun riviera-bp-overlay-hide (session)
  "Hide breakpoint overlays."
  (mapc (lambda (bp)
          (let ((overlay (plist-get bp :overlay)))
            (and (overlayp overlay)
                 (overlay-livep overlay)
                 (overlay-put overlay 'face nil))))
        (riviera-breakpoint-list (riviera-session-breakpoint session))))

(defun riviera-bp-overlay-modified (overlay afterp beg end &optional len)
  "A callback function invoked when inside of an overlay is modified.
With this callback RIVIERA tracks displacements of line breakpoints."
  (when afterp
    (save-excursion
      (save-restriction
        (widen)
        (let* ((lineno-from (progn (goto-char (overlay-start overlay))
                                   (riviera-what-line)))
               (lineno-to (progn (goto-char (overlay-end overlay))
                                 (riviera-what-line)))
               (lineno lineno-from))
          (goto-line lineno)
          (while (and (looking-at "[ \t]*$")
                      (< lineno lineno-to))
            (forward-line)
            (incf lineno))
          (if (< lineno-from lineno)
              (plist-put (overlay-get overlay 'bp) :lineno lineno))
          (goto-line lineno)
          (beginning-of-line)
          (move-overlay overlay (point) (save-excursion
                                          (forward-line)
                                          (point))))))))

(defun riviera-bp-overlay-inserted-in-front (overlay afterp beg end &optional len)
  "A callback function invoked when text in front of an overlay is modified.
With this callback RIVIERA tracks displacements of line breakpoints."
  (if afterp
      (save-excursion
        (goto-line (progn (goto-char (overlay-start overlay))
                          (riviera-what-line)))
        (move-overlay overlay (point) (save-excursion
                                        (forward-line)
                                        (point))))))

(defun riviera-bp-overlay-restore (session buf)
  "A callback function invoked when emacs visits a new file.
RIVIERA may place overlay markers if there are line breakpoints in
the file."
  (mapc (lambda (bp)
          (and (plist-get bp :lineno)
               (eq buf (find-buffer-visiting (or (plist-get bp :local-path)
                                                 "")))
               (riviera-bp-overlay-setup bp)))
        (riviera-breakpoint-list (riviera-session-breakpoint session))))

(defun riviera-session-breakpoint-init (session)
  (setf (riviera-session-breakpoint session) (riviera-breakpoint-make))
  (riviera-session-breakpoint-storage-restore session))

(add-hook 'riviera-session-enter-hook #'riviera-session-breakpoint-init)

(defun riviera-session-breakpoint-release (session)
  (when riviera-show-breakpoints-debugging-only
    (riviera-bp-overlay-hide session)))

(add-hook 'riviera-session-exit-hook #'riviera-session-breakpoint-release)

(defun riviera-dbgp-breakpoint-store-types (session cmd msg err)
  (when (equal "1" (xml-get-attribute msg 'supported))
    (let ((types (mapcar
                  (lambda (type)
                    (intern (concat ":" type)))
                  (split-string (or (car (xml-node-children msg))
                                    "")
                                " "))))
      (if (riviera-session-xdebug-p session)
          ;; Xdebug 2.0.3 supports the following types but they aren't
          ;; included in the response. Push them in the list manually.
          (setq types (append types '(:exception :conditional))))
      (unless types
        ;; Some debugger engines are buggy;
        ;; they don't return breakpoint types correctly.
        ;; To them put all of types to the list.
        (setq types '(:line :call :return :exception :conditional :watch)))
      (setf (riviera-breakpoint-types (riviera-session-breakpoint session)) types))))

(add-hook 'riviera-source-visit-hook #'riviera-bp-overlay-restore)

;;; breakpoint_set

(defun riviera-dbgp-command-breakpoint-set (session bp)
  "Send \`breakpoint_set\' command."
  (if (not (riviera-session-active-p session))
      (riviera-session-breakpoint-add session bp)
    (let ((obp (riviera-session-breakpoint-find session bp)))
      (if (and obp
               (plist-get obp :id))
          (riviera-dbgp-send-command session "breakpoint_update"
                                     (cons "-d" (plist-get obp :id))
                                     (cons "-h" (or (plist-get bp :hit-value)
                                                    0))
                                     (cons "-o" ">="))
        (let ((params
               (remove nil
                       (list
                        (cons "-t"
                              (substring (symbol-name (plist-get bp :type)) 1))
                        (and (plist-get bp :fileuri)
                             (cons "-f" (plist-get bp :fileuri)))
                        (and (plist-get bp :lineno)
                             (cons "-n" (plist-get bp :lineno)))
                        (and (plist-get bp :class)
                             (riviera-session-xdebug-p session)
                             (cons "-a" (plist-get bp :class)))
                        (and (plist-get bp :function)
                             (if (and (riviera-session-xdebug-p session)
                                      (plist-get bp :method))
                                 (cons "-m" (plist-get bp :method))
                               (cons "-m" (plist-get bp :function))))
                        (and (plist-get bp :exception)
                             (cons "-x" (plist-get bp :exception)))
                        (cons "-h" (or (plist-get bp :hit-value) 0))
                        (cons "-o" ">=")
                        (cons "-s" (or (plist-get bp :state)
                                       "enabled"))
                        (cons "-r" (if (plist-get bp :run-once) 1 0))
                        (and (plist-get bp :expression)
                             (cons "--"
                                   (base64-encode-string
                                    (plist-get bp :expression))))))))
          (when params
            (apply 'riviera-dbgp-send-command session "breakpoint_set" params)))))))

(defun riviera-dbgp-response-breakpoint-set (session cmd msg)
  "A response message handler for \`breakpoint_set\' command."
  (unless (eq (riviera-cmd-param-get cmd "-r") 1) ; unless :run-once is set
    (let* ((type (intern (concat ":" (riviera-cmd-param-get cmd "-t"))))
           (id (xml-get-attribute-or-nil msg 'id))
           (fileuri (riviera-cmd-param-get cmd "-f"))
           (lineno (riviera-cmd-param-get cmd "-n"))
           (function (riviera-cmd-param-get cmd "-m"))
           (class (riviera-cmd-param-get cmd "-a"))
           (method function)
           (exception (riviera-cmd-param-get cmd "-x"))
           (expression (riviera-cmd-param-get cmd "--"))
           (hit-value (riviera-cmd-param-get cmd "-h"))
           (state (riviera-cmd-param-get cmd "-s"))
           (local-path (and fileuri
                            (or (riviera-session-source-local-path session fileuri)
                                (riviera-source-local-path session fileuri))))
           bp)
      (when expression
        (setq expression (base64-decode-string expression)))
      (riviera-session-breakpoint-add session
                                      (setq bp (riviera-bp-make session type
                                                                :id id
                                                                :fileuri fileuri
                                                                :lineno lineno
                                                                :class class
                                                                :method method
                                                                :function function
                                                                :exception exception
                                                                :expression expression
                                                                :hit-value hit-value
                                                                :local-path local-path
                                                                :state state))))
    (riviera-breakpoint-list-refresh)))

(defun riviera-dbgp-response-breakpoint-update (session cmd msg)
  "A response message handler for `breakpoint_update' command."
  (let* ((id (riviera-cmd-param-get cmd "-d"))
         (bp (riviera-session-breakpoint-find session id)))
    (when bp
      (plist-put bp :hit-value (riviera-cmd-param-get cmd "-h"))
      (riviera-breakpoint-list-refresh))))

;;; breakpoint_remove

(defun riviera-dbgp-command-breakpoint-remove (session bid)
  "Send `breakpoint_remove' command."
  (if (riviera-session-active-p session)
      (riviera-dbgp-sequence-bind (bid)
        (riviera-dbgp-send-command session "breakpoint_remove" (cons "-d" bid))
        (lambda (session cmd msg err)
          (when (dbgp-xml-get-error-message msg)
            ;; remove a stray breakpoint from hash table.
            (riviera-session-breakpoint-remove session bid)
            (riviera-breakpoint-list-refresh))))
    (riviera-session-breakpoint-remove session bid)))

(defun riviera-dbgp-response-breakpoint-remove (session cmd msg)
  "A response message handler for \`breakpoint_remove\' command."
  (let* ((id (riviera-cmd-param-get cmd "-d"))
         (bp (riviera-session-breakpoint-find session id)))
    (riviera-session-breakpoint-remove session id)
    (riviera-breakpoint-list-refresh)))

(defun riviera-dbgp-command-breakpoint-list (session)
  "Send `breakpoint_list' command."
  (riviera-dbgp-send-command session "breakpoint_list"))

(defun riviera-dbgp-response-breakpoint-list (session cmd msg)
  "A response message handler for \`breakpoint_list\' command."
  t)

(defun riviera-dbgp-breakpoint-list-refresh (session)
  (riviera-breakpoint-list-refresh))



;;==============================================================
;; context
;;==============================================================

(defface riviera-context-category-face
  '((((class color))
     :background "purple"
     :foreground "white"
     :bold t))
  "Face used to highlight context category name."
  :group 'riviera-highlighting-faces)

(defface riviera-context-variable-face
  '((t :inherit 'font-lock-variable-name-face))
  "Face used to highlight variable name."
  :group 'riviera-highlighting-faces)

(defface riviera-context-type-face
  '((t :inherit 'font-lock-type-face))
  "Face used to highlight type name."
  :group 'riviera-highlighting-faces)

(defface riviera-context-class-face
  '((t :inherit 'font-lock-constant-face))
  "Face used to highlight type name."
  :group 'riviera-highlighting-faces)

(defface riviera-context-string-face
  '((t :inherit 'font-lock-string-face))
  "Face used to highlight string value."
  :group 'riviera-highlighting-faces)

(defface riviera-context-constant-face
  '((t :inherit 'font-lock-constant-face))
  "Face used to highlight numeric value."
  :group 'riviera-highlighting-faces)

(defstruct (riviera-context
            (:constructor nil)
            (:constructor riviera-context-make))
  names    ; context names alist(KEY: context name, VALUE: context id)
  tid  ; transaction id to which the current context variables belong.
  variables                     ;
  expanded-variables            ; context variables in expanded state.
  (depth 0)
  )

(defvar riviera-context-where "")
(defvar riviera-context-loading nil)
(defvar riviera-context-property-tree-fill-children-hook 'riviera-context-tree-children-fill)

(defun riviera-session-context-init (session)
  (setf (riviera-session-context session) (riviera-context-make)))
(add-hook 'riviera-session-enter-hook #'riviera-session-context-init)

;; context list buffer

(defsubst riviera-session-context-buffer (session)
  (let ((buf (riviera-session-buffer session riviera-context-buffer-name)))
    (with-current-buffer buf
      (riviera-context-mode session))
    buf))

(defsubst riviera-session-context-buffer-get (session)
  (riviera-session-buffer-get session riviera-context-buffer-name))

(defsubst riviera-session-context-buffer-live-p (session)
  (riviera-session-buffer-live-p session riviera-context-buffer-name))

(defsubst riviera-session-context-buffer-visible-p (session)
  (riviera-session-buffer-visible-p session riviera-context-buffer-name))

;;

(defsubst riviera-session-context-tid (session)
  (riviera-context-tid (riviera-session-context session)))

(defsubst riviera-session-context-names (session)
  (riviera-context-names (riviera-session-context session)))

(defsubst riviera-session-context-depth (session)
  (riviera-context-depth (riviera-session-context session)))

;; context list accessors

(defsubst riviera-session-context-list (session cid)
  "Get context list for the context id CID."
  (assq cid
        (riviera-context-variables
         (riviera-session-context session))))

(defsubst riviera-session-context-list-old (session cid)
  "Get previous context list for the context id CID."
  (cdr (assq 'old (riviera-session-context-list session cid))))

(defsubst riviera-session-context-list-new (session cid)
  "Get the current context list for the context id CID."
  (cdr (assq 'new (riviera-session-context-list session cid))))

(defsubst riviera-session-context-list-update (session cid list)
  "Update the current context list for the context id CID with LIST."
  (let* ((clist (riviera-session-context-list session cid))
         (old (assq 'new clist)))
    (setcdr clist (list (cons 'old (cdr old))
                        (cons 'new list)))))

;; context property list accessors

(defsubst riviera-context-property-has-children (property)
  "Check whether PROPERTY has any children."
  (equal "1" (xml-get-attribute-or-nil property 'children)))

(defsubst riviera-context-property-format-bool (value)
  "Format VALUE in the debuggee language expression."
  (let ((bool (if (equal "0" value) nil t)))
    (if bool "true" "false")))

(defsubst riviera-context-property-format-array-name (property)
  "Format array element name in the debuggee language expression."
  (format "%s[%s]"
          (propertize (xml-get-attribute property 'name)
                      'face 'riviera-context-variable-face)
          (propertize (xml-get-attribute property 'numchildren)
                      'face 'riviera-context-constant-face)))

(defsubst riviera-context-property-attribute (property sym)
  "Get attribute SYM from PROPERTY."
  ;; DBGp specs specifies property attributes of context_get and
  ;; property_get commands. But some debugger engines have values not
  ;; as attributes but child elements."
  (let ((node (car (xml-get-children property sym))))
    (if (consp node)
        (riviera-dbgp-decode-string (xml-node-children node)
                                    (xml-get-attribute node 'encoding)
                                    'utf-8)
      (xml-get-attribute property sym))))

(defsubst riviera-context-property-name (property)
  "Get name attribute value from PROPERTY."
  (riviera-context-property-attribute property 'name))

(defsubst riviera-context-property-fullname (property)
  "Get fullname attribute value from PROPERTY."
  (riviera-context-property-attribute property 'fullname))

(defsubst riviera-context-property-value (property)
  "Get value from PROPERTY."
  (let ((node (car (xml-get-children property 'value))))
    (if (consp node)
        (riviera-dbgp-decode-string (xml-node-children node)
                                    (xml-get-attribute node 'encoding)
                                    'utf-8)
      (riviera-dbgp-decode-string (xml-node-children property)
                                  (xml-get-attribute property 'encoding)
                                  'utf-8))))

(defun riviera-context-property-typeinfo (property)
  "Get type information of PROPERTY to display it in the context buffer."
  (let ((type (and (xml-get-attribute-or-nil property 'type)
                   (intern (xml-get-attribute-or-nil property 'type))))
        typeinfo)
    (setq typeinfo
          (cond
           ((null type) nil)
           ((member type '(int float))
            (list :type type
                  :type-visiblep nil
                  :value-face 'riviera-context-constant-face))
           ((eq type 'bool)
            (list :type type
                  :type-visiblep nil
                  :value-face 'riviera-context-constant-face
                  :value-formatter 'riviera-context-property-format-bool))
           ((eq type 'string)
            (list :type type
                  :type-visiblep nil
                  :value-face 'riviera-context-string-face))
           ((member type '(array hash))
            (list :type type
                  :type-visiblep nil
                  :name-formatter 'riviera-context-property-format-array-name
                  :value-face 'default
                  :value-formatter (lambda (value) "")))
           ((eq type 'null)
            (list :type type
                  :type-visiblep nil
                  :value-face 'riviera-context-constant-face
                  :value-formatter (lambda (value) "null")))
           ((eq type 'resource)
            (list :type type
                  :type-visiblep t
                  :value-face 'riviera-context-constant-face))
           ((eq type 'object)
            (list :type (if (xml-get-attribute-or-nil property 'classname)
                            (intern (xml-get-attribute-or-nil property 'classname))
                          type)
                  :type-visiblep t
                  :type-face 'riviera-context-class-face
                  :value-face 'default))
           ((eq type 'uninitialized)
            (list :type 'undef
                  :type-visiblep t
                  :type-face 'riviera-context-type-face
                  :value-face 'default))
           (t
            (list :type type
                  :type-visiblep t
                  :type-face 'riviera-context-type-face
                  :value-face 'default))))
    typeinfo))

;;--------------------------------------------------------------
;; context property tree widget
;;--------------------------------------------------------------

(defun riviera-context-property-tree-open (tree)
  "Expand TREE."
  (let ((marker (widget-get tree :from)))
    (when (markerp marker)
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (call-interactively 'widget-button-press)
        (unless (widget-get tree :open)
          (call-interactively 'widget-button-press))))))

(defun riviera-context-property-tree-expand-p (tree)
  "A tree widget callback function to indicate whether TREE is able to expand."
  (or (riviera-context-property-tree-has-complete-children tree)
      (and (run-hook-with-args 'riviera-context-property-tree-fill-children-hook
                               tree)
           nil)))

(defun riviera-context-property-tree-expand (tree)
  "A tree widget callback function to create child list of TREE."
  (mapcar #'riviera-context-property-tree-create-node
          (xml-get-children (widget-get tree :property) 'property)))

(defun riviera-context-property-tree-has-complete-children (tree)
  "Determine whether TREE has complete child nodes.
Child nodes can be short for :property property of TREE."
  (let* ((property (widget-get tree :property))
         (children (xml-get-children property 'property))
         (numchildren (and children
                           (string-to-number (xml-get-attribute property 'numchildren)))))
    (and children
         (<= numchildren (length children)))))

(defun riviera-context-property-tree-create-node (property)
  "Create nodes which represent PROPERTY."
  (let* ((typeinfo (riviera-context-property-typeinfo property))
         (value (riviera-context-property-value property))
         tag)
    (let ((formatter (plist-get typeinfo :name-formatter)))
      (setq tag
            (if formatter
                (funcall formatter property)
              (propertize (riviera-context-property-name property)
                          'face 'riviera-context-variable-face))))
    (when (plist-get typeinfo :type-visiblep)
      (setq tag (concat tag
                        (format "(%s)" (propertize
                                        (symbol-name (plist-get typeinfo :type))
                                        'face (plist-get typeinfo :type-face))))))
    (let ((formatter (plist-get typeinfo :value-formatter)))
      (when (or value formatter)
        (setq tag (format "%-32s %s" tag
                          (propertize (if formatter
                                          (funcall formatter value)
                                        value)
                                      'face (plist-get typeinfo :value-face))))))
    (if (riviera-context-property-has-children property)
        (list 'tree-widget
              :tag tag
              :property property
              :expander 'riviera-context-property-tree-expand
              :expander-p 'riviera-context-property-tree-expand-p)
      (list 'item :tag (concat "   " tag)))))

(defun riviera-context-property-tree-context-id (tree)
  "Get context id to which TREE belongs."
  (when tree
    (let ((cid (widget-get tree :context-id)))
      (or cid
          (riviera-context-property-tree-context-id (widget-get tree :parent))))))

;;--------------------------------------------------------------
;; context functions
;;--------------------------------------------------------------

(defun riviera-context-list-fetch (session callback)
  "Fetch context variables for a SESSION from debuggee server.
After fetching it calls CALLBACK function."
  (let ((context (riviera-session-context session)))
    (when (riviera-context-names context)
      (unless (riviera-context-variables context)
        (setf (riviera-context-variables context)
              (mapcar (lambda (context)
                        (list (cdr context)))
                      (riviera-context-names context))))
      ;; Remain the current tid.
      ;; It is possible that the current context proceeds by step_in or
      ;; other continuous commands while retrieving variables.
      ;; To avoid mixing variables with multi context, remain something at here,
      ;; tid, and check the value in the retrieving process.
      (setf (riviera-context-tid context) (riviera-session-tid session))
      (riviera-context-list-fetch-loop session
                                       (riviera-context-tid context)
                                       (riviera-context-depth context)
                                       (mapcar (lambda (context)
                                                 (cdr context))
                                               (riviera-context-names context))
                                       callback))))

(defun riviera-context-list-fetch-loop (session tid-save depth context-id-list callback)
  (let ((buf (riviera-session-context-buffer-get session)))
    (when buf
      (with-current-buffer buf
        (setq riviera-context-loading t))
      (riviera-dbgp-sequence-bind (tid-save depth context-id-list callback)
        (riviera-dbgp-command-context-get session (car context-id-list) depth)
        (lambda (session cmd msg err)
          (when (and (not err)
                     (eq tid-save (riviera-session-context-tid session))
                     (riviera-session-context-buffer-live-p session))
            (riviera-session-context-list-update session
                                                 (riviera-cmd-param-get cmd "-c")
                                                 (xml-get-children msg 'property))
            (if (cdr context-id-list)
                (riviera-context-list-fetch-loop session tid-save depth
                                                 (cdr context-id-list) callback)
              (riviera-context-fill-buffer session)
              (with-current-buffer (riviera-session-context-buffer-get session)
                (setq riviera-context-loading nil))
              (funcall callback session))))))))

(defun riviera-context-fill-buffer (session)
  "Fill the context buffer with locally stored context list."
  (let ((buf (riviera-session-context-buffer-get session)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (widen)
          (erase-buffer)
          (dolist (context-name (riviera-session-context-names session))
            (let ((new (riviera-session-context-list-new session (cdr context-name))))
              (apply 'widget-create
                     'tree-widget
                     :tag (car context-name)
                     :context-id (cdr context-name)
                     :open t
                     (mapcar #'riviera-context-property-tree-create-node new))))
          (widget-setup))
        (goto-char (point-min))))))

(defun riviera-context-tree-children-fill (tree &optional tid-save)
  (riviera-with-current-session session
    (let ((tid-save (or tid-save
                        (riviera-session-context-tid session)))
          (completed (riviera-context-property-tree-has-complete-children tree))
          (buf (riviera-session-context-buffer-get session)))
      (when (and (buffer-live-p buf)
                 (eq tid-save (riviera-session-context-tid session)))
        (with-current-buffer buf
          (setq riviera-context-loading (not completed)))
        (if completed
            (riviera-context-property-tree-open tree)
          (riviera-context-tree-children-fill-1 session tree tid-save))))))

(defun riviera-context-tree-children-fill-1 (session tree tid-save)
  (let* ((property (widget-get tree :property))
         (children (xml-get-children property 'property)))
    (with-current-buffer (riviera-session-context-buffer-get session)
      ;; -- comment on :property-page property --
      ;; debugger engine may lack of PAGESIZE in property message(bug).
      ;; so the following code doesn't rely on PAGESIZE but uses own
      ;; :property-page widget property.
      (let* ((nextpage (if (widget-get tree :property-page)
                           (1+ (widget-get tree :property-page))
                         (if children 1 0)))
             (args (list :depth (riviera-session-context-depth session)
                         :context-id (riviera-context-property-tree-context-id tree)
                         :name (riviera-context-property-fullname property)
                         :page nextpage)))
        (widget-put tree :property-page nextpage)
        (when (xml-get-attribute-or-nil property 'key)
          (plist-put args :key (xml-get-attribute-or-nil property 'key)))
        (riviera-dbgp-sequence-bind (tree tid-save)
          (riviera-dbgp-command-property-get session args)
          (lambda (session cmd msg err)
            (unless err
              (riviera-context-tree-children-append session
                                                    tid-save
                                                    tree
                                                    (car (xml-get-children msg 'property)))
              (riviera-context-tree-children-fill tree
                                                  tid-save))))))))

(defun riviera-context-tree-children-append (session tid-save tree property)
  (if (eq tid-save (riviera-session-context-tid session))
      (let ((tree-prop (widget-get tree :property)))
        (nconc (or (cddr tree-prop)
                   tree-prop)
               (cddr property)))))

(defun riviera-context-list-refresh (session depth &optional force)
  (when (and (riviera-session-active-p session)
             (or force
                 (riviera-session-context-buffer-visible-p session)))
    (riviera-context-list-display session depth (not force))))

(defun riviera-context-list-display (session depth &optional no-select)
  "Display context variables in the context buffer."
  (unless (riviera-session-active-p session)
    (error "RIVIERA is out of debugging session."))
  (when (or (< depth 0)
            (< (length (riviera-session-stack session)) (1+ depth)))
    (error "RIVIERA context display: invalid depth: %S" depth))
  (setf (riviera-context-depth (riviera-session-context session)) depth)
  (let ((buf (riviera-session-context-buffer session)))
    (with-current-buffer buf
      (setq riviera-context-where
            (xml-get-attribute (nth depth (riviera-session-stack session))
                               'where)))
    (unless no-select
      (riviera-dbgp-display-window buf))
    (riviera-context-list-fetch session
                                (riviera-lexical-bind (buf no-select)
                                  (lambda (session)
                                    (and (buffer-live-p buf)
                                         (not no-select)
                                         (riviera-dbgp-display-window buf)))))))

;;--------------------------------------------------------------
;; context mode
;;--------------------------------------------------------------

(defcustom riviera-context-mode-hook nil
  "*Hook running at when RIVIERA's context buffer is initialized."
  :group 'riviera
  :type 'hook)

(defvar riviera-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'widget-forward)
    (define-key map "S-\t" 'widget-backward)
    ;;(define-key map "\C-m" 'riviera-context-mode-expand)
    ;;(define-key map "e" 'riviera-context-mode-edit)
    (define-key map "r" 'riviera-context-mode-refresh)
    (define-key map "q" 'riviera-quit-window)
    (define-key map "p" 'widget-backward)
    (define-key map "n" 'widget-forward)
    (define-key map "?" 'riviera-context-mode-help)
    map)
  "Keymap for `riviera-context-mode'")

(defun riviera-context-mode (session)
  "Major mode for RIVIERA's context output.
The buffer commands are:
\\{riviera-context-mode-map}"
  (interactive)
  (unless (eq major-mode 'riviera-context-mode)
    (kill-all-local-variables)
    (use-local-map riviera-context-mode-map)
    (setq major-mode 'riviera-context-mode)
    (setq mode-name "RIVIERA context")
    (set (make-local-variable 'revert-buffer-function)
         (lambda (a b) nil))
    (and (fboundp 'font-lock-defontify)
         (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
    (if (fboundp 'run-mode-hooks)
        (run-mode-hooks 'riviera-context-mode-hook)
      (run-hooks 'riviera-context-mode-hook))
    (buffer-disable-undo)
    (set (make-local-variable 'riviera-context-where) "")
    (set (make-local-variable 'riviera-context-loading) nil)
    (set (make-local-variable 'tree-widget-theme) "riviera")
    (setq header-line-format
          (list
           "Where: "
           'riviera-context-where
           "   "
           '(riviera-context-loading "(loading...)")
           ))
    (setq buffer-read-only t))
  (set (make-local-variable 'riviera-current-session) session))

(defun riviera-context-mode-refresh (&optional force)
  "Refresh the context buffer."
  (interactive)
  (riviera-with-current-session session
    (riviera-context-list-refresh session
                                  (riviera-session-context-depth session)
                                  force)))

(defun riviera-context-mode-help ()
  "Display description and key bindings of `riviera-context-mode'."
  (interactive)
  (describe-function 'riviera-context-mode))

;; context

(defun riviera-dbgp-command-context-names (session &optional depth)
  (riviera-dbgp-send-command session "context_names"
                             (and (numberp depth)
                                  (cons "-d" depth))))

(defun riviera-dbgp-response-context-names (session cmd msg)
  (setf (riviera-context-names (riviera-session-context session))
        (mapcar (lambda (context)
                  (let ((name (xml-get-attribute context 'name))
                        (id (xml-get-attribute context 'id)))
                    (cons name (string-to-number id))))
                (xml-get-children msg 'context))))

;; context

(defun riviera-dbgp-command-context-get (session context-id &optional depth)
  (riviera-dbgp-send-command session "context_get"
                             (cons "-c" context-id)
                             (and depth
                                  (cons "-d" depth))))

;; property

(defun riviera-dbgp-command-property-get (session &rest args)
  (apply 'riviera-dbgp-send-command session "property_get"
         (mapcar (lambda (key)
                   (let ((arg (plist-get (car args) key)))
                     (when arg
                       (cons (riviera-cmd-param-for key) arg))))
                 '(:depth :context-id :name :max-data-size :type :page :key :address))))


;;==============================================================
;; stack
;;==============================================================

;; backtrace

(defface riviera-backtrace-fileuri
  '((((class color))
     (:foreground "green" :weight bold))
    (t (:weight bold)))
  "Face used to highlight fileuri in backtrace buffer."
  :group 'riviera-highlighting-faces)

(defface riviera-backtrace-lineno
  '((t :inherit font-lock-variable-name-face))
  "Face for displaying line numbers in backtrace buffer."
  :group 'riviera-highlighting-faces)

(defcustom riviera-backtrace-mode-hook nil
  "*Hook running at when RIVIERA's backtrace buffer is initialized."
  :group 'riviera
  :type 'hook)

(defun riviera-backtrace-buffer (session)
  (let ((buf (get-buffer-create (riviera-session-buffer session riviera-backtrace-buffer-name))))
    (with-current-buffer buf
      (riviera-backtrace-mode session))
    buf))

(defun riviera-backtrace (session)
  "Display backtrace."
  (unless (riviera-session-active-p session)
    (error "RIVIERA is out of debugging session."))
  (with-current-buffer (riviera-backtrace-buffer session)
    (let ((inhibit-read-only t)
          (stack (riviera-session-stack session)))
      (erase-buffer)
      (dotimes (i (length stack))
        (let* ((stack (nth i stack))
               (fileuri (riviera-source-fileuri-regularize (xml-get-attribute stack 'filename)))
               (lineno (xml-get-attribute stack 'lineno))
               (where (xml-get-attribute stack 'where))
               (level (xml-get-attribute stack 'level)))
          (insert (format "%s:%s %s\n"
                          (propertize fileuri 'face "riviera-backtrace-fileuri")
                          (propertize lineno 'face "riviera-backtrace-lineno")
                          where))
          (put-text-property (save-excursion (forward-line -1) (point))
                             (point)
                             'riviera-stack-frame
                             (list :fileuri fileuri
                                   :lineno lineno
                                   :level (string-to-number level)))))
      (goto-char (point-min)))
    (riviera-dbgp-display-window (riviera-backtrace-buffer session))))

(defvar riviera-backtrace-mode-map nil
  "Keymap for `riviera-backtrace-mode'")
(unless riviera-backtrace-mode-map
  (setq riviera-backtrace-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map [mouse-2] 'riviera-backtrace-mode-mouse-goto)
          (define-key map "\C-m" 'riviera-backtrace-mode-goto)
          (define-key map "q" 'riviera-quit-window)
          (define-key map "p" 'previous-line)
          (define-key map "n" 'next-line)
          (define-key map "v" 'riviera-backtrace-mode-context)
          (define-key map "?" 'riviera-backtrace-mode-help)
          map)))

(defun riviera-backtrace-mode (session)
  "Major mode for RIVIERA's backtrace output.
The buffer commands are:
\\{riviera-backtrace-mode-map}"
  (interactive)
  (unless (eq 'riviera-backtrace-mode major-mode)
    (kill-all-local-variables)
    (use-local-map riviera-backtrace-mode-map)
    (setq major-mode 'riviera-backtrace-mode)
    (setq mode-name "RIVIERA backtrace")
    (set (make-local-variable 'revert-buffer-function)
         (lambda (a b) nil))
    (and (fboundp 'font-lock-defontify)
         (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (if (fboundp 'run-mode-hooks)
        (run-mode-hooks 'riviera-backtrace-mode-hook)
      (run-hooks 'riviera-backtrace-mode-hook)))
  (set (make-local-variable 'riviera-current-session) session))

(defalias 'riviera-backtrace-mode-mouse-goto 'riviera-backtrace-mode-goto)
(defun riviera-backtrace-mode-goto (&optional event)
  (interactive (list last-nonmenu-event))
  (riviera-with-current-session session
    (let ((stack-frame
           (if (or (null event)
                   (not (listp event)))
               ;; Actually `event-end' works correctly with a nil argument as
               ;; well, so we could dispense with this test, but let's not
               ;; rely on this undocumented behavior.
               (get-text-property (point) 'riviera-stack-frame)
             (with-current-buffer (window-buffer (posn-window (event-end event)))
               (save-excursion
                 (goto-char (posn-point (event-end event)))
                 (get-text-property (point) 'riviera-stack-frame)))))
          same-window-buffer-names
          same-window-regexps)
      (when stack-frame
        (riviera-session-cursor-update session
                                       (plist-get stack-frame :fileuri)
                                       (plist-get stack-frame :lineno))))))

(defun riviera-backtrace-mode-help ()
  "Display description and key bindings of `riviera-backtrace-mode'."
  (interactive)
  (describe-function 'riviera-backtrace-mode))

(defvar riviera-dbgp-stack-update-hook nil)

(defun riviera-backtrace-mode-context ()
  (interactive)
  (riviera-with-current-session session
    (let ((stack (get-text-property (point) 'riviera-stack-frame)))
      (when stack
        (run-hook-with-args 'riviera-dbgp-stack-update-hook
                            session (plist-get stack :level))))))

;;; stack_get

(defun riviera-dbgp-command-stack-get (session)
  "Send \`stack_get\' command."
  (riviera-dbgp-send-command session "stack_get"))

(defun riviera-dbgp-stack-update (session)
  (riviera-dbgp-sequence
      (riviera-dbgp-command-stack-get session)
    (lambda (session cmd msg err)
      (unless err
        (setf (riviera-session-stack session) (xml-get-children msg 'stack))
        (let* ((stack (car (xml-get-children msg 'stack)))
               (fileuri (xml-get-attribute-or-nil stack 'filename))
               (lineno (xml-get-attribute-or-nil stack 'lineno)))
          (and fileuri lineno
               (riviera-session-cursor-update session fileuri lineno)))
        (run-hook-with-args 'riviera-dbgp-stack-update-hook
                            session 0)))))


;;==============================================================
;; redirect
;;==============================================================

(defconst riviera-redirect-combine-buffer-name "*RIVIERA<%s> output*"
  "Name for the debuggee script's STDOUT and STDERR redirection buffer.")
(defconst riviera-redirect-stdout-buffer-name "*RIVIERA<%s> stdout*"
  "Name for the debuggee script's STDOUT redirection buffer.")
(defconst riviera-redirect-stderr-buffer-name "*RIVIERA<%s> stderr*"
  "Name for the debuggee script's STDERR redirection buffer.")

(defstruct (riviera-redirect
            (:constructor nil)
            (:constructor riviera-redirect-make))
  (stdout :redirect)
  (stderr :redirect)
  (combine t)
  (coding-system 'utf-8))

(defcustom riviera-dbgp-redirect-buffer-init-hook nil
  "*Hook running at when a redirection buffer is created."
  :group 'riviera
  :type 'hook)

(defun riviera-session-redirect-init (session)
  (setf (riviera-session-redirect session) (riviera-redirect-make))
  (dolist (type '(:stdout :stderr))
    (let ((buf (get-buffer (riviera-session-redirect-buffer-name session type))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (erase-buffer)))))))

(add-hook 'riviera-session-enter-hook #'riviera-session-redirect-init)

(defun riviera-session-redirect-buffer (session type)
  (let ((bufname (riviera-session-redirect-buffer-name session type)))
    (when bufname
      (or (get-buffer bufname)
          (with-current-buffer (get-buffer-create bufname)
            (unless (local-variable-p 'riviera-dynamic-property-buffer-p)
              (set (make-local-variable 'riviera-dynamic-property-buffer-p) t)
              (setq buffer-undo-list t)
              (run-hook-with-args 'riviera-dbgp-redirect-buffer-init-hook (current-buffer)))
            (current-buffer))))))

(defun riviera-session-redirect-buffer-name (session type)
  "Select buffer name for a redirection type."
  (let ((redirect (riviera-session-redirect session)))
    (when (or (and (eq type :stdout)
                   (riviera-redirect-stdout redirect))
              (and (eq type :stderr)
                   (riviera-redirect-stderr redirect)))
      (riviera-session-buffer-name session
                                   (cond
                                    ((riviera-redirect-combine redirect)
                                     riviera-redirect-combine-buffer-name)
                                    ((eq :stdout type)
                                     riviera-redirect-stdout-buffer-name)
                                    (t
                                     riviera-redirect-stderr-buffer-name))))))

(defun riviera-session-redirect-buffer-existp (session)
  "Check whether any redirection buffer exists."
  (let (name)
    (or (and (setq name (riviera-session-redirect-buffer-name session :stdout))
             (get-buffer name))
        (and (setq name (riviera-session-redirect-buffer-name session :stderr))
             (get-buffer name)))))

(defun riviera-dbgp-redirect-init (session)
  "Initialize redirection related variables."
  (let ((stdout (riviera-redirect-stdout (riviera-session-redirect session)))
        (stderr (riviera-redirect-stderr (riviera-session-redirect session))))
    (when stdout
      (riviera-dbgp-command-stdout session stdout))
    (when stderr
      (riviera-dbgp-command-stderr session stderr))))

(defun riviera-dbgp-handle-stream (session msg)
  "Handle a stream message."
  (let ((type (case (intern-soft (xml-get-attribute msg 'type))
                ('stdout :stdout)
                ('stderr :stderr)))
        (encoding (xml-get-attribute msg 'encoding))
        (content (car (last msg))))
    (riviera-dbgp-redirect-stream session type encoding content)))

(defun riviera-dbgp-redirect-stream (session type encoding content)
  "Print redirected string to specific buffers."
  (let ((buf (riviera-session-redirect-buffer session type))
        save-pos)
    (when buf
      (with-current-buffer buf
        (setq save-pos (unless (eobp) (point)))
        (save-excursion
          (goto-char (point-max))
          (insert (decode-coding-string
                   (if (string= "base64" encoding)
                       (base64-decode-string content)
                     content)
                   (riviera-redirect-coding-system (riviera-session-redirect session)))))
        (goto-char (or save-pos
                       (point-max))))
      (riviera-dbgp-display-window buf))))

(defun riviera-dbgp-command-stdout (session mode)
  "Send `stdout' command."
  (let ((m (plist-get '(nil 0 :disable 0 :redirect 1 :intercept 2) mode)))
    (when (and m)
      (riviera-dbgp-send-command session "stdout" (cons "-c" m)))))

(defun riviera-dbgp-response-stdout (session cmd msg)
  "A response message handler for `stdout' command."
  (setf (riviera-redirect-stdout (riviera-session-redirect session))
        (case (riviera-cmd-param-get cmd "-c")
          (0 nil)
          (1 :redirect)
          (2 :intercept))))

(defun riviera-dbgp-command-stderr (session mode)
  "Send `stderr' command."
  (let ((m (plist-get '(nil 0 :disable 0 :redirect 1 :intercept 2) mode)))
    (when (and m)
      (riviera-dbgp-send-command session "stderr" (cons "-c" m)))))

(defun riviera-dbgp-response-stderr (session cmd msg)
  "A response message handler for `stderr' command."
  (setf (riviera-redirect-stderr (riviera-session-redirect session))
        (case (riviera-cmd-param-get cmd "-c")
          (0 nil)
          (1 :redirect)
          (2 :intercept))))


;;==============================================================
;; DBGp starter
;;==============================================================

(defun riviera-dbgp-start (port)
  "Create DBGp listeners at each CONNECTION-POINTS."
  (condition-case error-sexp
      (let* ((result (dbgp-exec port
                                :session-accept 'riviera-dbgp-session-accept-p
                                :session-init 'riviera-dbgp-session-init
                                :session-filter 'riviera-dbgp-session-filter
                                :session-sentinel 'riviera-dbgp-session-sentinel))
             (listener (and (consp result)
                            (car result))))
        (when (processp listener)
          (message "Waiting for debug server to connect at port %s." port)))
    (error
     (beep)
     (read-char (format "[port %s] %s" port (second error-sexp))
                nil 3))))

(defun riviera-dbgp-start-proxy (ip-or-addr port idekey ;;multi-session-p
                                            session-port)
  "Create DBGp listeners at each CONNECTION-POINTS."
  (condition-case error-sexp
      (let* ((result
              (dbgp-proxy-register-exec ip-or-addr port idekey nil ;; multi-session-p
                                        session-port
                                        :session-accept 'riviera-dbgp-session-accept-p
                                        :session-init 'riviera-dbgp-session-init
                                        :session-filter 'riviera-dbgp-session-filter
                                        :session-sentinel 'riviera-dbgp-session-sentinel))
             (listener (and (consp result)
                            (car result))))
        (when (processp listener)
          (message "Waiting for debug server to connect.")))
    (error
     (beep)
     (read-char (format "[proxy %s:%s-%s] %s"
                        ip-or-addr port idekey (second error-sexp))
                nil 3))))

(defun riviera-dbgp-session-accept-p (proc)
  "Judge whether the SESSION is to be processed or to be terminated."
  ;; accept the new session if:
  ;;  a. capable for multi sessions.
  ;;  b. not used yet; it's the first session for the connection-point.
  (let ((accept-p
         (if (dbgp-proxy-p proc)
             (let ((proxy (dbgp-plist-get proc :proxy)))
               (or (plist-get proxy :multi-session)
                   (not (some (lambda (session)
                                (eq proxy (dbgp-plist-get proc :proxy)))
                              riviera-sessions))))
           (let ((port (dbgp-port-get (dbgp-listener-get proc))))
             (not (some (lambda (session)
                          (let ((oproc (riviera-session-process session)))
                            (and oproc
                                 (not (dbgp-proxy-p oproc))
                                 (eq port (dbgp-port-get (dbgp-listener-get oproc))))))
                        riviera-sessions))))))
    (unless accept-p
      (message "RIVIERA: Rejected new connection from %s (Already in debugging)"
               (car (process-contact proc))))
    accept-p))

(defun riviera-dbgp-session-init (proc)
  "Initialize SESSION environment."
  (let ((session (riviera-session-make :process proc)))
    (push session riviera-sessions)
    (dbgp-plist-put proc :session session)
    (with-current-buffer (process-buffer proc)
      (set (make-local-variable 'riviera-current-session) session)
      (rename-buffer (riviera-session-buffer-name session riviera-process-buffer-name) t))))

(defun riviera-dbgp-session-filter (proc string)
  "Process DBGp response STRING.
Parse STRING, find xml chunks, convert them to xmlized lisp objects
and call `riviera-dbgp-entry' with each chunk."
  (let ((session (dbgp-plist-get proc :session))
        xml output)
    (with-temp-buffer
      (insert string)
      (setq output
            (or (ignore-errors
                  (setq xml (xml-parse-region (point-min) (point-max)))
                  (goto-char (point-min))
                  (when (re-search-forward "\\?>" nil t)
                    (delete-region (match-end 0) (point-max))
                    (insert "\n")
                    (xml-print xml)
                    (propertize (buffer-string)
                                'front-sticky t
                                'font-lock-face 'dbgp-response-face)))
                string)))
    (when xml
      (condition-case error-sexp
          (riviera-dbgp-entry session (car xml))
        (error
         (warn "RIVIERA internal error: %S" error-sexp))))
    output))

(defun riviera-dbgp-session-sentinel (proc string)
  (when (buffer-live-p (process-buffer proc))
    (dbgp-session-echo-input proc "\nDisconnected.\n\n"))
  (let ((session (dbgp-plist-get proc :session)))
    (when session
      (ignore-errors
        (riviera-session-release session))
      (accept-process-output)
      (setq riviera-sessions (remq session riviera-sessions)))))

(add-hook 'kill-emacs-hook (lambda ()
                             (dolist (session riviera-sessions)
                               (ignore-errors
                                 (riviera-session-release session)))))


;;==============================================================
;; DBGp connected session initialization
;;==============================================================

(defun riviera-dbgp-init-fetch-entry-source (session)
  "Fetch the content of the entry source file."
  (let ((fileuri (xml-get-attribute-or-nil (riviera-session-initmsg session) 'fileuri)))
    (when fileuri
      (riviera-dbgp-command-source session fileuri))))

(defun riviera-dbgp-first-continuous-command (session)
  ""
  (riviera-dbgp-sequence
      (riviera-dbgp-send-command session "status")
    (lambda (session cmd msg err)
      (unless err
        (if (not riviera-pause-at-entry-line)
            (riviera-dbgp-command-run session)
          (if (and (equal "break" (xml-get-attribute msg 'status))
                   (not (member (riviera-session-language session) '(:perl))))
              ;; it is nonconforming to DBGp specs; anyway manage it.
              (run-hook-with-args 'riviera-dbgp-continuous-command-hook session)
            (riviera-dbgp-command-step-into session)))))))

;; features

(defcustom riviera-dbgp-feature-list
  '((:set max_data 32768)
    (:set max_depth 1)
    (:set max_children 32)
    (:get breakpoint_types riviera-dbgp-breakpoint-store-types))
  "*Specifies set of feature variables for each new debugging session.
Each entry forms a list (METHOD FEATURE_NAME VALUE_OR_CALLBACK).
METHOD is either `:get' or `:set'.
FEATURE_NAME is a feature name described in DBGp specification.
VALUE_OR_CALLBACK is, if the METHOD is `:get' then it should
be symbol of a callback function will be invoked 3 arguments
\(CMD MSG ERR), which are results of feature_get DBGp command.
If the method is `:set' VALUE_OR_CALLBACK can be either a value
or a symbol of a function. In the latter case the result value
of the function is passed to feature_set DBGp command."
  :group 'riviera
  :type '(repeat (list (radio (const :get)
                              (const :set))
                       (radio (const :help-echo ":get" :tag "language_supports_threads (:get)" language_supports_threads)
                              (const :tag "language_name (:get)" language_name)
                              (const :tag "encoding (:get)" encoding)
                              (const :tag "protocol_version (:get)" protocol_version)
                              (const :tag "supports_async (:get)" supports_async)
                              (const :tag "data_encoding (:get)" data_encoding)
                              (const :tag "breakpoint_languages (:get)" breakpoint_languages)
                              (const :tag "breakpoint_types (:get)" breakpoint_types)
                              (const :tag "multiple_sessions (:get :set)" multiple_sessions)
                              (const :tag "encoding (:get :set)" encoding)
                              (const :tag "max_children (:get :set)" max_children)
                              (const :tag "max_data (:get :set)" max_data)
                              (const :tag "max_depth (:get :set)" max_depth)
                              (const :tag "supports_postmortem (:get)" supports_postmortem)
                              (const :tag "show_hidden (:get :set)" show_hidden)
                              (const :tag "notify_ok (:get :set)" notify_ok))
                       sexp)))

(defun riviera-dbgp-feature-init (session)
  "Configure debugger engine with value of `riviera-dbgp-feature-list'."
  (let ((features (or (riviera-session-feature session)
                      riviera-dbgp-feature-list)))
    (dolist (entry features)
      (let ((method (car entry))
            (name (symbol-name (nth 1 entry)))
            (param (nth 2 entry)))
        (case method
          (:set
           (let ((value (cond
                         ((null param) nil)
                         ((symbolp param)
                          (if (fboundp param)
                              (funcall param)
                            (if (boundp param)
                                (symbol-value param)
                              (symbol-name param))))
                         (t param))))
             (riviera-dbgp-command-feature-set session name value)))
          (:get
           (condition-case error-sexp
               (if (and (symbolp param)
                        (fboundp param))
                   (riviera-dbgp-sequence
                       (riviera-dbgp-command-feature-get session name)
                     param))
             (error
              (warn "`riviera-dbgp-feature-alist' has invalid entry: %S" entry)))))))))

;; feature

(defun riviera-dbgp-command-feature-get (session feature)
  "Send \`feature_get\' command."
  (riviera-dbgp-send-command session "feature_get" (cons "-n" feature)))

(defun riviera-dbgp-command-feature-set (session feature value)
  "Send \`feature_get\' command."
  (riviera-dbgp-send-command session "feature_set"
                             (cons "-n" feature)
                             (cons "-v" (format "%S" (eval value)))))

                                        ;(add-hook 'riviera-dbgp-init-hook #'riviera-dbgp-init-fetch-entry-source t)
(add-hook 'riviera-dbgp-init-hook #'riviera-dbgp-feature-init t)
(add-hook 'riviera-dbgp-init-hook #'riviera-dbgp-redirect-init t)
(add-hook 'riviera-dbgp-init-hook #'riviera-dbgp-command-context-names t)
(add-hook 'riviera-dbgp-init-hook #'riviera-dbgp-breakpoint-restore t)
(add-hook 'riviera-dbgp-init-hook #'riviera-dbgp-first-continuous-command t)

(add-hook 'riviera-dbgp-continuous-command-hook #'riviera-dbgp-stack-update)
(add-hook 'riviera-dbgp-continuous-command-hook #'riviera-dbgp-breakpoint-list-refresh)
(add-hook 'riviera-dbgp-stack-update-hook #'riviera-context-list-refresh)


;;==============================================================
;;  riviera-mode
;;==============================================================

(defcustom riviera-query-on-clear-breakpoints t
  "*Specify if query is needed before removing all breakpoints.
If non-nil, RIVIERA will query the user before removing all breakpoints."
  :group 'riviera
  :type 'boolean)

(defvar riviera-mode-map nil)
(unless riviera-mode-map
  (setq riviera-mode-map (make-sparse-keymap "riviera"))
  ;; control
  (define-key riviera-mode-map " " 'riviera-step-again)
  (define-key riviera-mode-map "g" 'riviera-run)
  ;;(define-key riviera-mode-map "G" 'riviera-Go-nonstop-mode)
  (define-key riviera-mode-map ">" 'riviera-set-redirect)
  ;;(define-key riviera-mode-map "T" 'riviera-Trace-fast-mode)
  (define-key riviera-mode-map "c" 'riviera-run-to-cursor)
  ;;(define-key riviera-mode-map "C" 'riviera-Continue-fast-mode)

  ;;(define-key riviera-mode-map "f" 'riviera-forward) not implemented
  ;;(define-key riviera-mode-map "f" 'riviera-forward-sexp)
  ;;(define-key riviera-mode-map "h" 'riviera-goto-here)

  ;;(define-key riviera-mode-map "I" 'riviera-instrument-callee)
  (define-key riviera-mode-map "i" 'riviera-step-into)
  (define-key riviera-mode-map "o" 'riviera-step-over)
  (define-key riviera-mode-map "r" 'riviera-step-out)

  ;; quitting and stopping
  (define-key riviera-mode-map "q" 'riviera-stop)
  ;;(define-key riviera-mode-map "Q" 'riviera-top-level-nonstop)
  ;;(define-key riviera-mode-map "a" 'abort-recursive-edit)
  (define-key riviera-mode-map "v" 'riviera-display-context)

  ;; breakpoints
  (define-key riviera-mode-map "b" 'riviera-set-breakpoint-line)
  (define-key riviera-mode-map "B" 'riviera-breakpoint-menu)
  (define-key riviera-mode-map "u" 'riviera-unset-breakpoint-line)
  (define-key riviera-mode-map "U" 'riviera-clear-breakpoints)
  (define-key riviera-mode-map "\C-cb" 'riviera-show-breakpoint-list)
  ;;(define-key riviera-mode-map "B" 'riviera-next-breakpoint)
  ;;(define-key riviera-mode-map "x" 'riviera-set-conditional-breakpoint)
  ;;(define-key riviera-mode-map "X" 'riviera-set-global-break-condition)

  ;; evaluation
  (define-key riviera-mode-map "e" 'riviera-eval-expression)
  ;;(define-key riviera-mode-map "E" 'riviera-eval-current-word)
  ;;(define-key riviera-mode-map "\C-x\C-e" 'riviera-eval-last-sexp)

  ;; views
  (define-key riviera-mode-map "w" 'riviera-where)
  ;;(define-key riviera-mode-map "v" 'riviera-view-outside) ;; maybe obsolete??
  ;;(define-key riviera-mode-map "p" 'riviera-bounce-point)
  ;;(define-key riviera-mode-map "P" 'riviera-view-outside) ;; same as v
  ;;(define-key riviera-mode-map "W" 'riviera-toggle-save-windows)

  ;; misc
  (define-key riviera-mode-map "?" 'riviera-mode-help)
  (define-key riviera-mode-map "d" 'riviera-show-backtrace)
  (define-key riviera-mode-map "t" 'riviera-show-backtrace)
  (define-key riviera-mode-map "\C-cp" 'riviera-toggle-pause-at-entry-line-flag)
  (define-key riviera-mode-map "\C-cf" 'riviera-find-file)

  ;;(define-key riviera-mode-map "-" 'negative-argument)

  ;; statistics
  ;;(define-key riviera-mode-map "=" 'riviera-temp-display-freq-count)

  ;; GUD bindings
  (define-key riviera-mode-map "\C-c\C-s" 'riviera-step-into)
  (define-key riviera-mode-map "\C-c\C-n" 'riviera-step-over)
  (define-key riviera-mode-map "\C-c\C-c" 'riviera-run)

  (define-key riviera-mode-map "\C-x " 'riviera-set-breakpoint-line)
  (define-key riviera-mode-map "\C-c\C-d" 'riviera-unset-breakpoint-line)
  (define-key riviera-mode-map "\C-c\C-t" 'riviera-set-breakpoint-line)
  (define-key riviera-mode-map "\C-c\C-l" 'riviera-where))

;;;###autoload
(define-minor-mode riviera-mode
  "Minor mode for debugging source code with RIVIERA.
The riviera-mode buffer commands:
\\{riviera-mode-map}"
  nil " *debugging*" riviera-mode-map
  (setq buffer-read-only riviera-mode)
  (setq left-margin-width (if riviera-mode 2 0))
  ;; when the buffer is visible in a window,
  ;; force the window to notice the margin modification
  (set (make-local-variable 'command-error-function) #'riviera-mode-read-only-handler)
  (let ((win (get-buffer-window (current-buffer))))
    (if win
        (set-window-buffer win (current-buffer)))))

(add-hook 'riviera-source-visit-hook 'riviera-enter-riviera-mode)

(defun riviera-mode-read-only-handler (data context caller)
  (if (eq 'buffer-read-only (car data))
      (riviera-with-current-session session
        (let ((prompt "The buffer is under debug mode. Want to open the original file? (y/N): "))
          (if (memq (read-char prompt) '(?Y ?y))
              (riviera-session-source-visit-original-file
               session
               (riviera-session-source-fileuri session (buffer-file-name))))))
    (message (error-message-string data))
    (beep)))

(defun riviera-enter-riviera-mode (session buf)
  (with-current-buffer buf
    (riviera-mode 1)
    (set (make-local-variable 'riviera-current-session) session)))

(add-hook 'riviera-source-release-hook
          (lambda () (riviera-mode 0)))

(defun riviera-where ()
  "Move to the current breaking point."
  (interactive)
  (riviera-with-current-session session
    (if (riviera-session-stack session)
        (let* ((stack (second (car (riviera-session-stack session))))
               (fileuri (riviera-source-fileuri-regularize (cdr (assq 'filename stack))))
               (lineno (cdr (assq 'lineno stack))))
          (riviera-session-cursor-update session fileuri lineno))
      (when (interactive-p)
        (message "RIVIERA is not started.")))))

(defun riviera-quit-window ()
  (interactive)
  (quit-window)
  (riviera-where))

(defun riviera-mode-help ()
  "Display description and key bindings of `riviera-mode'."
  (interactive)
  (describe-function 'riviera-mode))

(defvar riviera-step-type :step-into
  "Step command of what `riviera-step-again' acts.
This value remains the last step command type either
`:step-into' or `:step-out'.")

(defun riviera-step-again ()
  "Do either `riviera-step-into' or `riviera-step-over' what the last time called.
Default is `riviera-step-into'."
  (interactive)
  (case riviera-step-type
    (:step-over (riviera-step-over))
    (:step-into (riviera-step-into))
    (t (riviera-step-into))))

(defun riviera-step-into ()
  "Step into the definition of the function or method about to be called.
If there is a function call involved it will break on the first
statement in that function"
  (interactive)
  (setq riviera-step-type :step-into)
  (riviera-with-current-session session
    (riviera-dbgp-command-step-into session)))

(defun riviera-step-over ()
  "Step over the definition of the function or method about to be called.
If there is a function call on the line from which the command
is issued then the debugger engine will stop at the statement
after the function call in the same scope as from where the
command was issued"
  (interactive)
  (setq riviera-step-type :step-over)
  (riviera-with-current-session session
    (riviera-dbgp-command-step-over session)))

(defun riviera-step-out ()
  "Step out of the current scope.
It breaks on the statement after returning from the current
function."
  (interactive)
  (riviera-with-current-session session
    (riviera-dbgp-command-step-out session)))

(defun riviera-run ()
  "Start or resumes the script.
It will break at next breakpoint, or stops at the end of the script."
  (interactive)
  (riviera-with-current-session session
    (riviera-dbgp-command-run session)))

(defun riviera-run-to-cursor ()
  "Run the script to where the cursor points."
  (interactive)
  (riviera-with-current-session session
    (riviera-dbgp-sequence
        (riviera-set-breakpoint-line nil nil nil t)
      (lambda (session cmd msg err)
        (let ((bid (xml-get-attribute-or-nil msg 'id)))
          (riviera-dbgp-sequence-bind (bid)
            (riviera-run)
            (lambda (session cmd msg err)
              (riviera-dbgp-command-breakpoint-remove session bid))))))))

(defun riviera-stop ()
  "End execution of the script immediately."
  (interactive)
  (riviera-with-current-session session
    (riviera-dbgp-command-stop session)))

(defun riviera-breakpoint-menu (arg)
  "Set a breakpoint interactively.
Script debugger engine may support a kind of breakpoints, which
will be stored in the variable `riviera-dbgp-breakpoint-types'
after a debugging session is started.

This command asks you a breakpoint type and its options.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<riviera-mode-map>\\[riviera-breakpoint-menu] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[riviera-breakpoint-menu]), \
this command will also ask a
hit-value interactively.
"
  (interactive "P")
  (riviera-with-current-session session
    (let ((candidates (remove nil
                              (mapcar
                               (lambda (x)
                                 (if (member (car x)
                                             (riviera-breakpoint-types (riviera-session-breakpoint session)))
                                     x))
                               '((:line . "l)Line")
                                 (:call . "c)Call")
                                 (:return . "r)Return")
                                 (:exception . "e)Exception")
                                 (:conditional . "d)Conditional")
                                 (:watch . "w)Watch"))))))
      (when (null candidates)
        (error "No breakpoint type is supported by the debugger engine."))
      (let* ((c (read-char (concat "Breakpoint type: "
                                   (mapconcat
                                    (lambda (x)
                                      (cdr x))
                                    candidates " "))))
             (x (find-if (lambda (x)
                           (eq c (elt (cdr x) 0)))
                         candidates))
             (fn (and x
                      (intern-soft (concat "riviera-set-breakpoint-"
                                           (substring (symbol-name (car x)) 1))))))
        (unless x
          (error "Cancelled"))
        (if (fboundp fn)
            (call-interactively fn)
          (error (concat (symbol-name fn) " is not implemented.")))))))

(defun riviera-set-breakpoint-common (session hit-value bp)
  (setq hit-value (if (and (not (null hit-value))
                           (listp hit-value))
                      (if (fboundp 'read-number)
                          (read-number "Number of hit to break: ")
                        (string-to-number
                         (read-string "Number of hit to break: ")))
                    hit-value))
  (plist-put bp :hit-value (if (and (numberp hit-value)
                                    (<= 0 hit-value))
                               hit-value
                             0))
  (riviera-dbgp-command-breakpoint-set session bp))

(defun riviera-set-breakpoint-line (fileuri lineno &optional hit-value temporary-p)
  "Set a breakpoint at the current line.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<riviera-mode-map>\\[riviera-set-breakpoint-line] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[riviera-set-breakpoint-line]), \
this command will also ask a
hit-value interactively."
  (interactive (list nil nil current-prefix-arg nil))
  (riviera-with-current-session session
    (let ((local-path (if fileuri
                          (riviera-session-source-local-path session fileuri)
                        (buffer-file-name (current-buffer)))))
      (riviera-set-breakpoint-common session hit-value
                                     (riviera-bp-make
                                      session :line
                                      :fileuri (or fileuri
                                                   (riviera-session-source-fileuri session local-path)
                                                   (riviera-session-source-fileuri session (file-truename local-path))
                                                   (riviera-source-fileuri session local-path))
                                      :lineno (if (numberp lineno)
                                                  lineno
                                                (riviera-what-line))
                                      :local-path local-path
                                      :overlay t
                                      :run-once temporary-p)))))

(defvar riviera-set-breakpoint-call-history nil)
(defvar riviera-set-breakpoint-fileuri-history nil)
(defvar riviera-set-breakpoint-exception-history nil)
(defvar riviera-set-breakpoint-condition-history nil)

(defun riviera-set-breakpoint-call (name &optional fileuri hit-value)
  "Set a breakpoint to break at when entering function/method named NAME.
For a class method, specify NAME like \"MyClass::MyMethod\".
For an instance method, do either like \"MyClass::MyMethod\" or
\"MyClass->MyMethod\".
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<riviera-mode-map>\\[riviera-set-breakpoint-call] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[riviera-set-breakpoint-call]),
this command will also ask a
hit-value interactively."
  (interactive (list nil))
  (riviera-with-current-session session
    (when (interactive-p)
      (setq name (read-string "Name: " ""
                              'riviera-set-breakpoint-call-history))
      (setq fileuri
            (unless (member (riviera-session-language session) '(:php :ruby))
              ;; at this present some debugger engines' implementations is buggy:
              ;; some requires fileuri and some don't accept it.
              (let ((local-path (file-truename (buffer-file-name (current-buffer)))))
                (read-string "fileuri: "
                             (or (riviera-session-source-fileuri session local-path)
                                 (riviera-source-fileuri session local-path))
                             'riviera-set-breakpoint-fileuri-history))))
      (setq hit-value current-prefix-arg))
    (when (string< "" name)
      (riviera-set-breakpoint-common session hit-value
                                     (riviera-bp-make session :call
                                                      :function name
                                                      :fileuri fileuri)))))

(defun riviera-set-breakpoint-return (name &optional fileuri hit-value)
  "Set a breakpoint to break after returned from a function/method named NAME.
For a class method, specify NAME like \"MyClass::MyMethod\".
For an instance method, do either like \"MyClass::MyMethod\" or
\"MyClass->MyMethod\".
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<riviera-mode-map>\\[riviera-set-breakpoint-return] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[riviera-set-breakpoint-return]),
this command will also ask a
hit-value interactively."
  (interactive (list nil))
  (riviera-with-current-session session
    (when (interactive-p)
      (setq name (read-string "Name: " ""
                              'riviera-set-breakpoint-call-history))
      (setq fileuri
            (unless (member (riviera-session-language session) '(:php :ruby))
              ;; at this present some debugger engines' implementations are buggy:
              ;; some requires fileuri and some don't accept it.
              (let ((local-path (file-truename (buffer-file-name (current-buffer)))))
                (read-string "fileuri: "
                             (or (riviera-session-source-fileuri session local-path)
                                 (riviera-source-fileuri session local-path))
                             'riviera-set-breakpoint-fileuri-history))))
      (setq hit-value current-prefix-arg))
    (when (string< "" name)
      (riviera-set-breakpoint-common session hit-value
                                     (riviera-bp-make session :return
                                                      :function name
                                                      :fileuri fileuri)))))

(defun riviera-set-breakpoint-exception (name &optional hit-value)
  "Set a breakpoint to break at when an exception named NAME is occurred.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<riviera-mode-map>\\[riviera-set-breakpoint-exception] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[riviera-set-breakpoint-exception]),
this command will also ask a
hit-value interactively."
  (interactive (list
                (read-string "Exception type: "
                             "Exception"
                             'riviera-set-breakpoint-exception-history)
                current-prefix-arg))
  (riviera-with-current-session session
    (riviera-set-breakpoint-common session hit-value
                                   (riviera-bp-make session :exception
                                                    :exception name))))

(defun riviera-set-breakpoint-conditional (expr fileuri &optional lineno hit-value)
  "Set a breakpoint to break at when the expression EXPR is true in the file FILEURI.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<riviera-mode-map>\\[riviera-set-breakpoint-conditional] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[riviera-set-breakpoint-conditional]),
this command will also ask a
hit-value interactively."
  (interactive (list nil nil))
  (riviera-with-current-session session
    (when (interactive-p)
      (setq expr (read-string "Expression: " ""
                              'riviera-set-breakpoint-condition-history))
      (setq fileuri
            (let ((local-path (file-truename (buffer-file-name (current-buffer)))))
              (or (riviera-session-source-fileuri session local-path)
                  (riviera-source-fileuri session local-path))))
      (setq lineno (read-string "Line number to evaluate (blank means entire file): "
                                (number-to-string (riviera-what-line))))
      (setq hit-value current-prefix-arg))

    (riviera-set-breakpoint-common session hit-value
                                   (riviera-bp-make session :conditional
                                                    :expression expr
                                                    :fileuri fileuri
                                                    :lineno (and (stringp lineno)
                                                                 (string-match "^[0-9]+$" lineno)
                                                                 (string-to-number lineno))))))

(defun riviera-set-breakpoint-watch (expr &optional hit-value)
  "Set a breakpoint to break on write of the variable or address.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<riviera-mode-map>\\[riviera-set-breakpoint-conditional] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[riviera-set-breakpoint-conditional]),
this command will also ask a
hit-value interactively."
  (interactive (list nil))
  (riviera-with-current-session session
    (when (interactive-p)
      (setq expr (read-string "Expression: " ""
                              'riviera-set-breakpoint-condition-history))
      (setq hit-value current-prefix-arg))
    (riviera-set-breakpoint-common session hit-value
                                   (riviera-bp-make session :watch
                                                    :expression expr))))

(defun riviera-unset-breakpoint-line ()
  "Clear a breakpoint set at the current line."
  (interactive)
  (riviera-with-current-session session
    (mapc (lambda (bp)
            (riviera-dbgp-command-breakpoint-remove session (plist-get bp :id)))
          (riviera-breakpoint-find-at-pos session (current-buffer) (point)))))

(defun riviera-clear-breakpoints ()
  "Clear all breakpoints.
If `riviera-query-on-clear-breakpoints' is non-nil, RIVIERA will query the user before
removing all breakpoints."
  (interactive)
  (riviera-with-current-session session
    (when (or (not riviera-query-on-clear-breakpoints)
              (let ((prompt "Clear all breakpoints? (y/N): "))
                (memq (read-char prompt) '(?Y ?y))))
      (riviera-breakpoint-clear session))))

(defun riviera-show-breakpoint-list ()
  "Display breakpoint list.
The breakpoint list buffer is under `riviera-breakpoint-list-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (riviera-breakpoint-list-refresh t))

(defvar riviera-eval-history nil)

(defun riviera-eval-expression (expr)
  "Evaluate a given string EXPR within the current execution context."
  (interactive
   (progn
     (list (read-from-minibuffer "Eval: "
                                 nil nil nil 'riviera-eval-history))))
  (riviera-with-current-session session
    (riviera-dbgp-command-eval session expr)))

(defun riviera-eval-current-word ()
  "Evaluate a word at where the cursor is pointing."
  (interactive)
  (let ((expr (current-word)))
    (when expr
      (riviera-with-current-session session
        (riviera-dbgp-command-eval session expr)))))

(defun riviera-open-file (fileuri)
  "Open a debugger server side file specified by FILEURI.
FILEURI forms like as \`file:///path/to/file\'."
  (interactive (list (read-string "Open file: " "file://")))
  (riviera-with-current-session session
    (riviera-dbgp-command-source session fileuri)))

(defun riviera-show-backtrace ()
  "Display backtrace list.
The backtrace list buffer is under `riviera-backtrace-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (riviera-with-current-session session
    (riviera-backtrace session)))

(defun riviera-toggle-pause-at-entry-line-flag ()
  "Toggle `riviera-pause-at-entry-line'."
  (interactive)
  (setq riviera-pause-at-entry-line
        (not riviera-pause-at-entry-line))
  (if (interactive-p)
      (message (format "`riviera-pause-at-entry-line' is %s" riviera-pause-at-entry-line))))

(defun riviera-set-redirect (target &optional arg)
  "Set the debuggee script's output redirection mode.
This command enables you to redirect the debuggee script's output to RIVIERA.
You can select redirection target from \`stdout', \`stderr' and both of them.
Prefixed with \\[universal-argument], you can also select redirection mode
from \`redirect', \`intercept' and \`disabled'."
  (interactive (list (case (read-char "Redirect: o)STDOUT e)STRERR b)Both")
                       (?o :stdout)
                       (?e :stderr)
                       (?b :both))
                     current-prefix-arg))
  (unless target
    (error "Cancelled"))
  (let ((mode (if arg
                  (case (read-char "Mode: r)Redirect i)Intercept d)Disable")
                    (?r :redirect)
                    (?i :intercept)
                    (?d :disable))
                :redirect)))
    (unless mode
      (error "Cancelled"))
    (riviera-with-current-session session
      (when (memq target '(:stdout :both))
        (riviera-dbgp-command-stdout session mode))
      (when (memq target '(:stderr :both))
        (riviera-dbgp-command-stderr session mode)))))

(defun riviera-display-context (&optional depth)
  (interactive (list (cond
                      ((null current-prefix-arg) 0)
                      ((numberp current-prefix-arg)
                       current-prefix-arg)
                      ((listp current-prefix-arg)
                       (if (fboundp 'read-number)
                           (read-number "Depth: " 0)
                         (string-to-number (read-string "Depth: " "0"))))
                      (t nil))))
  (riviera-with-current-session session
    (riviera-context-list-display session (or depth 0))))

(defun riviera-find-file ()
  (interactive)
  (riviera-with-current-session session
    (let ((file-path (riviera-session-source-read-file-name
                      session
                      (file-name-directory (riviera-source-fileuri session
                                                                   (buffer-file-name)))
                      t)))
      (when file-path
        (riviera-open-file (riviera-source-fileuri session file-path))))))


(defcustom riviera-dbgp-default-port 9000
  "Default port number to listen a new DBGp connection."
  :group 'riviera
  :type 'integer)

(defcustom riviera-dbgp-default-proxy '("127.0.0.1" 9001 "default" nil t)
  "Default setting for a new DBGp proxy connection.

The first and second elements are address and port where the DBGp proxy listening on.
The third element is IDE key.
The forth element is a flag but currently not used yet.
The fifth element is port to be used in debugging sessions. If a non-integer value is
set, then any free port will be allocated.
"
  :group 'riviera)

;;;###autoload
(defun riviera (&optional args)
  "Start RIVIERA, a DBGp protocol frontend - a script debugger.
Variations are described below.

By default, starts RIVIERA listening to port `riviera-dbgp-default-port'.
Prefixed with one \\[universal-argument], asks listening port number interactively and
starts RIVIERA on the port.
Prefixed with two \\[universal-argument]'s, starts a RIVIERA proxy listener.
Prefixed with three \\[universal-argument]'s, kills a RIVIERA listener.
Prefixed with four \\[universal-argument]'s, kills a RIVIERA proxy listener.

RIVIERA communicates with script servers, located anywhere local or
remote, in DBGp protocol (e.g. PHP with Xdebug extension)
to help you debugging your script with some valuable features:
 - continuation commands like \`step in\', \`step out\', ...
 - a kind of breakpoints like \`line no\', \`function call\' and
   \`function return\'.
 - evaluation
 - stack dump
 - etc.

The script servers should be DBGp protocol enabled.
Ask to your script server administrator about this setting up
issue.

Once you've done these setup operation correctly, run RIVIERA first
and your script on your script server second. After some
negotiation RIVIERA will display your script's entry source code.
The debugging session is started.

In the debugging session the source code buffers are under the
minor mode  `riviera-mode'. Key mapping and other information is
described its help page."
  (interactive "p")
  (case args
    (1
     (riviera-dbgp-start riviera-dbgp-default-port))
    (4
     (let ((default (or (car dbgp-listener-port-history)
                        riviera-dbgp-default-port
                        (default-value 'riviera-dbgp-default-port))))
       (riviera-dbgp-start (dbgp-read-integer (format "Listen port(default %s): " default)
                                              default 'dbgp-listener-port-history))))
    (16
     (call-interactively 'riviera-proxy))
    (64
     (call-interactively 'riviera-end))
    (t
     (call-interactively 'riviera-proxy-end))))

(defun riviera-end (port)
  "Stop the DBGp listener on PORT."
  (interactive
   (let ((ports (remq nil
                      (mapcar (lambda (listener)
                                (and (not (dbgp-proxy-p listener))
                                     (number-to-string (second (process-contact listener)))))
                              dbgp-listeners))))
     (list
      (if (= 1 (length ports))
          (string-to-number (car ports))
        ;; ask user for the target idekey.
        (let ((num (completing-read "Listener port to kill: " ports nil t)))
          (if (string< "" num)
              (read num)
            (signal 'quit nil)))))))
  (let ((listener (dbgp-listener-find port)))
    (dbgp-listener-kill port)
    (and (interactive-p)
         (message (if listener
                      "The DBGp listener for port %d is terminated."
                    "DBGp listener for port %d does not exist.")
                  port))
    (and listener t)))

(defun riviera-proxy (ip-or-addr port idekey ;;multi-session-p
                                 &optional session-port)
  "Start a new DBGp proxy listener.
The DBGp proxy should be found at IP-OR-ADDR / PORT.
This create a new DBGp listener and register it to the proxy
associating with the IDEKEY."
  (interactive (list
                (let ((default (or (car dbgp-proxy-address-history)
                                   (nth 0 riviera-dbgp-default-proxy)
                                   (nth 0 (default-value 'riviera-dbgp-default-proxy)))))
                  (dbgp-read-string (format "Proxy address (default %s): " default)
                                    nil 'dbgp-proxy-address-history default))
                (let ((default (or (car dbgp-proxy-port-history)
                                   (nth 1 riviera-dbgp-default-proxy)
                                   (nth 1 (default-value 'riviera-dbgp-default-proxy)))))
                  (dbgp-read-integer (format "Proxy port (default %d): " default)
                                     default 'dbgp-proxy-port-history))
                (let ((default (or (car dbgp-proxy-idekey-history)
                                   (nth 2 riviera-dbgp-default-proxy)
                                   (nth 2 (default-value 'riviera-dbgp-default-proxy)))))
                  (dbgp-read-string "IDE key: " nil 'dbgp-proxy-idekey-history))
                ;;(not (memq (read-char "Multi session(Y/n): ") '(?N ?n)))
                (let ((default (or (car dbgp-proxy-session-port-history)
                                   (nth 4 riviera-dbgp-default-proxy)
                                   (nth 4 (default-value 'riviera-dbgp-default-proxy)))))
                  (unless (numberp default)
                    (setq default 0))
                  (dbgp-read-integer (format "Port for debug session (%s): "
                                             (if (< 0 default)
                                                 (format "default %d, 0 to use any free port" default)
                                               (format "leave empty to use any free port")))
                                     default 'dbgp-proxy-session-port-history))))
  (riviera-dbgp-start-proxy ip-or-addr port idekey ;;multi-session-p
                            session-port))

(defalias 'riviera-proxy-end #'dbgp-proxy-unregister)

(provide 'riviera)
