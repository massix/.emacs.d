;; init-orange.el -- Inits some random functions I use at work

(require 'cl) ;; Common Lisp FTW.

(defcustom ke/naughty-notifier-active
  nil
  "Set to true if you want to use the naughty notifier"
  :group 'my/dotemacs
  :type 'boolean)

(defcustom ke/bear-path
  "~/dev/Bear/build/src/bear"
  "Set this to the location of your Bear binary"
  :group 'my/dotemacs)

(defcustom ke/default-prefix
  "\C-ck"
  "Default prefix for KE specific commands"
  :group 'my/dotemacs)

(defcustom ke/opinel-prefix
  "\C-co"
  "Default prefix for Opinel commands"
  :group 'my/dotemacs)

(defun ke/setup-path ()
  "Sets the KE's path"
  (setenv "PATH"
          (concat "/ke/local/toolchain3-x86_64-nptl/bin:"
                  "/ke/local/toolchain3-x86_64-nptl/tools/bin:"
                  "/ke/local/toolchain3-x86_64-nptl/tools/java/jdk1.6.0_13/bin:"
                  "/ke/local/toolchain3-x86_64-nptl/tools/java/ant-1.7.0/bin:"
                  "/ke/local/toolchain3-x86_64-nptl/bin:"
                  "/ke/local/toolchain3-x86_64-nptl/tools/bin:"
                  "/ke/local/toolchain3-x86_64-nptl/tools/java/jdk1.6.0_13/bin:"
                  "/ke/local/toolchain3-x86_64-nptl/tools/java/ant-1.7.0/bin:"
                  (getenv "PATH"))))

(defcustom ke/opinel-buffer-name "*Opinel Output*"
  "The buffer name used for output of Opinel commands"
  :group 'my/dotemacs)

(defvar ke/opinel-environment nil)
(defvar ke/opinel-filter nil)

(defmacro ke/with-compilation-path (&rest body)
  (let ((current-path (getenv "PATH")))
    (ke/setup-path)
    `(unwind-protect
         (progn ,@body)
       (setenv "PATH" ,current-path))))

(defun ke/notify (text)
  (interactive "sInsert the text of the notification: ")
  (when ke/naughty-notifier-active
    (let ((command (format
                    "echo \"naughty.notify({ text = \\\"%s\\\", timeout = 10 })\" | $(which awesome-client)" text)))
      (message command)
      (shell-command command))))

(defun ke/trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun ke/find-root-dir (path look-for-member &optional discarded)
  "Recursively finds for a directory in the given path"
  (let* ((split-path (split-string path "/"))
         (new-split-path (butlast split-path)))
    (cond
     ((string-equal "~" path) `(nil ,discarded))
     ((string-equal "/" path) `(nil ,discarded))
     ((null path) `(nil ,discarded))
     ((and (file-exists-p path)
           (member look-for-member (directory-files path))) `(,path ,discarded))
     (t (ke/find-root-dir
         (mapconcat 'identity new-split-path "/")
         look-for-member
         (concat (car (last split-path))
                 (when discarded "/")
                 discarded))))))

(defun ke/find-where-to-compile (path)
  "Finds the topmost .bzr and then it descends in the .release subfolder
   to look for a suitable Makefile to use"
  (let ((root-path (ke/find-root-dir path ".bzr")))
    (car (ke/find-root-dir
          (concat (car root-path) "/.release/" (car (cdr root-path)))
          "Makefile"))))

(setq compilation-finish-functions
      (lambda (arg0 arg1)
        (ke/notify (format "Compilation is over ['%s']" (ke/trim-string arg1)))))

(cl-defun ke/compile (argument &key (parallel-jobs 2) (silent t) (alternative-compiler nil) (pre-make nil))
  "KE Compilation System, calls 'compile without cluttering the default values"
  (interactive "sTarget: ")
  (ke/with-compilation-path
   (let* ((full-path (ke/find-where-to-compile (if (eq major-mode 'dired-mode)
                                                   (dired-current-directory)
                                                 (file-name-directory buffer-file-name))))
          (compile-command (concat pre-make " make -C \""
                                   full-path
                                   "\" "
                                   (when parallel-jobs (concat "-j" (number-to-string parallel-jobs)))
                                   " "
                                   (when silent "--silent")
                                   " "
                                   (when alternative-compiler (concat "CXX=\"" alternative-compiler "\""))
                                   " "
                                   argument)))
     (call-interactively 'compile))))

(defun ke/reset-opinel-environment (environment)
  "Allows to reset the ke/opinel-environment variable"
  (interactive "sEnvironment: " ke/opinel-environment)
  (setq ke/opinel-environment environment))

(cl-defun ke/run-opinel-command (command &optional ask-for-filter)
  "Run an opinel command using the stored ke/opinel-environment or
   asking for one if it is not set"
  (interactive "sCommand: \nP")
  (if ask-for-filter (setq ke/opinel-filter (read-from-minibuffer "Filter: " ke/opinel-filter)))
  (let* ((filter (if ask-for-filter ke/opinel-filter nil))
         (ke/project-root (concat (car (ke/find-root-dir (file-name-directory buffer-file-name) ".bzr")) "/../"))
         (ke/opinel-bin (concat ke/project-root "ke-opinel/src/opinel"))
         (buffer (get-buffer-create ke/opinel-buffer-name)))
    (if (null ke/opinel-environment)
        (setq ke/opinel-environment (read-from-minibuffer "Environment name: ")))
    (with-current-buffer buffer
      (async-shell-command (format "%s %s --env=%s %s"
                                   ke/opinel-bin
                                   (if ask-for-filter
                                       (format "-g role:%s" filter)
                                     "")
                                   ke/opinel-environment
                                   command) buffer)
      (read-only-mode))))

(defun ke/run-opinel-remote-command (command &optional ask-for-filter)
  "Convenient wrapper to run cmd -- command"
  (interactive "sCommand: \nP")
  (ke/run-opinel-command (format "cmd -- %s" command) ask-for-filter))

(defun ke/opinel-get-services-status ()
  (interactive)
  (setq ke/opinel-filter "")
  (ke/run-opinel-command "cmd -- /ke/scripts/ke status" nil))

;; Set the bear command
(defun ke/generate-bear-command ()
  "Return a compatible bear command for the buffer"
  (let* ((full-path (ke/find-where-to-compile (if (eq major-mode 'dired-mode)
                                                  (dired-current-directory)
                                                (file-name-directory buffer-file-name)))))
    (concat ke/bear-path " -o " full-path "/compile_commands.json -- ")))

(defmacro ke/create-compilation-function (argument)
  `(defun ,(intern (concat "ke/compile-" argument)) (&optional prefix)
     "KE Compilation System, calls 'compile without cluttering the default values"
     (interactive "P")
     (if prefix
         (ke/compile ,argument :pre-make (ke/generate-bear-command))
       (ke/compile ,argument))))

(defmacro ke/bind-key-to-dired-and-c-map (prefix key symbol)
  `(define-key dired-mode-map (concat ,prefix ,key) ,symbol)
  `(define-key c-mode-base-map (concat ,prefix ,key) ,symbol))

(ke/create-compilation-function "clean")
(ke/create-compilation-function "all")
(ke/create-compilation-function "check")
(ke/create-compilation-function "deb-main-deploy")

(require 'cc-mode)

;; Map keys

;; Compilation
(ke/bind-key-to-dired-and-c-map ke/default-prefix "k" 'ke/compile)
(ke/bind-key-to-dired-and-c-map ke/default-prefix "m" 'ke/compile-all)
(ke/bind-key-to-dired-and-c-map ke/default-prefix "c" 'ke/compile-clean)
(ke/bind-key-to-dired-and-c-map ke/default-prefix "t" 'ke/compile-check)
(ke/bind-key-to-dired-and-c-map ke/default-prefix "d" 'ke/compile-deb-main-deploy)

;; Opinel
(ke/bind-key-to-dired-and-c-map ke/opinel-prefix "o" 'ke/run-opinel-command)
(ke/bind-key-to-dired-and-c-map ke/opinel-prefix "r" 'ke/reset-opinel-environment)
(ke/bind-key-to-dired-and-c-map ke/opinel-prefix "s" 'ke/opinel-get-services-status)
(ke/bind-key-to-dired-and-c-map ke/opinel-prefix "x" 'ke/run-opinel-remote-command)

(provide 'init-orange)
