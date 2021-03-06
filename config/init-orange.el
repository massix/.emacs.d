;; init-orange.el -- Inits some random functions I use at work

(require 'cl) ;; Common Lisp FTW.

(defcustom ke/messages-notifier
  nil
  "Accepted values are 'naughty or 'stumpish"
  :group 'my/dotemacs)

(defcustom ke/bear-path
  "~/dev/Bear/build/src/bear"
  "Set this to the location of your Bear binary"
  :group 'my/dotemacs)

(defcustom ke/cc_args-path
  "~/.emacs.d/cc_args.py"
  "Set this to the path to the cc_args.py binary"
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
                  (getenv "PATH"))))

(defcustom ke/opinel-buffer-name "*Opinel Output*"
  "The buffer name used for output of Opinel commands"
  :group 'my/dotemacs)

(defvar ke/opinel-environment nil)
(defvar ke/opinel-filter nil)

(defmacro ke/with-compilation-path (&rest body)
  (let ((current-path (getenv "PATH")))
    `(unwind-protect
         (progn
           (ke/setup-path)
           ,@body
           (setenv "PATH" ,current-path)))))

(defun ke/notify (text)
  (interactive "sInsert the text of the notification: ")
  (cond
   ((eq ke/messages-notifier 'stumpish)
    (shell-command (format "stumpish echo \"%s\"" text)))
   ((eq ke/messages-notifier 'naughty)
    (shell-command (format "echo \"naughty.notify({ text = \\\"%s\\\", timeout = 10 })\" | $(which awesome-client)" text)))))

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
    (car (ke/find-root-dir (concat (car root-path) "/.release/" (car (cdr root-path))) "Makefile"))))

(setq compilation-finish-functions
      (lambda (arg0 arg1)
        (ke/notify (format "Compilation is over ['%s']" (ke/trim-string arg1)))))

(defun ke/pwd ()
  (cond
   ((eq major-mode 'dired-mode) (dired-current-directory))
   ((eq major-mode 'eshell-mode) (eshell/pwd))
   (t (file-name-directory buffer-file-name))))

(cl-defun ke/compile (argument &key (parallel-jobs 2) (silent t) (alternative-compiler nil) (pre-make nil))
  "KE Compilation System, calls 'compile without cluttering the default values"
  (interactive "sTarget: ")
  (ke/with-compilation-path
   (let* ((full-path (ke/find-where-to-compile (ke/pwd)))
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
  (let* ((full-path (ke/find-where-to-compile (ke/pwd))))
    (concat ke/bear-path " -o " full-path "compile_commands.json -- ")))

(defun ke/prepare-dev (repositories-list branch &optional branch-only)
  "Prepare a development branch with the specified arguments"
  (interactive "sRepositories: \nsBranch: ")
  (let* ((default-directory "/tmp/")
         (repositories (split-string repositories-list " "))
         (final-destination (concat default-directory branch "/"))
         (processes))
    (when (not (file-exists-p (concat default-directory branch)))
      (make-directory (concat default-directory branch))
      (dolist (repository repositories processes)
        (let* ((source      (concat "mel:" repository))
               (destination (concat final-destination repository))
               (pname       (concat "bzr-branch:" repository)))
          (setq processes (cons (start-process pname "*branching*" "bzr" "branch" source destination) processes))))
      (switch-to-buffer "*branching*")
      (dolist (process processes nil)
        (insert "Process " (process-name process) " running\n")))))

(defun ke/bootstrap-component (component branch)
  "Bootstrap a component"
  (interactive "sComponent: \nsBranch: ")
  (let* ((toolchain-directory "/ke/local/toolchain3-x86_64-nptl")
         (root-directory (expand-file-name (concat "~/dev/" branch "/" component "/")))
         (common-directory (expand-file-name "../ke-common" root-directory))
         (kemake-directory (expand-file-name "../ke-kemake" root-directory))
         (default-directory (expand-file-name (concat root-directory ".release/"))))
    (if (not (file-exists-p default-directory))
        (make-directory default-directory))
    (let ((command (concat (expand-file-name "../configure" default-directory) " "
                           "--with-toolchain-dir=" toolchain-directory " "
                           "--with-kemake-dir=" kemake-directory " "
                           "--with-common-lib=" common-directory "/.release "
                           "--with-common-include=" common-directory " &")))
      (shell-command command "*configure-buffer*"))))

(defmacro ke/create-compilation-function (argument)
  `(defun ,(intern (concat "ke/compile-" argument)) (&optional prefix)
     "KE Compilation System, calls 'compile without cluttering the default values"
     (interactive "p")
     (cond
      ((eq prefix 4) (ke/compile ,argument :alternative-compiler (concat ke/cc_args-path " colorg++")))
      ((eq prefix 16) (ke/compile ,argument :pre-make (ke/generate-bear-command)))
      (t (ke/compile ,argument)))))

(defmacro ke/bind-key-to-dired-and-c-map (prefix key symbol)
  "Binds the same key to both dired and c-mode"
  `(progn
     (define-key dired-mode-map (concat ,prefix ,key) ,symbol)
     (define-key c-mode-base-map (concat ,prefix ,key) ,symbol)))

(ke/create-compilation-function "clean")
(ke/create-compilation-function "all")
(ke/create-compilation-function "check")
(ke/create-compilation-function "deb-main-deploy")

(require 'cc-mode)
(require 'eshell)

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

;; Eshell
(defun eshell/ke-compile (argument)
  (ke/compile argument))

(provide 'init-orange)
