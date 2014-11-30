;; init-orange.el -- Inits some random functions I use at work

(defcustom ke/naughty-notifier-active
  nil
  "Set to true if you want to use the naughty notifier"
  :group 'my/dotemacs
  :type 'boolean)

(defvar ke/original-path)

(defun ke/setup-path ()
  "Sets the KE's path"
  (setq ke/original-path (getenv "PATH"))
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

(defun ke/reset-path ()
  "Reset the path to the original one"
  (when ke/original-path (setenv "PATH" ke/original-path)))

(defun ke/find-root-dir (path look-for-member &optional discarded)
  "Recursively finds for a directory in the given path"
  (let* ((split-path (split-string path "/"))
         (new-split-path (butlast split-path 1)))
    (cond
     ((string-equal "~" path) '(nil discarded))
     ((string-equal "/" path) '(nil discarded))
     ((null path) '(nil discarded))
     ((member look-for-member (directory-files path))
      (cons path (mapconcat 'identity discarded "/")))
     (t (ke/find-root-dir
         (mapconcat 'identity new-split-path "/")
         look-for-member
         (cons (car (last split-path 1)) discarded))))))

(setq compilation-finish-functions
      (lambda (arg0 arg1)
        (ke/notify (format "Compilation is over ['%s']" (ke/trim-string arg1)))))

(defun ke/compile (argument &optional parallel-jobs silent alternative-compiler pre-make)
  "KE Compilation System"
  (interactive "sTarget: ")
  (ke/setup-path)
  (let* ((couple (ke/find-root-dir (file-name-directory buffer-file-name) ".bzr"))
         (full-path (concat "/" (car couple) "/.release/" (cdr couple))))
    (setq compile-command
          (concat pre-make " make -C \"\""
                  full-path
                  "\" "
                  (when parallel-jobs (concat "-j" (number-to-string parallel-jobs)))
                  " "
                  (when silent "--silent")
                  " "
                  (when alternative-compiler (concat "CXX=\"" alternative-compiler "\""))
                  " "
                  argument))
    (call-interactively 'compile)
    (ke/reset-path)))

;; Set the bear command
(defun ke/generate-bear-command (file)
  "Return a compatible bear command for the buffer"
  (let* ((couple (ke/find-root-dir (file-name-directory buffer-file-name) ".bzr"))
         (full-path (concat "/" (car couple) "/.release/" (cdr couple))))
    (concat "~/dev/Bear/build/src/bear -o " full-path "/compile_commands.json -- ")))

(require 'cc-mode)
(define-key c-mode-base-map (kbd "C-c k c")
  (lambda () (interactive) (ke/compile "clean" 2 t)))

(define-key c-mode-base-map (kbd "C-c k t")
  (lambda () (interactive) (ke/compile "check" 2 t)))

(define-key c-mode-base-map (kbd "C-c k d")
  (lambda () (interactive) (ke/compile "deb-main-deploy" 2 t)))

(define-key c-mode-base-map (kbd "C-c k k")
  (lambda () (interactive) (ke/compile "" 2 t)))

;; Special compilation command to generate the .clang_complete file
(define-key c-mode-base-map (kbd "C-c k 0 m")
  (lambda () (interactive) (ke/compile "all" 2 nil "~/.emacs.d/cc_args.py $(which colorg++)")))

(define-key c-mode-base-map (kbd "C-c k 1 m")
  (lambda ()
    (interactive) (ke/compile "all" 2 nil nil (ke/generate-bear-command buffer-file-name))))

(provide 'init-orange)
