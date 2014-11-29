;; init-eshell.el -- Set up eshell
(require 'eshell)

(setq
 eshell-directory-name "~/.emacs.d/eshell"
 eshell-scroll-to-bottom-on-input 'all
 eshell-buffer-shorthand t
 eshell-aliases-file "~/.emacs.d/eshell-aliases"
 eshell-glob-case-insensitive t
 eshell-error-if-no-glob t
 eshell-history-size 4096)

 ;; Plan9 smart shell
(add-to-list 'eshell-modules-list 'eshell-smart)
(setq
 eshell-where-to-jump 'begin
 eshell-review-quick-commands nil
 eshell-smart-space-goes-to-end t)

;; Some functions
(defun eshell/clear ()
  "Clears the buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Open file
(defun eshell/ff (&rest args)
  (when (not (null args))
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

;; ll
(defun eshell/ll (&rest args)
  (eshell/ls "-l" args))

;; Show the git branch on prompt
(defun my-current-git-branch ()
  (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                           when (string-match "^\*" match)
                           collect match))))
    (if (not (eq branch nil))
        (concat " [" (substring branch 2) "]")
      "")))

(defun my-eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize (my-current-git-branch) 'face 'font-lock-function-name-face)
          (propertize " $ " 'face 'font-lock-constant-face)))

;; em-prompt
(setq eshell-prompt-function 'my-eshell-prompt)

(provide 'init-eshell)
