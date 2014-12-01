;; init-core.el -- Inits the core functionalities of Emacs

;; Start the server if it is not already running
(require 'server)
(unless (server-running-p)
  (server-start))

;; Recent files
(require 'recentf)
(setq
 recentf-save-file (concat my/dotemacs-cache-directory "recentf")
 recentf-max-saved-items 1000
 recentf-max-menu-items 500)
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(recentf-mode 1)
(run-with-timer 1800 1800 'recentf-save-list)


;; ERC logs
(setq erc-log-channels-directory (concat my/dotemacs-cache-directory "erc/logs"))

;; VC
(setq vc-make-backup-files t)

;; imenu
(setq-default imenu-auto-rescan t)

;; enable narrowing
(put 'narrow-to-region 'disabled nil)

;; enable down/upcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; dired
(require 'dired-x)

;; compilation
(setq
 compile-always-kill t
 compilation-ask-about-save t)

;; Colored COMINT buffer for compilation
(add-hook 'compilation-filter-hook
          (lambda ()
            (when (eq major-mode 'compilation-mode)
              (require 'ansi-color)
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region (point-min) (point-max))))))

;; bookmarks
(setq
 bookmark-default-file (concat my/dotemacs-cache-directory "bookmarks")
 bookmark-save-flag 1)

;; fringe
(when (display-graphic-p)
  (fringe-mode 16))

;; ediff
(setq ediff-split-window-function 'split-window-horizontally  ;; side-by-side
      ediff-window-setup-function 'ediff-setup-windows-plain) ;; no extra frames

;; re-builder
(setq reb-re-syntax 'string)

;; clean up old buffers periodically
(require 'midnight)

;; store most files in the cache
(setq backup-directory-alist
      `((".*" . ,(concat my/dotemacs-cache-directory "backups")))
      auto-save-file-name-transforms
      `((".*" ,(concat my/dotemacs-cache-directory "backups") t))
      auto-save-list-file-prefix
      (concat my/dotemacs-cache-directory "auto-save-list/saves-"))


;; better scrolling
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)


;; better buffer names for duplicates
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-p t)


(defun my-do-not-kill-scratch-buffer ()
  (if (member (buffer-name (current-buffer)) '("*scratch*" "*Messages*"))
      (progn
        (bury-buffer)
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'my-do-not-kill-scratch-buffer)


(defalias 'yes-or-no-p 'y-or-n-p)
(xterm-mouse-mode t)

;; Scroll half-page with C-v and M-v instead of the full page
(require 'view)
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)
(setq ring-bell-function (lambda () ()))
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)
(setq save-interprogram-paste-before-kill t)
(setq create-lockfiles nil)

(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

(which-function-mode t)
(blink-cursor-mode -1)
(global-auto-revert-mode 1)
(electric-indent-mode t)
(transient-mark-mode 1)
(delete-selection-mode 1)

(setq-default
 c-default-style "linux"
 tab-width 4
 c-basic-offset 4
 indent-tabs-mode nil)

(defun my-find-file-check-large-file ()
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook (lambda ()
                            (my-find-file-check-large-file)
                            (visual-line-mode)
                            (unless (eq major-mode 'org-mode)
                              (setq show-trailing-whitespace t))))


(random t) ;; seed

(provide 'init-core)
