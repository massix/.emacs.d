(require-package 'undo-tree)
(require 'undo-tree)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      `(("." . ,(concat my/dotemacs-cache-directory "undo"))))
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(global-undo-tree-mode)


(require-package 'multiple-cursors)
(require-package 'wgrep)

(require-package 'project-explorer)

(require 'project-explorer)
(after 'project-explorer
  (setq pe/cache-directory (concat my/dotemacs-cache-directory "project-explorer"))
  (setq pe/omit-regex (concat pe/omit-regex "\\|^node_modules$"))
  (setq pe/side 'right))


(require-package 'ace-jump-mode)
(require 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Eamcs quick move minor mode"
  t)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(require-package 'expand-region)
(require 'expand-region)
(define-key global-map (kbd "C-=") 'er/expand-region)
(define-key global-map (kbd "C--") 'er/contract-region)


(require-package 'editorconfig)
(require 'editorconfig)

(require-package 'aggressive-indent)
(require 'aggressive-indent)
(add-to-list 'aggressive-indent-excluded-modes 'org-mode)
(global-aggressive-indent-mode)

(require-package 'windsize)
(require 'windsize)
(setq windsize-cols 16)
(setq windsize-rows 8)
(windsize-default-keybindings)

(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require-package 'framemove)
(require 'framemove)
(setq framemove-hook-into-windmove t)

(require-package 'discover-my-major)

(require-package 'popwin)
(require 'popwin)
(popwin-mode 1)


;; Setup PATH correctly on MacBook
(when my/is-a-mac
  (require-package 'vkill)
  (setq mac-right-option-modifier nil
        mac-command-modifier 'meta
        mac-right-command-modifier 'meta)
  (setenv "PATH" (concat (getenv "PATH") ":"
                         "/usr/texbin:"
                         "/usr/local/bin:"
                         "/usr/local/sbin:"))

  (defun set-exec-path (string)
    (maplist (lambda (elt) (add-to-list 'exec-path (car elt)))
             (split-string string ":")))
  (set-exec-path (getenv "PATH")))

;; Package: volatile-highlights
(require-package 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Package: clean-aindent-mode
(require-package 'clean-aindent-mode)
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require-package 'dtrt-indent)
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-verbosity 0)

;; Package: anzu
(require-package 'anzu)
(require 'anzu)
(global-anzu-mode)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)


(windmove-default-keybindings)

;; Customized functions
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)


;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; prelude-editing.el
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)


;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-o") 'prelude-smart-open-line)

(require-package 'comment-dwim-2)
(require 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)

(require-package 'iedit)
(require 'iedit)
(global-set-key (kbd "C-;") 'iedit-mode)

;; Ask for confirmation before leaving
(global-set-key (kbd "C-x C-c")
                (lambda ()
                  (interactive)
                  (when (yes-or-no-p "Are you really sure?")
                    (call-interactively #'save-buffers-kill-emacs))))


(provide 'init-misc)
