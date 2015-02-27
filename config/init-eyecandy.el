;; init-eyecandy.el -- The eye wants its part too!

(show-paren-mode)
(setq show-paren-delay 0)

(line-number-mode t)
(column-number-mode t)
(display-time-mode t)
(size-indication-mode t)

(require-package 'diminish)
(diminish 'visual-line-mode)
(after 'autopair (diminish 'autopair-mode))
(after 'undo-tree (diminish 'undo-tree-mode))
(after 'auto-complete (diminish 'auto-complete-mode))
(after 'projectile (diminish 'projectile-mode))
(after 'guide-key (diminish 'guide-key-mode))
(after 'eldoc (diminish 'eldoc-mode))
(after 'smartparens (diminish 'smartparens-mode))
(after 'company (diminish 'company-mode))
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'git-gutter+ (diminish 'git-gutter+-mode))
(after 'magit (diminish 'magit-auto-revert-mode))
(after 'highlight-symbol-mode (diminish 'highlight-symbol-mode))
(after 'indent-guide-mode (diminish 'indent-guide-mode))
(after 'dtrt-indent-mode (diminish 'dtrt-indent-mode))


(require-package 'heroku-theme)
(require-package 'zenburn-theme)
(require-package 'smart-mode-line)
(require 'smart-mode-line)
(setq sml/show-client t)
(setq sml/show-eol t)
(setq sml/show-frame-identification t)

(sml/setup)
(sml/apply-theme 'respectful)

(when (fboundp 'global-prettify-symbols-mode)
  (progn
    (global-prettify-symbols-mode)))
    ;; (add-hook 'c-mode-hook
    ;;           (lambda ()
    ;;             (push '("return" . 8592) prettify-symbols-alist)
    ;;             (push '("<="     . ?≤) prettify-symbols-alist)
    ;;             (push '(">="     . ?≥) prettify-symbols-alist)
    ;;             (push '("fn"     . ?𝑓) prettify-symbols-alist)
    ;;             (push '("!="     . ?≢) prettify-symbols-alist)
    ;;             (push '("!="     . ?≢) prettify-symbols-alist)
    ;;             (push '("=="     . ?≡) prettify-symbols-alist)
    ;;             (push '("this"   . ?◎) prettify-symbols-alist)
    ;;             (push '("->"     . ?⇝) prettify-symbols-alist)))
    ;; (add-hook 'c++-mode-hook
    ;;           (lambda ()
    ;;             (push '("return" . 8592) prettify-symbols-alist)
    ;;             (push '("<="     . ?≤) prettify-symbols-alist)
    ;;             (push '(">="     . ?≥) prettify-symbols-alist)
    ;;             (push '("fn"     . ?𝑓) prettify-symbols-alist)
    ;;             (push '("!="     . ?≢) prettify-symbols-alist)
    ;;             (push '("!="     . ?≢) prettify-symbols-alist)
    ;;             (push '("=="     . ?≡) prettify-symbols-alist)
    ;;             (push '("this"   . ?◎) prettify-symbols-alist)
    ;;             (push '("->"     . ?⇝) prettify-symbols-alist)))))

(require-package 'color-identifiers-mode)
(global-color-identifiers-mode)
(diminish 'color-identifiers-mode)

(require-package 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.3)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(require-package 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(require-package 'highlight-quoted)
(add-hook 'prog-mode-hook 'highlight-quoted-mode)


;; this is pretty slow on big files
;; (require-package 'indent-guide)
;; (require 'indent-guide)
;; (setq indent-guide-recursive t)
;; (add-to-list 'indent-guide-inhibit-modes 'package-menu-mode)
;; (indent-guide-global-mode 1)


(add-hook 'find-file-hook 'hl-line-mode)


(provide 'init-eyecandy)
