;; init-smartparens.el -- Init smart parens

(require-package 'smartparens)
(require 'smartparens-config)

(setq sp-show-pair-delay 0)
(setq sp-show-pair-from-inside t)
(setq sp-autoescape-string-quote nil)
(setq sp-autoinsert-if-followed-by-same 1)
(setq sp-highlight-pair-overlay nil)
(setq sp-autoescape-string-quote nil)
(setq sp-autoescape-string-quote-if-empty nil)

;; (sp-use-smartparens-bindings)
(smartparens-global-mode t)

(show-smartparens-global-mode t)
(show-paren-mode -1)

(defun my-open-block-c-mode (id action context)
  (when (eq action 'insert)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(sp-pair "{" nil :post-handlers '(:add (my-open-block-c-mode "RET")))
(sp-pair "[" nil :post-handlers '(:add (my-open-block-c-mode "RET")))

(provide 'init-smartparens)

