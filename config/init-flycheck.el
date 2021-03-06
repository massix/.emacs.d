;; init-flycheck.el -- Init flycheck and friends

(require-package 'flycheck)

(after 'flycheck
       (setq flycheck-check-syntax-automatically '(save mode-enabled))
       (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
       (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
       (setq flycheck-standard-error-navigation nil))

(global-flycheck-mode -1)

(provide 'init-flycheck)
