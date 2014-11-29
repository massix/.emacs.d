;; init-helm.el -- Init Helm modules
(require-package 'helm)
(require-package 'helm-swoop)
(require-package 'helm-descbinds)

(setq helm-command-prefix-key "C-c h")
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)


(after "projectile-autoloads"
  (require-package 'helm-projectile))

(after "company-autoloads"
  (require-package 'helm-company))
(require 'helm-grep)
(provide 'init-helm)
