;; init-projectile.el -- Projectile project management

(require-package 'projectile)
(require 'projectile)

(setq projectile-cache-file (concat my/dotemacs-cache-directory "projectile.cache"))
(setq projectile-known-projects-file (concat my/dotemacs-cache-directory "projectile-bookmarks.eld"))
(setq projectile-indexing-method 'alien)

(add-to-list 'projectile-globally-ignored-directories "elpa")
(add-to-list 'projectile-globally-ignored-directories ".cache")
(add-to-list 'projectile-globally-ignored-directories "node_modules")

(projectile-global-mode t)


(provide 'init-projectile)
