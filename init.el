;; My Emacs configuration, the modular idea is mainly stolen from
;; https://github.com/bling/dotemacs/blob/master/init.el

;; Add customizable things

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defgroup my/dotemacs nil
  "Customization group"
  :group 'local)

(defcustom my/dotemacs-cache-directory (concat user-emacs-directory ".cache/")
  "Storage location for persistent files"
  :group 'my/dotemacs)

;; Some defaults
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(unless (display-graphic-p)
  (menu-bar-mode -1))

(defvar my/is-a-mac
  (eq system-type 'darwin))

;; Where the custom packages will be
(add-to-list 'load-path
             (concat user-emacs-directory "config"))

(require 'cl)
(require 'init-packages)
(require 'init-util)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defcustom my/dotemacs-modules
  '(init-core
    init-eshell
    init-org
    init-erc
    init-eyecandy
    init-smartparens
    init-company
    init-projectile
    init-helm
    init-ido
    init-vcs
    init-flycheck
    init-markdown
    init-misc
    init-bindings
    init-macros
    init-mu
    init-jabber
    init-override)
  "Modules to load with emacs"
  :group 'my/dotemacs)


(add-to-list
 'after-init-hook
 (lambda ()
   (dolist (module my/dotemacs-modules)
     (with-demoted-errors "##### INIT-ERROR ##### %s" (require module)))))

