;; init-ido.el -- Sets up IDO + Smex instead of Helm

(require-package 'ido-vertical-mode)
(require-package 'flx-ido)
(require-package 'ido-ubiquitous)
(require-package 'smex)

(require 'ido-vertical-mode)
(require 'flx-ido)
(require 'ido-ubiquitous)
(require 'smex)

(setq
 ido-enable-prefix nil
 ido-use-virtual-buffers t
 ido-enable-flex-matching t
 ido-create-new-buffer 'always
 ido-use-filename-at-point 'guess
 ido-save-directory-list-file (concat my/dotemacs-cache-directory "ido.last"))

(ido-mode t)
(ido-everywhere t)

(flx-ido-mode t)
(ido-vertical-mode)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ido)
