;; init-company.el -- Company and friends

(require-package 'company)
(require-package 'company-c-headers)
(require 'cc-mode)

(require 'company)
(require 'company-c-headers)

(defcustom my/company-use-rtags t
  "Set to nil if you do not want to use rtags"
  :group 'my/dotemacs)

(defcustom my/company-use-irony nil
  "Set to t if you want to use irony"
  :group 'my/dotemacs)


(defun my/increase-company-async-timeout (time)
  "Increases the company timeout to the given value in seconds."
  (interactive "nNew timeout: ")
  (setq company-async-timeout time))


;; Some defaults
(setq company-idle-delay 0.2
      company-minimum-prefix-length 1
      company-show-numbers t
      company-tooltip-limit 20
      company-dabbrev-downcase t
      company-dabbrev-ignore-case nil)

(setq company-global-modes
      '(not eshell-mode comint-mode org-mode erc-mode))

;; Remove the following from the backends, we will setup them later
(delete 'company-semantic company-backends) ;; Too buggish
(delete 'company-clang company-backends)    ;; Replaced by irony/rtags
(add-to-list 'company-backends 'company-c-headers)

;; Change Company's timeout
(define-key c-mode-base-map (kbd "C-c c t") 'my/increase-company-async-timeout)

;; Set up rtags
(when my/company-use-rtags
  (require-package 'rtags)
  (require 'company-rtags)
  (add-to-list 'company-backends 'company-rtags)
  (setq rtags-completions-enabled t
        company-rtags-begin-after-member-access t
        company-async-timeout 10
        local-rtags-enabled t)
  (define-key c-mode-base-map (kbd "C-c r d") 'rtags-diagnostics)
  (define-key c-mode-base-map (kbd "C-c r t") 'rtags-taglist)
  (define-key c-mode-base-map (kbd "C-c r R") 'rtags-find-references-at-point)
  (define-key c-mode-base-map (kbd "C-c r F") 'rtags-fix-fixit-at-point)
  (define-key c-mode-base-map (kbd "C-c r f") 'rtags-find-symbol-at-point)
  (define-key c-mode-base-map (kbd "C-c r v") 'rtags-find-virtuals-at-point)
  (define-key c-mode-base-map (kbd "C-c r [") 'rtags-location-stack-back)
  (define-key c-mode-base-map (kbd "C-c r ]") 'rtags-location-stack-forward)
  (define-key c-mode-base-map (kbd "C-c r i") 'rtags-print-cursorinfo)
  (define-key c-mode-base-map (kbd "C-c r C-r") 'rtags-rename-symbol) ;; might be dangerous
  (define-key c-mode-base-map (kbd "C-c r C-x C-f") 'rtags-find-file) ;; Not very helpful though
  (define-key c-mode-base-map (kbd "C-c r x") 'rtags-restart-process))

;; Set up Irony
(when my/company-use-irony
  (require-package 'company-irony)
  (require 'company-irony)
  (add-to-list 'company-backends 'company-irony)
  (defun my/irony-mode-hook ()
    "Replace the `completion-at-point' and `complete-symbol' bindings in
   irony-mode's buffers by irony-mode's function"
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (define-key irony-mode-map (kbd "C-c C-b") 'irony-cdb-menu)
  (add-hook 'irony-mode-hook 'my-irony-mode-hook))

(global-company-mode)
(provide 'init-company)
