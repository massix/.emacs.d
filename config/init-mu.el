;; init-mu.el -- Mu and related options

(defcustom my/mu4e-site-lisp-dir
  "/usr/local/share/emacs/site-lisp/mu4e"
  "Where to find the mu4e.el file"
  :group 'my/dotemacs
  :type 'string)

(defcustom my/mu4e-maildir
  (expand-file-name "~/mail/gmail/")
  "Maildir for mu4e"
  :group 'my/dotemacs
  :type 'string)

(defcustom my/mu4e-drafts-folder
  "/[Gmail]/.Bozze"
  "Drafts folder for mu4e"
  :group 'my/dotemacs
  :type 'string)

(defcustom my/mu4e-sent-folder
  "/[Gmail]/.Posta inviata"
  "Sent folder for mu4e"
  :group 'my/dotemacs
  :type 'string)

(defcustom my/mu4e-trash-folder
  "/[Gmail]/.Cestino"
  "Trash folder"
  :group 'my/dotemacs
  :type 'string)

(defcustom my/mu4e-mu-binary
  "/usr/local/bin/mu"
  "Mu4e binary"
  :group 'my/dotemacs
  :type 'string)

(defcustom my/mu4e-get-mail-command
  "mbsync gmail"
  "Default command to sync mail"
  :group 'my/dotemacs
  :type 'string)

(defcustom my/mu4e-use-maildir-extension
  t
  "Set to t if you want to use maildir extension"
  :group 'my/dotemacs
  :type 'boolean)

(defcustom my/mu4e-user-mail-address
  "massimo.gengarelli@gmail.com"
  "Set to your user-mail address"
  :group 'my/dotemacs
  :type 'string)

(defcustom my/mu4e-user-full-name
  "Massimo Gengarelli"
  "Set to your name"
  :group 'my/dotemacs
  :type 'string)

(defcustom my/mu4e-compose-signature
  (concat
   "Massimo Gengarelli\n"
   "M: +33 (0)6 43 87 57 67\n"
   "E: massimo.gengarelli@gmail.com")
  "A function generating the signature"
  :group 'my/dotemacs
  :type 'function)

(add-to-list 'load-path my/mu4e-site-lisp-dir)

(require 'mu4e)

(require-package 'mu4e-maildirs-extension)
(require 'mu4e-maildirs-extension)

(setq mu4e-maildir          my/mu4e-maildir
      mu4e-drafts-folder    my/mu4e-drafts-folder
      mu4e-sent-folder      my/mu4e-sent-folder
      mu4e-trash-folder     my/mu4e-trash-folder
      mu4e-mu-binary        my/mu4e-mu-binary
      mu4e-get-mail-command my/mu4e-get-mail-command)

(setq mu4e-sent-messages-behavior 'delete
      mu4e-view-show-images t
      mu4e-use-fancy-chars nil)

(setq user-mail-address my/mu4e-user-mail-address
      user-full-name    my/mu4e-user-full-name
      mu4e-compose-signature my/mu4e-compose-signature)

(setq mu4e-compose-auto-include-date t
      mu4e-compose-signature-auto-include t)

(defun my/render-html-message ()
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

(setq mu4e-html2text-command 'my/render-html-message)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(when my/mu4e-use-maildir-extension
  (mu4e-maildirs-extension))

;; Used to send mail
(require 'smtpmail)

(provide 'init-mu)
