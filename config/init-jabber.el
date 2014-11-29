;; init-jabber.el -- Configure Jabber
(require-package 'jabber)
(require 'jabber)

(defcustom my/jabber-user-name nil
  "Default user name to use on jabber"
  :group 'my/dotemacs)

(defcustom my/jabber-password nil
  "Default password to use on jabber"
  :group 'my/dotemacs)

(setq jabber-avatar-cache-directory (concat my/dotemacs-cache-directory "jabber/"))

(setq jabber-account-list
      '((my/jabber-user-name
         (:password . my/jabber-password)
         (:port . 443)
         (:connection-type . ssl))))

(add-hook 'jabber-post-connect-hooks 'jabber-mode-line)
(add-hook 'jabber-post-disconnect-hook
          (lambda () (jabber-mode-line -1)))


(provide 'init-jabber)

