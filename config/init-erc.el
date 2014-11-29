;; init-erc.el -- Init ERC, submodules and even slack

(defcustom my/slack-nick nil
  "Nickname to use on slack"
  :group 'my/dotemacs)

(defcustom my/slack-password nil
  "Password to use on slack"
  :group 'my/dotemacs)

(defcustom my/slack-server nil
  "Server to use on slack"
  :group 'my/dotemacs)

(defun start-slack ()
  "Connect to orwell's slack"
  (interactive)
  (erc-tls :server     my/slack-server
           :nick       my/slack-nick
           :password   my/slack-password))

(setq erc-hide-list '("JOIN" "PART" "QUIT")
      erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "[%H:%M] "
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-truncate-mode t)

(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq erc-fill-column (- (window-width) 2))))

(provide 'init-erc)
