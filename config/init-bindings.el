;; init-bindings.el -- Common bindings

(defmacro bind (&rest commands)
  "Convience macro which creates a lambda interactive command."
  `(lambda ()
     (interactive)
     ,@commands))


(require-package 'guide-key)
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" ","))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)


(require-package 'guide-key-tip)
(require 'guide-key-tip)
(setq guide-key-tip/enabled t)


(after "smex-autoloads"
       (global-set-key (kbd "M-x") 'smex))

(global-set-key (kbd "RET") 'newline-and-indent)
(delete-selection-mode)

(provide 'init-bindings)
