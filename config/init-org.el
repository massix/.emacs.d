;; init-org.el -- Setup org-mode things

(require-package 'org-fstree)
(require 'org-fstree)
(when (eq system-type 'darwin)
  (require-package 'org-mac-link)
  (require-package 'org-mac-iCal)
  (require 'org-mac-link)
  (require 'org-mac-iCal)
  (autoload 'org-mac-grab-link "org-mac-link" nil t))

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c o c") 'org-capture)

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-src-fontify-natively t
      org-default-notes-file "~/org/captures.org"
      org-agenda-files '("~/org/")
      org-agenda-diary-file "~/org/diary.org"
      org-fontify-done-headline t
      org-fontify-whole-heading-line t
      org-highlight-latex-and-related '(latex script entities)
      org-pretty-entities t
      org-tags-column 80
      org-reverse-note-order t
      org-todo-keywords '((sequence "TODO" "ONGOING" "DELIVERED" "DONE"))
      org-babel-load-languages '((awk . t)
                                 (emacs-lisp . t)
                                 (python . t)
                                 (clojure . t)
                                 (sh . t)))

;; Org templates
(setq org-capture-templates
      '(
        ("j"
         "A task describing a job (might be Travis, Jenkins, ...)" entry
         (file "~/org/notes.org")
         "* TODO Run job %^{Define job}%?\n\t%i\n\t%F"
         :empty-lines 1
         :immediate-finish t
         :clock-in t
         :clock-keep t
         :clock-resume t)
        ("n"
         "A classic note taken from a file" entry
         (file "~/org/notes.org")
         "* _%F_: /%?/\n\t%i"
         :unnarrowed t
         :empty-lines 1)
        ("t"
         "A Todo item" entry
         (file "~/org/notes.org")
         "* TODO %?\n\t_%F_"
         :empty-lines 1)))

;; Org modules
(setq org-modules
      '(org-bbdb
        org-bibtex
        org-crypt
        org-docview
        org-eshell
        org-eval
        org-eval-light
        org-gnus
        org-habit
        org-info
        org-interactive-query
        org-irc
        org-man
        org-mhe
        org-rmail
        org-w3m))

;; Only load these on Mac OS X
(when (eq system-type 'darwin)
  (add-to-list 'org-modules 'org-mac-link)
  (add-to-list 'org-modules 'org-mac-iCal))


;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(defun my/org-mode-hook ()
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when (eq system-type 'darwmin)
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd ("C-c g") 'org-mac-grab-link))))

(add-hook 'org-mode-hook 'my/org-mode-hook)

(provide 'init-org)
