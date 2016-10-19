
(setq ring-bell-function 'ignore)

(global-auto-revert-mode t)

(global-linum-mode t)

(setq make-backup-files nil)

(setq auto-save-default nil)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ("8ss" "sswanv")
					    ))

(delete-selection-mode t)

(provide 'init-better-defaults)
