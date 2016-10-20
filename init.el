;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(add-to-list 'load-path "~/.emacs.d/lisp/")

(package-initialize)

(defun open-my-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-keybindings)

;; (load "init-packages")
;; (load "init-ui")
;; (load "init-better-defaults")
;; (load "init-org")
;; (load "init-keybindings")

(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))

(load-file custom-file)

