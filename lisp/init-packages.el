(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			   ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  )
(require 'cl)

;;add whatever packages you want here
(defvar sswanv/packages '(
			  company
			  monokai-theme
			  hungry-delete
			  swiper
			  counsel
			  smartparens
			  js2-mode
			  nodejs-repl
			  exec-path-from-shell
			  popwin
			  )  "Default packages")

(setq package-selected-packages sswanv/packages)

(defun sswanv/packages-installed-p ()
  (loop for pkg in sswanv/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (sswanv/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg sswanv/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(global-hungry-delete-mode t)

(smartparens-global-mode t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))

(require 'popwin)
(popwin-mode t)

(load-theme 'monokai t)

(global-company-mode t)

(provide 'init-packages)
