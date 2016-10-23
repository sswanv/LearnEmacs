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
			  reveal-in-osx-finder
			  web-mode
			  js2-refactor
			  expand-region
			  iedit
			  org-pomodoro
			  helm-ag
			  flycheck
			  auto-yasnippet
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
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode))
       auto-mode-alist))

(require 'popwin)
(popwin-mode t)

(load-theme 'monokai t)

(global-company-mode t)

(add-hook 'web-mode-hook 'my-web-mode-indent-setup)

(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
	(setq js-indent-level (if (= js-indent-level 2) 4 2))
	(setq js2-basic-offset (if (= js2-basic-offset 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
	     (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
	     (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))

  (setq indent-tabs-mode nil))

(add-hook 'js2-mode-hook #'js2-refactor-mode)

(require 'org-pomodoro)

(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(provide 'init-packages)
