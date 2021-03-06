
(require 'cl)

(when (>= emacs-major-version 24)
  (require 'package)
  ;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;; 			   ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  (setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
			   ("melpa" . "http://elpa.zilongshanren.com/melpa/")))
  )

;;add whatever packages you want here
(defvar sswanv/packages '(
			  company
			  monokai-theme
			  hungry-delete
			  swiper
			  counsel smartparens
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
			  evil
			  evil-leader
			  window-numbering
			  powerline
			  evil-surround
			  evil-nerd-commenter
			  which-key
			  use-package
			  company-anaconda
			  anaconda-mode
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

(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      (setq exec-path-from-shell-arguments '("-l")))

    (exec-path-from-shell-initialize)
    )
  )

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

(evil-mode t)
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

(global-evil-leader-mode t)
(evil-leader/set-key
  "ff" 'find-file
  "fr" 'recentf-open-files
  "bb" 'switch-to-buffer
  "bk" 'kill-buffer
  "pf" 'counsel-git
  "ps" 'helm-do-ag-project-root
  "0" 'select-window-0
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "w/" 'split-window-right
  "w-" 'split-window-below
  ":" 'counsel-M-x
  "wm" 'delete-other-windows
  "qq" 'save-buffers-kill-emacs)

(window-numbering-mode t)
;; (require 'powerline)
;; (powerline-default-theme)

(require 'evil-surround)
(global-evil-surround-mode t)

(define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
(evilnc-default-hotkeys)

(add-hook 'occur-mode-hook
	  (lambda ()
	    (evil-add-hjkl-bindings occur-mode-map 'emacs
	      (kbd "/")       'evil-search-forward
	      (kbd "n")       'evil-search-next
	      (kbd "N")       'evil-search-previous
	      (kbd "C-d")     'evil-scroll-down
	      (kbd "C-u")     'evil-scroll-up
	      )))

(which-key-mode t)

(add-hook 'python-mode-hook
	  (lambda()
	    (set (make-local-variable 'company-backends) '((company-anaconda company-dabbrev-code) company-dabbrev))))

(provide 'init-packages)
