;; load the welcome screen for encouragement
(setq inhibit-startup-screen nil)
;; disable menu, scroll, and tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)
;; enable modus-vivendi theme
(load-theme 'modus-vivendi t)
;; allow `C-x C-l`
(put 'downcase-region 'disabled nil)
;; allow `C-x C-u
(put 'upcase-region 'disabled nil)
;; sly
(setq inferior-lisp-program "sbcl --noinform --no-linedit")
;; set re-builder syntax
(require 're-builder)
(setq reb-re-syntax 'string)
;; set line and column number modes on
(global-display-line-numbers-mode)
(setq column-number-mode t)
;; disable line numbers in pdf-tools minor mode
(add-hook 'pdf-tools-enabled-hook (lambda () (display-line-numbers-mode -1)))
;; show 80 character indicator in programming modes enabled, but not scratch    
(add-hook 'prog-mode-hook
	  (lambda ()
	    (when (not (equal "*scratch*" (buffer-name)))
	      (progn (setq fill-column 80)
		     (display-fill-column-indicator-mode)))))
;; tree-sitter
(require 'treesit)

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; packages
(use-package pdf-tools
  :config
  (pdf-tools-install 'NO-QUERY-P t))
(use-package sly)
(use-package zygospore
  :config
  (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))
(use-package ace-window
  :config
  (global-set-key (kbd "C-x o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))
(use-package helm
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))
