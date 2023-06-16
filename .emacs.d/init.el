;; disable menu, scroll, and tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; set frame transparency
(set-frame-parameter nil 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80))

;; Put backup and auto-save into ~/.emacs.d/
;;
;; https://overflow.smnz.de/exchange/emacs/questions/33/put-all-backups-into-one-backup-folder
(let ((backup-dir "~/.emacs.d/backups/")
      (auto-saves-dir "~/.emacs.d/autosaves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; enable modus-vivendi theme
(load-theme 'modus-vivendi t)

;; enable default disabled features
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; sly
(setq inferior-lisp-program "sbcl --noinform --no-linedit")

;; set warning buffer to only log errors
(setq warning-minimum-level :error)

;; set re-builder syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; set dictionary-search dictionary server
(setq dictionary-server "dict.org")

;; save emacs state
(require 'desktop)
(desktop-read)
(setq desktop-path (list "~/.emacs.d/desktop-save/")
      desktop-restore-eager 4
      desktop-auto-save-timeout 10
      desktop-load-locked-desktop t
      desktop-restore-forces-onscreen nil
      savehist-mode t
      desktop-save-mode t)

;; Don't kill emacs, just close frames
(keymap-global-unset "C-x C-c")
(keymap-global-set "C-x C-c" 'delete-frame)

;; if I really need to kill emacs
;;; define a function that determines if in X or in terminal and, based on that,
;;; will call *save-buffers-kill-emacs* or *save-buffers-kill-terminal*
(keymap-global-set "C-S-x C-S-c" 'save-buffers-kill-emacs)

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
(setq straight-repository-branch "develop") ;; workaround for https://github.com/radian-software/straight.el/issues/1053

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
  (pdf-tools-install t nil nil t))
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
(use-package racket-mode)
(use-package vterm					
  :init
  ; https://github.com/akermu/emacs-libvterm#frequently-asked-questions-and-problems
  (setq vterm-always-compile-module t
	vterm-kill-buffer-on-exit t
	vterm-max-scrollback 100000)
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
              (buffer-face-mode t))))
(use-package multi-vterm
  :config
  (setq multi-vterm-dedicated-window-height-percent 50))
(use-package detached
  :init
  (detached-init)
  :bind
  (;; Replace `async-shell-command' with `detached-shell-command'
   ([remap async-shell-command] . detached-shell-command)
   ;; Replace `compile' with `detached-compile'
   ([remap compile] . detached-compile)
   ([remap recompile] . detached-compile-recompile)
   ;; Replace built in completion of sessions with `consult'
   ([remap detached-open-session] . detached-consult-session)
   ;; Replace `vterm' with `detached-vterm-send-input'
   ([remap vterm] . detached-vterm-send-input))
  :custom
  ((detached-show-output-on-attach t)
   (detached-vterm-mode 1)
   (detached-terminal-data-command system-type)))
(use-package magit)
(use-package scratch
  :config
  (global-set-key (kbd "C-c s") 'scratch))
