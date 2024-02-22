;; disable startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; disable menu, scroll, and tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; set frame transparency
(progn (set-frame-parameter nil 'alpha-background 90)
       (add-to-list 'default-frame-alist '(alpha-background . 90)))

;; Put backup and auto-save into ~/.emacs.d/
;;
;; https://overflow.smnz.de/exchange/emacs/questions/33/put-all-backups-into-one-backup-folder
(let ((backup-dir "~/.emacs.d/backups/")
      (auto-saves-dir "~/.emacs.d/auto-saves/"))
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

;; undo history
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/"))
      undo-tree-auto-save-history t)

;; enable modus-vivendi theme
(load-theme 'modus-vivendi t)

;; enable default disabled features
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; set warning buffer to only log errors
(setq warning-minimum-level :error)

;; set re-builder syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; set dictionary-search dictionary server
(setq dictionary-server "dict.org")

; Don't kill emacs, just close frames
(keymap-global-unset "C-x C-c")
(keymap-global-set "C-x C-c" 'delete-frame)

; if I really need to kill emacs
(keymap-global-set "C-S-x C-S-c" 'save-buffers-kill-emacs)

;; set line and column number modes on
(setq column-number-mode t)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

;; show 80 character indicator in programming modes enabled, but not scratch
(add-hook 'prog-mode-hook
	  (lambda ()
	    (when (not (equal "*scratch*" (buffer-name)))
	      (progn (setq fill-column 80)
		     (display-fill-column-indicator-mode)))))

;; set auth-sources to only use a gpg backed sources
(setq auth-sources '("~/.authinfo.gpg"))

;; packages
(elpaca-wait)

;;;; org-mode
(use-package org
  :init
  (setq org-confirm-babel-evaluate nil
	org-babel-lisp-eval-fn #'sly-eval)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED" "CANCELED")))
  :config
  (add-hook 'org-mode-hook #'org-indent-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (shell . t))))

(use-package org-web-tools)

;;;; pdf-tools
(use-package pdf-tools
  :config
  (pdf-tools-install t nil nil nil))

;;;; sly
(use-package sly
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl --noinform --no-linedit"
	sly-command-switch-to-existing-lisp 'always)
  :config
  (keymap-global-set "C-c C-j" 'sly-eval-last-expression)
  ;; http://joaotavora.github.io/sly/#Auto_002dSLY
  ;; start sly repl when lisp file is opened
  (add-hook 'sly-mode-hook
	    (lambda ()
	      (unless (sly-connected-p)
		(save-excursion (sly))))))

;;;; zygospore
(use-package zygospore
  :config
  (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

;;;; ace-window
(use-package ace-window
  :config
  (global-set-key (kbd "C-x o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))

;;;; helm
(use-package helm
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))

;;;; company
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;;; eterm-256color
(use-package eterm-256color)

;;;; vterm
(use-package vterm
  :init
  ; https://github.com/akermu/emacs-libvterm#frequently-asked-questions-and-problems
  (setq vterm-always-compile-module t)
  :config
  ; https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#customization
  (setq vterm-kill-buffer-on-exit t
	vterm-copy-exclude-prompt t
	vterm-buffer-name-string t
	vterm-max-scrollback 100000
	vterm-term-environment-variable "eterm-color")
  (define-key vterm-mode-map (kbd "C-'") #'vterm-send-next-key)
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
	      (buffer-face-mode t))))
(elpaca-wait)

;;;; multi-vterm
(use-package multi-vterm
  :config
  (setq multi-vterm-dedicated-window-height-percent 50))

;;;; detached
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

;;;; magit
(use-package magit)

;;;; scratch
(use-package scratch
  :config
  (global-set-key (kbd "C-c s") 'scratch))

;;;; minimap
(use-package minimap)

;;;; transpose-frame
(use-package transpose-frame)

;;;; ement
(use-package ement)

;; powershell.el
(use-package powershell)

;; tree-sitter
(require 'treesit)

;; save emacs state
(require 'desktop)
(desktop-read)
(setq desktop-path (list "~/.emacs.d/desktop-save/")
      ; set this to 0 to avoid emacs.service hitting timeout
      desktop-restore-eager 0
      desktop-auto-save-timeout 5
      desktop-load-locked-desktop t
      desktop-restore-forces-onscreen nil
      savehist-mode t
      desktop-save-mode t)

