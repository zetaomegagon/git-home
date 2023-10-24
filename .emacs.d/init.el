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

;;;; org-mode
(use-package org
  :init
  (setq org-confirm-babel-evaluate nil
	org-babel-lisp-eval-fn #'sly-eval)
  :config
  (add-hook 'org-mode-hook #'org-indent-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (shell . t))))

;; (use-package org-modern
;;   :config
;;   (add-hook 'org-mode-hook #'org-modern-mode)
;;   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  
;;   (defun org-rice-setup () 
;;     ;; Add frame borders and window dividers
;;     (modify-all-frames-parameters
;;      '((right-divider-width . 40)
;;        (internal-border-width . 40)))
;;     (dolist (face '(window-divider
;; 		    window-divider-first-pixel
;; 		    window-divider-last-pixel))
;;       (face-spec-reset-face face)
;;       (set-face-foreground face (face-attribute 'default :background)))
;;     (set-face-background 'fringe (face-attribute 'default :background))

;;     (setq
;;      ;; Edit settings
;;      org-auto-align-tags nil
;;      org-tags-column 0
;;      org-catch-invisible-edits 'show-and-error
;;      org-special-ctrl-a/e t
;;      org-insert-heading-respect-content t

;;      ;; Org styling, hide markup etc.
;;      org-hide-emphasis-markers t
;;      org-pretty-entities t
;;      org-ellipsis "…"

;;      ;; Agenda styling
;;      org-agenda-tags-column 0
;;      org-agenda-block-separator ?─
;;      org-agenda-time-grid
;;      '((daily today require-timed)
;;        (800 1000 1200 1400 1600 1800 2000)
;;        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;      org-agenda-current-time-string
;;      "⭠ now ─────────────────────────────────────────────────"))
;;   (add-hook 'org-mode-hook #'org-rice-setup)
;;   (add-hook 'org-agenda-finalize-hook #'org-rice-setup))

(use-package org-web-tools)

;;;; pdf-tools
(use-package pdf-tools
  :config
  (pdf-tools-install t nil nil nil))

;;;; sly
(use-package sly
  :init
  (setq inferior-lisp-program "sbcl --noinform --no-linedit"
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

;;;; racket-mode
(use-package racket-mode)

;;;; vterm
(use-package vterm
  :init
  ; https://github.com/akermu/emacs-libvterm#frequently-asked-questions-and-problems
  (setq vterm-always-compile-module t)
  :config
  (setq vterm-kill-buffer-on-exit t
	vterm-max-scrollback 100000)
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
	      (buffer-face-mode t))))

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
