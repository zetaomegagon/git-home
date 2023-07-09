(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; disable menu, scroll, and tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; set frame transparency
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 93))

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
  (pdf-tools-install t nil nil nil))
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

(use-package minimap
  :config
  (setq minimap-mode t))
