;; disable startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; disable menu, scroll, and tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; set frame transparency
(progn (set-frame-parameter nil 'alpha-background 95)
       (add-to-list 'default-frame-alist '(alpha-background . 95)))

(defun eb/toggle-frame-transparency ()
  "toggle frame alpha value between 85 and 95"
  (interactive nil)
  (cond ((equal (frame-parameter nil 'alpha-background) 95)
	 (set-frame-parameter nil 'alpha-background 85))
	(t (set-frame-parameter nil 'alpha-background 95))))

(keymap-global-set "<f1>" 'eb/toggle-frame-transparency)

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

;; Don't kill emacs, just close frames
(keymap-global-unset "C-x C-c")
(keymap-global-set "C-x C-c" 'delete-frame)

;; if I really need to kill emacs
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

;; winner mode
(winner-mode)

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
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((lisp   . t)
				 (shell  . t)
				 (awk    . t)))
  :hook
  (org-mode . (lambda ()
		(org-indent-mode)
		(visual-line-mode)
		(toggle-word-wrap))))

;;;; org-web-toos
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
  :bind
  ("C-c C-j" . sly-eval-last-expression)
  :config
  ;; http://joaotavora.github.io/sly/#Auto_002dSLY
  ;; start sly repl when lisp file is opened
  :hook
  (sly-mode . (lambda ()
		(unless (sly-connected-p)
		  (save-excursion (sly))))))

;;;; zygospore
(use-package zygospore
  :bind
  ("C-x 1" . zygospore-toggle-delete-other-windows))

;;;; ace-window
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))

;;;; helm
(use-package helm
  :bind (("M-x"     . helm-M-x)
	 ("C-x b"   . helm-buffers-list)
	 ("C-x r b" . helm-filtered-bookmarks)
	 ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1))

;;;; company
(use-package company
  :hook
  (after-init . global-company-mode))

;;;; which-key
(use-package which-key
  :config
  (which-key-mode))

;;;; eterm-256color
(use-package eterm-256color)


;;;; vterm
(use-package vterm
  :init
  ;; https://github.com/akermu/emacs-libvterm#frequently-asked-questions-and-problems
  (setq vterm-always-compile-module t)
  :config
  ;; https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#customization
  (setq vterm-kill-buffer-on-exit t
	vterm-copy-exclude-prompt t
	;;vterm-buffer-name-string t
	vterm-term-environment-variable "eterm-color"
	vterm-max-scrollback 100000)
  (define-key vterm-mode-map (kbd "C-'") #'vterm-send-next-key)
  :hook
  (vterm-mode . (lambda ()
		       (set
			(make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
		       (buffer-face-mode t))))

(elpaca-wait)

;;;; multi-vterm
(use-package multi-vterm
  :init
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
  :bind ("C-c s" . scratch))

;;;; minimap
(use-package minimap
  :init
  (setq minimap-width-fraction 0.10
	minimap-minimum-width 29
	minimap-window-location 'left
	minimap-buffer-name " *MINIMAP*"
	minimap-update-delay 0.1
	minimap-always-recenter nil
	minimap-recenter-type 'relative
	minimap-hide-scroll-bar t
	minimap-hide-fringes nil
	minimap-dedicated-window t
	minimap-display-semantic-overlays t
	minimap-enlarge-certain-faces 'as-fallback
	minimap-normal-height-faces '(font-lock-function-name-face)
	minimap-sync-overlay-properties '(face invisible)
	minimap-major-modes '(prog-mode)
	minimap-recreate-window t
	minimap-automatically-delete-window 'visible
	minimap-tag-only nil
	minimap-highlight-line t
	minimap-disable-mode-line t
	minimap-hide-cursor t)
  :bind
  ("C-0" . (lambda ()
	     (interactive) (minimap-mode 'toggle))))

;;;; transpose-frame
(use-package transpose-frame)

;;;; ement
(use-package ement)

;;;; emms
(use-package emms
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv emms-player-vlc)
	emms-info-functions '(emms-info-native
			      emms-info-metaflac
			      emms-info-ogginfo)))

;;;; elfeed
(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  (setq elfeed-feeds
	'(("https://lwn.net/headlines/rss" news linux foss)
	  ("https://www.phoronix.com/rss.php" news linux foss reviews)
	  ("https://planet.kernel.org/rss20.xml" planet linux blog)
	  ("https://planet.lisp.org/rss20.xml" planet lisp common-lisp blog)
	  ("https://planet.scheme.org/atom.xml" planet lisp scheme blog)
	  ("https://planet.emacslife.com/atom.xml" planet emacs blog)
	  ("https://www.kernel.org/feeds/all.atom.xml" news linux kernel)
	  ("https://www.kernel.org/feeds/kdist.xml" news linux kernel)
	  ("https://rss.slashdot.org/Slashdot/slashdotLinux" news linux slashdot)))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "lwn\\.net"                  :add 'lwn))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "phoronix\\.com"             :add 'phoronix))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "kernel\\.org/feeds/all.+"   :add 'kernel-archives))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "kernel\\.org/feeds/kdist.+" :add 'kernel-release))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "slashdot\\.org"             :add 'slashdot))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "lwn.net"                    :add 'lwn)))

;;;; powershell.el
(use-package powershell)

;;;; racket-mode
(use-package racket-mode
  :config
  (require 'racket-xp)
  :hook
  (racket-hash-lang-mode . racket-xp-mode))

;;;; tree-sitter
(require 'treesit)

;;;; eglot
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))
  :hook
  (sh-mode      . eglot-ensure)
  (bash-ts-mode . eglot-ensure))

;;;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (or (memq window-system '(mac ns x))
	    (daemonp))
    (exec-path-from-shell-initialize)))

;;;; tldr
(use-package tldr
  :config
  (setq tldr-enabled-categories
	'("common"  "linux"  "sunos"
	  "freebsd" "netbsd" "openbsd"
	  "android" "osx"    "windows")))

;;;; disable-mouse
(use-package disable-mouse
  :config
  (global-disable-mouse-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trying these packages out ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; eat
(use-package eat)

;;;; avy
(use-package avy)
(elpaca-wait)

;;; ace-link
(use-package ace-link)

;; save emacs state
(require 'desktop)
(desktop-read)
(setq desktop-path (list "~/.emacs.d/desktop-save/")
      ;; set this to 0 to avoid emacs.service hitting timeout due to pdfs loading
      desktop-restore-eager 0
      desktop-auto-save-timeout 5
      desktop-load-locked-desktop t
      desktop-restore-forces-onscreen nil
      savehist-mode t
      desktop-save-mode t)
