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

;; save emacs state
(require 'desktop)
(desktop-read)
(setq desktop-path (list "~/.emacs.d/desktop-save/")
      desktop-restore-eager 0 ;emacs service times out on large files
      desktop-auto-save-timeout 5
      desktop-load-locked-desktop t
      desktop-restore-forces-onscreen nil
      savehist-mode t
      desktop-save-mode t)

;; undo history
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/"))
      undo-tree-auto-save-history t)

;; enable modus-vivendi theme
(load-theme 'modus-vivendi t)

;; enable default disabled features
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

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

;; set auth-sources to only use gpg backed sources
(setq auth-sources '("~/.authinfo.gpg"))

;; winner mode
(winner-mode)

;; eshell
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-vertically)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))

(global-set-key (kbd "C-c C-;") 'eshell-here)

(defun delete-single-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too."
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (kill-buffer)
    (if (one-window-p t)
        (delete-frame)
        (delete-window (selected-window)))))

(defun eshell/x (&rest args)
  (delete-single-window))

;; ediff mode
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Begin Packages                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; jq-mode
(use-package jq-mode
  :ensure (:wait t)
  :config
  (or (with-eval-after-load "json-mode" (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))
      (with-eval-after-load "json-ts-mode" (define-key json-ts-mode-map (kbd "C-c C-j") #'jq-interactively))))

;;;; org-mode
(use-package org
  :ensure nil
  :init
  (setq org-confirm-babel-evaluate nil
	org-babel-lisp-eval-fn #'sly-eval)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED" "CANCELED")))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((lisp   . t)
				 (shell  . t)
				 (awk    . t)
				 (jq     . t)))
  :hook
  (org-mode . (lambda ()
		(org-indent-mode)
		;;(visual-line-mode)
		(toggle-word-wrap))))

;;;; org-web-toos
(use-package org-web-tools)

;;;; pdf-tools
;; (use-package pdf-tools
;;    :config
;;    (pdf-tools-install t nil nil nil))

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

;;;; avy
(use-package avy)

;;; ace-link
(use-package ace-link
  :ensure (:wait t))

;;;; ace-window
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))

;;;; which-key
(use-package which-key
  :config
  (which-key-mode))

;;;; eterm-256color
(use-package eterm-256color)

;;;; vterm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; emacs-libvterm compiles the libvterm module in a non-blocking way, so the
;; normal =:enusre (:wait t)= doesn't work
(use-package vterm
  ;; https://github.com/progfolio/.emacs.d#vterm
  :ensure (vterm :post-build
                 (progn
				   (setq vterm-always-compile-module t)
                   (require 'vterm)
                   ;;print compilation info for elpaca
                   (with-current-buffer (get-buffer-create vterm-install-buffer-name)
                     (goto-char (point-min))
                     (while (not (eobp))
                       (message "%S"
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                       (forward-line)))
                   (when-let ((so (expand-file-name "./vterm-module.so"))
                              ((file-exists-p so)))
                     (make-symbolic-link
                      so (expand-file-name (file-name-nondirectory so)
                                           "../../builds/vterm")
                      'ok-if-already-exists))))
  :config
  ;; https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#customization
  (setq vterm-kill-buffer-on-exit t
	vterm-copy-exclude-prompt t
	vterm-max-scrollback 100000)
  :bind
  ("C-'" . vterm-send-next-key)
  :hook
  (vterm-mode . (lambda () (set
			    (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
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
	minimap-major-modes '(prog-mode 'nxml-mode)
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
(use-package transpose-frame
  :bind
  ("C-c C-c t" . transpose-frame))

;;;; ement
(use-package ement)

;;;; emms
(use-package emms
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv emms-player-vlc)
	emms-info-functions '(emms-info-native
			      emms-info-metaflac
			      emms-info-ogginfo))
  :bind
  ("C-c -" . emms-volume-mode-plus)
  ("C-c +" . emms-volume-mode-minus))

(setq browse-url-firefox-arguments '("--profile" "/home/ebeale/.mozilla/firefox/18rv2ik5.arkenfox-user.js")
      browse-url-browser-function 'browse-url-firefox)

;;;; elfeed
(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :config
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
  (add-hook 'elfeed-new-entry-hook (progn
				     (elfeed-make-tagger :feed-url "lwn\\.net"                  :add 'lwn)
				     (elfeed-make-tagger :feed-url "phoronix\\.com"             :add 'phoronix)
				     (elfeed-make-tagger :feed-url "kernel\\.org/feeds/all.+"   :add 'kernel-archives)
				     (elfeed-make-tagger :feed-url "kernel\\.org/feeds/kdist.+" :add 'kernel-release)
				     (elfeed-make-tagger :feed-url "slashdot\\.org"             :add 'slashdot))))

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
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))
  :hook
  (sh-mode      . eglot-ensure)
  (bash-ts-mode . eglot-ensure))

;;;; exec-path-from-shell
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-arguments nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Trying these packages out                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; doc-tools / toc-mode
;; needs a custom elpaca recipie
;; (use-package epc)
;; (use-package doc-tools)
;; (elpaca-wait)
;; (use-package toc-mode)

;;;; vundo
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;;;; hyperbole
(use-package hyperbole
  :config
  (hyperbole-mode 1))

;;;; helpful
(use-package helpful
  :bind
  ("C-h f"   . helpful-callable)
  ("C-h v"   . helpful-variable)
  ("C-h k"   . helpful-key)
  ("C-h x"   . helpful-command)
  ("C-c C-d" . helpful-at-point)
  ("C-h F"   . helpful-function))

;;;; terraform-mode
(use-package terraform-mode
  :custom
  (terraform-indent-level 4)
  :hook
  (terraform-mode . (lambda () (outline-minor-mode 1))))

;;;; eat
(use-package eat)

;;;; vertico
(use-package vertico
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

;;;; marginalia
(use-package marginalia
  :config
  (marginalia-mode 1))

;;;; orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

;;;; consult
(use-package consult
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))

;;;; embark
(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;;;; embark-consult
(use-package embark-consult)

;;;; wgrep
(use-package wgrep
  :bind
  ( :map grep-mode-map
    ("e"       . wgrep-change-to-wgrep-mode)
    ("C-x C-q" . wgrep-change-to-wgrep-mode)
    ("C-c C-c" . wgrep-finish-edit)))

;;;; corfu
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           End Packages                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

