;; load the welcome screen for encouragement
(setq inhibit-startup-screen nil)
;; disable menu bar
(menu-bar-mode -1)
;; disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)
;; allow `C-x C-l`
(put 'downcase-region 'disabled nil)
;; allow `C-x C-u
(put 'upcase-region 'disabled nil)
;; slime
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl --noinform --no-linedit")
;; set re-builder syntax
(require 're-builder)
(setq reb-re-syntax 'string)
;; set line and column number modes on
(global-display-line-numbers-mode)
(setq column-number-mode t)
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

