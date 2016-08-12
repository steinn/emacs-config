;;; package --- ..
;;; Commentary:
;;; Code:

;; elpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(defun require-package (package)
  """Refresh package archives, check PACKAGE presence,
     install if it's not installed and load package."""
     (if (null (require package nil t))
         (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                     (progn (package-refresh-contents)
                                            package-archive-contents)
                                   package-archive-contents))
                       (AVAIL (assoc package ARCHIVES)))
                  (if AVAIL
                      (package-install package)))
                (require package))))
;;
;; Misc
;;

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable menu-bar
(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable scroll bar
(scroll-bar-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; disable tabs for indent
(setq-default indent-tabs-mode nil)

;; Require newline at end of file
(setq require-final-newline t)

;; Unbind suspend-frame binds
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; highlight the current line
(global-hl-line-mode +1)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)
(setq load-prefer-newer t)

;; .zsh file is shell script
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(defvar init-dir (file-name-directory load-file-name))
(defvar savefile-dir (expand-file-name "savefile" init-dir))

;;
;; Setup packages
;;
(require-package 'req-package)

(req-package use-package-chords
  :config (key-chord-mode 1))

(req-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(req-package helm
  :chords (("xx"  . helm-M-x))
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1)
  (setq helm-split-window-in-side-p           t
        helm-buffers-fuzzy-matching           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t))

(req-package helm-eshell
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              ;; (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
              ))
  (setq eshell-directory-name (expand-file-name "eshell" savefile-dir)))

(req-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld"
                                                         savefile-dir)))

(req-package magit
  :bind (("C-x g" . magit-status)))

(req-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing)) ;; lines-tail
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'whitespace-cleanup nil t)
              (whitespace-mode +1)))
  (add-hook 'text-mode-hook
            (lambda ()
              (whitespace-mode +1)
              (add-hook 'before-save-hook #'whitespace-cleanup nil t)
              )))

(req-package fill-column-indicator
  :config
  (setq fci-rule-column 80)
  (add-hook 'prog-mode-hook
            (lambda () (fci-mode 1)))
  (add-hook 'text-mode-hook
            (lambda () (fci-mode 1))))

(req-package ace-window
  :bind (("M-p" . ace-window)))

(req-package winner
  :config
  (winner-mode +1))

(req-package windmove
  :bind (("<S-left>"  . windmove-left)
         ("<S-right>" . windmove-right)
         ("<S-up>"    . windmove-up)
         ("<S-down>"  . windmove-down)))

(req-package autorevert
  :config
  (global-auto-revert-mode t))


(req-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (add-hook 'prog-mode-hook
            (lambda () (smartparens-mode +1))))

(req-package rainbow-delimiters
  ;; color highlight parenthesis according to depth
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)     ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

(req-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  (setq-default save-place t))

(req-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

(req-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" prelude-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; TODO: enable flyspell
(req-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")))

(req-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" prelude-savefile-dir)
        bookmark-save-flag 1))

(req-package undo-tree
  :diminish undo-tree-mode
  :chords (("uu" . undo-tree-visualize))
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(req-package scala-mode
  :config
  (add-hook 'scala-mode-hook
            (lambda ()
              (subword-mode +1))))

(req-package which-func
  :config
  (which-function-mode 1))

(req-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

(req-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(req-package imenu
  :config
  (set-default 'imenu-auto-rescan t))

(req-package-finish)


(find-file "~/.emacs.d/init.el")

;;; init-new.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (volatile-highlights flycheck scala-mode undo-tree rainbow-delimiters smartparens ace-window fill-column-indicator magit projectile helm zenburn-theme use-package-chords req-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
