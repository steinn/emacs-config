;;; package --- ..
;;; Commentary:
;;; Code:

;; elpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)


;; setup quelpa
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

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

;; disable tool-bar
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

;; don't create lock files (.#<filename>)
(setq create-lockfiles nil)

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

;; disable vc
(require 'vc)
(setq vc-handled-backends ())

(setq confirm-kill-emacs 'y-or-n-p)

;; (global-set-key (kbd "C-+") 'text-scale-increase)
;; (global-set-key (kbd "C--") 'text-scale-decrease)


(defvar init-dir (file-name-directory load-file-name))
(defvar savefile-dir (expand-file-name "savefile" init-dir))
(defvar vendor-dir (expand-file-name "vendor" init-dir))

(setq custom-file (expand-file-name "custom.el" init-dir))

;; (setq exec-path (append exec-path '(expand-file-name "~/.local/bin")))
;; (setq exec-path (append exec-path '(expand-file-name "~/.opam/4.02.3/bin/")))
;; (setq exec-path (append exec-path '(expand-file-name "~/.dotfiles/bin/")))
;; (setq exec-path (append exec-path '(expand-file-name "/usr/local/bin/")))


(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;
;; Setup packages
;;
(require-package 'req-package)

(req-package key-chord)

(req-package use-package-chords
  :require key-chord
  :config (key-chord-mode 1))

(req-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(req-package smart-mode-line
  :require zenburn-theme
  :config
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful
        sml/col-number-format "%c")
  (add-hook 'after-init-hook #'sml/setup))

(req-package helm
  :diminish helm-mode
  :chords (("xx"  . helm-M-x))
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("M-i" . helm-semantic-or-imenu))
  :config
  (helm-mode 1)
  (setq helm-split-window-in-side-p           t
        helm-buffers-fuzzy-matching           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t))

(req-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (add-to-list 'exec-path-from-shell-variables "ANDROID_HOME")
    (exec-path-from-shell-initialize)))

(req-package helm-eshell
  :require helm
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))
  (setq eshell-directory-name (expand-file-name "eshell" savefile-dir)))

(req-package eshell-z)

(req-package helm-flycheck
  :require helm
  :config
  (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(req-package projectile
  :diminish projectile-mode
  :bind (("<f1>" . projectile-run-eshell))
  :demand
  :init
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.cache" savefile-dir))
  :config
  (projectile-mode))

(req-package helm-projectile
  :require projectile
  :config
  (message "HELM-PROJECTILE")
  (helm-projectile-on))

(req-package helm-ag)

(req-package magit
  :bind (("C-x g" . magit-status)))

(req-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face tabs empty trailing)) ;; lines-tail
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
  :bind (("M-o" . ace-window)))

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
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-smartparens-bindings)
  ;; (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (add-hook 'prog-mode-hook
            (lambda () (smartparens-mode +1))))

(req-package rainbow-delimiters
  ;; color highlight parenthesis according to depth
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t     ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

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
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(req-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
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

(req-package thrift)
(req-package scala-mode
  :config
  (add-hook 'scala-mode-hook
            (lambda ()
              (subword-mode +1))))

(req-package which-func
  :config
  (which-function-mode 1))

(req-package flyspell
  :diminish flyspell-mode
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")
        flyspell-issue-message-flag nil)
  ;; enable flyspell-prog-mode
  (dolist (hook '(prog-mode-hook))
    (add-hook hook (lambda ()
                     (when (executable-find ispell-program-name)
                       (flyspell-prog-mode)))))
  ;; enable flyspell-mode
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda ()
                     (when (executable-find ispell-program-name)
                       (flyspell-mode +1))))))

(req-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

(req-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(req-package imenu
  :config
  (set-default 'imenu-auto-rescan t))

(req-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "yasnippet-snippets" vendor-dir)))

(req-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  (which-key-mode +1))

(req-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq mode-name "elisp"))))

(req-package elisp-slime-nav
  :require ielm
  :diminish elisp-slime-nav-mode
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(req-package autorevert
  :diminish auto-revert-mode)

(req-package smartscan
  ;; M-n - smartscan-symbol-go-forward
  ;; M-p - smartscan-symbol-go-backward
  ;; M-' - smartscan-symbol-replace
  :config
  ;; (global-smartscan-mode)
  (add-hook 'prog-mode-hook
            (lambda () (smartscan-mode 1))))

(req-package org
  :demand
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     ))
  )

;; python
(req-package python-mode
  :config
  (addv-to-list 'auto-mode-alist '("BUILD" . python-mode)))
;; (req-package jedi
;;   :config
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t))
;; (req-package virtualenvwrapper
;;   :config
;;   (venv-initialize-interactive-shells)
;;   (venv-initialize-eshell))

;; javascript
;; (req-package flycheck-flow
;;   :config
;;   (flycheck-add-mode 'javascript-flow 'js2-jsx-mode))
(req-package js2-mode
  :require flycheck
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
  (setq js-indent-level 2))

(req-package prettier-js
  :require js2-mode
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (setq prettier-js-args '("--single-quote"
                           "--trailing-comma" "es5"
                           "--bracket-spacing" "false"
                           "--parser" "flow")))

(req-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(req-package terraform-mode)

(req-package yaml-mode)

(req-package go-mode
  :config
  (add-hook 'go-mode-hook (lambda () (whitespace-mode -1))))

(req-package make-mode
  :config
  (add-hook 'makefile-mode-hook (lambda () (whitespace-mode -1))))

(req-package rust-mode)
(req-package lua-mode)
(req-package protobuf-mode)

(req-package markdown-mode)

;; ocaml packages
(req-package tuareg)
(req-package utop)
(req-package merlin)

;; org-mode presentation stuff
(req-package epresent
  :config
  ;; (setq epresent-mode-line nil)
  (add-hook 'epresent-start-presentation-hook (lambda ()
                                                (whitespace-mode -1)
                                                (fci-mode -1))))

(req-package zoom-frm
  :config
  (global-set-key (kbd "C-+") 'zoom-frm-in)
  (global-set-key (kbd "C--") 'zoom-frm-out))

(req-package-finish)

(quelpa '(reason-mode :repo "reasonml-editor/reason-mode" :fetcher github :stable t))
(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))

(setq merlin-ac-setup t)

(find-file "~/.emacs.d/init.el")
