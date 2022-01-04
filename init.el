;;; package --- ..
;;; Commentary:
;;; Code:
;; elpa
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

;; emacs server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

(defun require-package (package)
  "Refresh package archives, check PACKAGE presence, install if it's not installed and load package."
     (if (null (require package nil t))
         (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                     (progn (package-refresh-contents)
                                            package-archive-contents)
                                   package-archive-contents))
                       (AVAIL (assoc package ARCHIVES)))
                  (if AVAIL
                      (package-install package)))
                (require package))))

;; setup req-package
(require-package 'use-package)
(require 'use-package)
(setq use-package-always-ensure t)

(require-package 'req-package)
(use-package use-package-chords
  :ensure t)
(use-package req-package
  :ensure t
  :config (req-package--log-set-level 'debug))


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
(setq require-final-newline nil)

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

(defvar init-dir (file-name-directory load-file-name))
(defvar savefile-dir (expand-file-name "savefile" init-dir))
(defvar vendor-dir (expand-file-name "vendor" init-dir))

(setq custom-file (expand-file-name "custom.el" init-dir))

;;
;; Setup packages
;;

(req-package key-chord
  :config
  (key-chord-mode 1))

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

(req-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (add-to-list 'exec-path-from-shell-variables "ANDROID_HOME")
    (exec-path-from-shell-initialize)))

;; (req-package xterm-color
;;   :config
;;   (setq comint-output-filter-functions
;;         (remove 'ansi-color-process-output comint-output-filter-functions))
;;   (add-hook 'shell-mode-hook
;;             (lambda ()
;;               ;; Disable font-locking in this buffer to improve performance
;;               (font-lock-mode -1)
;;               ;; Prevent font-locking from being re-enabled in this buffer
;;               (make-local-variable 'font-lock-function)
;;               (setq font-lock-function (lambda (_) nil))
;;               (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))))

;; (req-package eshell
;;   :require xterm-color
;;   :config
;;   (add-hook 'eshell-before-prompt-hook
;;             (lambda ()
;;               (setq xterm-color-preserve-properties t)))
;;   (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;;   (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
;;   (setenv "TERM" "xterm-256color")
;;   )

(req-package eshell-z)
(req-package flx-ido)
(req-package ag)

(req-package projectile
  :diminish projectile-mode
  :require flx-ido
  :bind (("<f1>" . projectile-run-eshell))
  :demand
  :init
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.cache" savefile-dir))
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-globally-ignored-file-suffixes '("*.meta" "*.bs.js"))
  (setq projectile-globally-ignored-directories '(
                                                 ".idea" ".ensime_cache" ".eunit"
                                                 ".git" ".hg" ".fslckout" "_FOSSIL_"
                                                 ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
                                                 "node_modules"))
  )

(req-package magit
  :bind (("C-x g" . magit-status)))
(req-package forge)

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

(req-package ace-window
  :bind (("M-o" . ace-window)))

(req-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode +1)
  (add-hook 'prog-mode-hook
            (lambda () (smartparens-mode +1))))

(req-package rainbow-delimiters
  ;; color highlight parenthesis according to depth
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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

(req-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t))


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
(req-package python-mode)


(req-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(req-package yaml-mode)
;; (req-package markdown-mode)
;; (req-package graphviz-dot-mode)
;; (req-package csharp-mode)
;; (req-package omnisharp
;;   :require company flycheck
;;   :config
;;   (add-hook 'csharp-mode-hook 'omnisharp-mode)
;;   (add-to-list 'company-backends 'company-omnisharp)
;;   (define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
;;   (define-key omnisharp-mode-map (kbd "<M-SPC>") 'omnisharp-auto-complete)
;;   (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
;;   (setq c-basic-offset 2))

(req-package modern-cpp-font-lock
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(req-package clang-format
  :config
  (add-hook 'c++-mode-hook (lambda ()
                             (fset 'c-indent-region 'clang-format-region))))

(req-package editorconfig
  :config
  (editorconfig-mode 1))

(req-package rjsx-mode)
(req-package prettier
  :config
  (add-hook 'js2-mode-hook 'prettier-mode))

(req-package tide
  :require prettier
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
    (prettier-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; don't use tide formatting, use prettier to format
  ;; (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(req-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))


(req-package json-mode)

(defun ivy-open-json-file-action (file)
  "Read a file, FILE, as json in temp buffer."
  (progn
    (switch-to-buffer (make-temp-name "json"))
    (json-mode)
    (let ((json-encoding-pretty-print t))
    (insert (json-encode (json-read-file file))))))

(req-package counsel
  :chords (("xx"  . counsel-M-x))
  :bind (("M-x" . counsel-M-x)
         ("C-c r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         ("C-x C-b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("M-i" . counsel-semantic-or-imenu)
         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h s" . counsel-descbinds)
         ("C-c c" . counsel-compile)
         ("C-'" . avy-goto-line))
  :config
  (setq ivy-use-virtual-buffers t)
  ;;(setq ivy-count-format "(%d/%d) ")
  (setq ivy-count-format "")
  (define-key swiper-map (kbd "C-r") 'ivy-previous-line)
  (ivy-set-actions 'counsel-find-file '(("j" ivy-open-json-file-action "json"))))

(req-package counsel-projectile
  :require counsel projectile
  :config
  (counsel-projectile-mode)
  (setq projectile-completion-system 'ivy))

(req-package glsl-mode)

(req-package company
  :config
  (global-company-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(req-package ediff
  :config
  (defvar my-ediff-last-windows nil)

  (defun my-store-pre-ediff-winconfig ()
    (setq my-ediff-last-windows (current-window-configuration)))

  (defun my-restore-pre-ediff-winconfig ()
    (set-window-configuration my-ediff-last-windows))

  (add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig))

(req-package-finish)

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)
(global-set-key (kbd "C-c C-c") 'compile)

(find-file "~/.emacs.d/init.el")


(defun steinn/paste-json ()
  "Foobar."
  (interactive)
  (let ((json-encoding-pretty-print t))
    (insert (json-encode (json-read-from-string (substring-no-properties (current-kill 0)))))))

(defun steinn/clear-buffer-and-paste-json ()
  "Foobar."
  (interactive)
  (let ((json-encoding-pretty-print t))
    (erase-buffer)
    (insert (json-encode (json-read-from-string (substring-no-properties (current-kill 0)))))))

(provide 'init)

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
