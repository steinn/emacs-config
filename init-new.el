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
  """refresh package archives, check package presence,
     install if it's not installed and load package"""
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
              )))

(req-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm))

(req-package magit
  :bind (("C-x g" . magit-status)))

(req-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)
              (whitespace-mode +1)))
  (add-hook 'text-mode-hook
            (lambda ()
              (whitespace-mode +1)
              (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)
              )))

(req-package-finish)


(find-file "~/.emacs.d/init-new.el")
