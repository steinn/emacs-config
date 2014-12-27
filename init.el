;;; package --- ..
;;; Commentary:
;;; Code:

(setq root-config-dir (file-name-directory load-file-name))
(setq vendor-dir (expand-file-name "vendor" root-config-dir))
(setq themes-dir (expand-file-name "themes" root-config-dir))

(defun add-subfolders-to-load-path (parent-dir list-to-add)
  "Add all first level PARENT-DIR subdirs to the list LIST-TO-ADD."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list list-to-add name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      configure prelude      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq prelude-dir (expand-file-name "prelude" vendor-dir))
(setq prelude-init-file (expand-file-name "init.el" prelude-dir))
(setq prelude-personal-dir (expand-file-name "prelude-personal"
                                             root-config-dir))
(setq prelude-modules-file (expand-file-name "prelude-modules.el"
                                             root-config-dir))

(add-to-list 'load-path vendor-dir)

(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" .
;;                "http://marmalade-repo.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Load prelude         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load prelude-init-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Post prelude configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install packages
(prelude-ensure-module-deps '(smart-mode-line))

;; Add all libraries from vendor folder to load path
(add-subfolders-to-load-path vendor-dir 'load-path)

;; Add all themes from themes folder to custom-theme-load-path
(add-subfolders-to-load-path themes-dir 'custom-theme-load-path)

(setq ring-bell-function 'ignore)
(require 'prelude-helm-everywhere)

;; Load color theme
;(disable-theme 'zenburn)
;(load-theme 'solarized-dark t)

;; Resize fringe back to normal size
(fringe-mode 8)

;; Remove scroll bar
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))


(require 'smart-mode-line)
(add-hook 'after-init-hook 'sml/setup)
(sml/setup)
(add-to-list 'sml/replacer-regexp-list '("^~/resonata/" ":RSNT:"))
(add-to-list 'sml/replacer-regexp-list '("^~/.dotfiles/" ":DOT:"))
(add-to-list 'sml/replacer-regexp-list '("^/ssh:\\(.*\\):" ":SSH:\\1:"))

;; (require 'magit-gh-pulls)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; Clean modeline even more
;; (diminish 'prelude-mode)
;; (diminish 'guru-mode)
;; (diminish 'flyspell-mode)
;; (diminish 'whitespace-mode)
;; (diminish 'yas-minor-mode)

(setq browse-url-chromium-program "google-chrome"
      browse-url-firefox-program "")

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Confirm on exit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Modify PATH and exec-path so that emacs will find extra binaries
(expand-file-name "~/.virtualenvs/")
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
(add-to-list 'exec-path (expand-file-name "~/local/bin"))
(add-to-list 'exec-path (expand-file-name "~/.dotfiles/bin"))
(setenv "PATH"
        (concat (expand-file-name "~/.local/bin") ";"
                (expand-file-name "~/local/bin") ";"
                (expand-file-name "~/.dotfiles/bin") ";"
                (getenv "PATH")))

;; Load init.el after loading emacs
(find-file "~/org/refile.org")

;;; init.el ends here
