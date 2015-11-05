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



;; (eval-after-load 'eclim-mode
;;   '(define-key eclim-mode-map (kbd "M-.") 'eclim-java-find-declaration))

(require 'eclim)
(require 'eclim-java)
(require 'eclim-maven)
(global-eclim-mode)
(custom-set-variables
 '(eclim-eclipse-dirs '("~/java/eclipse"))
 '(eclim-executable "~/java/eclipse/eclim"))

(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)


(defvar eclim--marker-ring-length 16
  "Length of marker ring for eclim navigation.")

(defvar eclim--marker-ring
  (make-ring eclim--marker-ring-length)
  "Marker ring that stores eclim navigation.")

(defun eclim-navigation-push-marker ()
  "Push point onto navigation marker ring."
  (ring-insert eclim--marker-ring (point-marker)))

(defun eclim-navigation-pop-marker ()
  "Goto previous point in navigation marker ring."
  (interactive)
  (if (ring-empty-p eclim--marker-ring)
      (error "Eclim marker ring is empty, can't pop")
    (let ((marker (ring-remove eclim--marker-ring 0)))
      (switch-to-buffer (or (marker-buffer marker)
                            (error "Buffer has been deleted")))
      (goto-char (marker-position marker))
      ;; Cleanup the marker so as to avoid them piling up.
      (set-marker marker nil nil))))


(defadvice eclim-java-find-declaration (before goto-marker activate)
  "Find declartion and push marker."
  (eclim-navigation-push-marker))

(define-key eclim-mode-map (kbd "M-.") 'eclim-java-find-declaration)
(define-key eclim-mode-map (kbd "M-,") 'eclim-navigation-pop-marker)
(define-key eclim-mode-map (kbd "M-?")
  'eclim-java-show-documentation-for-current-element)
(define-key eclim-mode-map (kbd "C-c C-e m") 'eclim-run-class)


(eval-after-load "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
(add-hook 'jedi-mode-hook 'jedi-direx:setup)


(require 'ob-ipython)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(setq org-src-fontify-natively t)

(add-to-list 'org-structure-template-alist
             '("p" "#+BEGIN_SRC ipython :session ?\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
(add-to-list 'org-structure-template-alist
             '("P" "#+BEGIN_SRC ipython :session :file /tmp/tmp.png :exports both ?\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))

(setq-default major-mode 'org-mode)

;; Load init.el after loading emacs
(find-file "~/org/refile.org")

;;; init.el ends here
