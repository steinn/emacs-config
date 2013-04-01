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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Load prelude         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load prelude-init-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Post prelude configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install packages
;; (prelude-ensure-module-deps '(...))

;; Add all libraries from vendor folder to load path
(add-subfolders-to-load-path vendor-dir 'load-path)

;; Add all themes from themes folder to custom-theme-load-path
(add-subfolders-to-load-path themes-dir 'custom-theme-load-path)

;; Load color theme
(disable-theme 'zenburn)
(load-theme 'solarized-dark t)

;; Resize fringe back to normal size
(fringe-mode 8)

;; Remove scroll bar
(scroll-bar-mode -1)

;; Load init.el after loading emacs
(find-file load-file-name)

;;; init.el ends here
