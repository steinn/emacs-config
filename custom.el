(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme yasnippet yaml-mode which-key volatile-highlights use-package-chords undo-tree smartscan smartparens smart-mode-line req-package reason-mode rainbow-delimiters quelpa python-mode projectile prettier-js omnisharp multiple-cursors modern-cpp-font-lock markdown-mode magit js2-mode helm-flycheck helm-ag graphviz-dot-mode flx-ido exec-path-from-shell eshell-z elisp-slime-nav company clang-format ag ace-window)))
 '(safe-local-variable-values
   (quote
    ((eval setq-local flycheck-clang-include-path
           (list
            (expand-file-name
             (concat
              (locate-dominating-file default-directory ".dir-locals.el")
              "protobuf-2.5.0/src"))))
     (flycheck-clang-language-standard . c++11)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
