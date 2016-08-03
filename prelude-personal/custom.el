(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(background-color "#fdf6e3")
 '(background-mode light)
 '(cursor-color "#657b83")
 '(custom-safe-themes
   (quote
    ("1da9fccb3a233276317221adadeaaa6e12bdcce886062e47d473a7bb072ba3d9" "d8b7af879ad19616a8d1334478a14fc5deeabecd091943806a75a644772f2862" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(eclim-eclipse-dirs (quote ("~/java/eclipse")))
 '(eclim-executable "~/java/eclipse/eclim")
 '(ediff-merge-split-window-function (quote split-window-vertically))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ensime-server-logback "\"/home/steinn/ensime-server.log\"")
 '(fci-rule-color "#383838")
 '(foreground-color "#657b83")
 '(helm-ff-skip-boring-files t)
 '(magit-revert-buffers t t)
 '(org-agenda-files
   (quote
    ("/home/steinn/org/refile.org" "/home/steinn/org/matur.org" "~/org/work" "/home/steinn/org/personal/binni_api.org" "/home/steinn/org/personal/books.org" "/home/steinn/org/personal/bustadur.org" "/home/steinn/org/personal/contacts.org" "/home/steinn/org/personal/cs.org" "/home/steinn/org/personal/display.org" "/home/steinn/org/personal/doctor.org" "/home/steinn/org/personal/emacs.org" "/home/steinn/org/personal/erna.org" "/home/steinn/org/personal/fedora.org" "/home/steinn/org/personal/fluttningur.org" "/home/steinn/org/personal/gary.org" "/home/steinn/org/personal/gunni.org" "/home/steinn/org/personal/ideas.org" "/home/steinn/org/personal/info.org" "/home/steinn/org/personal/leaving_us.org" "/home/steinn/org/personal/macosx.org" "/home/steinn/org/personal/misc.org" "/home/steinn/org/personal/music.org" "/home/steinn/org/personal/pesi.org" "/home/steinn/org/personal/pydata2014.org" "/home/steinn/org/personal/reading.org" "/home/steinn/org/personal/refile.org" "/home/steinn/org/personal/research.org" "/home/steinn/org/personal/router.org" "/home/steinn/org/personal/slack.org" "/home/steinn/org/personal/ssh.org" "/home/steinn/org/personal/tryggingar.org" "/home/steinn/org/personal/vicki.org")))
 '(org-confirm-babel-evaluate nil)
 '(org-link-frame-setup
   (quote
    ((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame))))
 '(package-selected-packages
   (quote
    (zop-to-char zenburn-theme yari yaml-mode window-numbering which-key volatile-highlights virtualenvwrapper undo-tree tle thrift smex smartrep smartparens smart-mode-line slime scala-mode2 ruby-tools rainbow-mode rainbow-delimiters protobuf-mode php-mode ov org-jira operate-on-number ob-ipython nginx-mode move-text markdown-mode magit-gh-pulls lua-mode key-chord json-mode jedi-direx inf-ruby imenu-anywhere ido-ubiquitous helm-projectile helm-descbinds helm-ag guru-mode groovy-mode grizzl gotest god-mode go-projectile gitignore-mode gitconfig-mode git-timemachine gist geiser flycheck flx-ido expand-region etags-select eshell-z ensime elisp-slime-nav easy-kill dockerfile-mode docker discover-my-major diminish diff-hl csv-mode csharp-mode crux company-go cmake-mode browse-kill-ring beacon anzu ack-and-a-half ace-window ace-jump-mode ace-jump-buffer)))
 '(safe-local-variable-values
   (quote
    ((encoding . utf-8)
     (project-venv-name . "mailpile")
     (project-venv-name . "birdcore")
     (virtualenv-default-directory . "/home/steinn/work/birdcore")
     (python-run-file . "/home/steinn/work/birdcore/special/steinn/emacs_shell.py")
     (virtualenv-workon . "birdcore"))))
 '(scala-indent:align-parameters nil)
 '(scala-indent:default-run-on-strategy 2)
 '(scala-indent:indent-value-expression nil)
 '(split-height-threshold nil)
 '(split-width-threshold 190)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ensime-implicit-highlight ((t nil)))
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))
