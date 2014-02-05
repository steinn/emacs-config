(setq org-agenda-files (list "~/org/"
                             "~/org/work"
                             "~/org/personal"))

(setq org-default-notes-file "~/org/notes.org")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/refile.org" "All TODO")
         "* TODO %?\n  %i\n  %a")
        ("m" "Music" entry (file+headline "~/org/music.org" "New")
         "* TODO %?\n  %i\n  %a")))

(global-set-key (kbd "C-c c") 'org-capture)

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)

; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)
