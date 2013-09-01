(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

                                        ; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
