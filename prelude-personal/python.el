;;; package --- ...
;;; Commentary:
;;; Code:

(require 'prelude-programming)

(prelude-ensure-module-deps '(epc auto-complete jedi virtualenv))

(autoload 'python-mode "python" "Python Mode." t)
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


;; Setup jedi (auto-completion)
(setq jedi:server-args
      (quote
       ("--sys-path"
        "/home/steinn/work/birdcore")))
(setq jedi:server-command
      (quote
       ("python2"
        "/home/steinn/.emacs.d/vendor/prelude/elpa/jedi-20130621.2057/jediepcserver.py"
        )))
(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)

(defun prelude-python-mode-defaults ()
  "Python mode hook."
  (run-hooks 'prelude-prog-mode-hook)
  (jedi:setup)
  (auto-complete-mode +1)
  (whitespace-mode +1)
  (virtualenv-minor-mode +1)
  (electric-indent-mode -1)
  (which-function-mode -1)
  )

(setq prelude-python-mode-hook 'prelude-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'prelude-python-mode-hook)))

;;; python.el ends here
