;;; package --- ...
;;; Commentary:
;;; Code:

(require 'prelude-programming)
(require 'cython-mode)

(prelude-ensure-module-deps '(epc auto-complete jedi virtualenvwrapper pytest))

(setq HOME (file-name-as-directory (getenv "HOME")))
(setq EHOME (file-name-as-directory (concat HOME ".emacs")))


;; Fix ipython shell
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


;; Setup virtualenvwrapper
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location (expand-file-name "~/.virtualenvs/"))

;; Setup Jedi
;; Requires python packages: jedi and epc
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)

;; Setup key bindings
(setq jedi:key-goto-definition (kbd "M-."))
(setq jedi:key-goto-definition-pop-marker (kbd "M-,"))
(setq jedi:key-show-doc (kbd "M-?"))
(setq jedi:key-related-names (kbd "C-c j r"))

;; Do not add --virtual-env arg to
(setq jedi:import-python-el-settings nil)

(defun jedi-python-bin ()
  "Calculate the string used to execute the inferior Python process."
  (let ((process-environment (python-shell-calculate-process-environment))
        (exec-path (python-shell-calculate-exec-path)))
    (format "%s" (executable-find "python"))))

(defun jedi-process-per-venv ()
  (let ((cmds (list (jedi-python-bin) jedi:server-script ))
        (args nil))
    (when cmds (set (make-local-variable 'jedi:server-command) cmds))
    (when args (set (make-local-variable 'jedi:server-args) args))))


(require 'nose)

(require 'pytest)
(setq pytest-global-name "py.test")

;; Set project-venv-name in .dir-locals to automatically
;; switch to the venv name
(setq project-venv-name nil)


(defun prelude-python-mode-defaults ()
  "Python mode hook."
  (run-hooks 'prelude-prog-mode-hook)
  (subword-mode +1)
  (jedi:setup)
  (jedi-process-per-venv)
  (auto-complete-mode +1)
  (whitespace-mode +1)
  ; (electric-indent-mode -1)
  (hack-local-variables)
  (if project-venv-name
      (venv-workon project-venv-name))
  (setq nose-global-name (concat venv-current-dir "bin/nosetests"))
  (local-set-key "\C-c\C-ta" 'nosetests-all)
  (local-set-key "\C-c\C-tm" 'nosetests-module)
  (local-set-key "\C-c\C-t." 'nosetests-one)
  (local-set-key "\C-c\C-tpa" 'nosetests-pdb-all)
  (local-set-key "\C-c\C-tpm" 'nosetests-pdb-module)
  (local-set-key "\C-c\C-tp." 'nosetests-pdb-one)
  )
(setq prelude-python-mode-hook 'prelude-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'prelude-python-mode-hook)))

;;; python.el ends here
