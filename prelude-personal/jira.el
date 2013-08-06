;;; package --- ...
;;; Commentary:
;;; Code:

(prelude-ensure-module-deps '(org-jira))

(setq jiralib-url "https://jira.jivesoftware.com")
(setq org-jira-working-dir (expand-file-name "~/org/jira"))

(require 'org-jira)
