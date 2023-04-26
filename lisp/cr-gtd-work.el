;; -*- lexical-binding: t -*-

(require 'org)
(require 'org-agenda)
(require 'org-capture)

(defun cr-gtd-work-expand (file)
  (expand-file-name file cr-gtd-dir-work))

(setq org-agenda-files
      (append org-agenda-files
              `(,(cr-gtd-work-expand "inbox.org")
                ,(cr-gtd-work-expand "todos.org")
                ,(cr-gtd-work-expand "calendar.org")
                ,(cr-gtd-work-expand "tickler.org")
                ,(cr-gtd-work-expand "journal.org"))))

(add-to-list 'org-agenda-text-search-extra-files (cr-gtd-work-expand "someday.org"))
(add-to-list 'org-refile-targets `(,(cr-gtd-work-expand "someday.org") :maxlevel 1))

(setq org-capture-templates
      (append
       org-capture-templates
       `(("w" "Work")

         ("wi" "Inbox" entry
          (file ,(cr-gtd-work-expand "inbox.org"))
          "* %i%?\n%u\n")

         ("wm" "Meeting" entry
          (file ,(cr-gtd-work-expand "inbox.org"))
          "* Meeting with %? :meeting:"
          :clock-in t)

         ("we" "Event" entry
          (file+headline ,(cr-gtd-work-expand "calendar.org") "Events")
          "* %i%?\n")

         ("wj" "Journal" entry
          (file+olp+datetree ,(cr-gtd-work-expand "journal.org"))
          "* %^{prompt|journal-entry}\n%U\n%?"))))

(setq org-agenda-custom-commands
      (append
       org-agenda-custom-commands
       '(("w" "Work"
          ((agenda "")
           (tags "project")
           (stuck "")
           (todo "TODO|WAITING"))
          ((org-agenda-tag-filter '("+@work")))))))

(provide 'cr-gtd-work)
