;; -*- lexical-binding: t -*-

(require 'org)
(require 'org-agenda)
(require 'org-capture)

(defun cr-gtd-expand (file)
  (expand-file-name file cr-gtd-dir))

(setq org-agenda-files
      (append org-agenda-files
              `(,(cr-gtd-expand "inbox.org")
                ,(cr-gtd-expand "todos.org")
                ,(cr-gtd-expand "calendar.org")
                ,(cr-gtd-expand "tickler.org")
                ,(cr-gtd-expand "journal.org.gpg"))))

(add-to-list 'org-agenda-text-search-extra-files (cr-gtd-expand "someday.org"))
(add-to-list 'org-refile-targets `(,(cr-gtd-expand "someday.org") :maxlevel 1))

(setq org-capture-templates
      (append
       org-capture-templates
       `(("i" "Inbox" entry
          (file ,(cr-gtd-expand "inbox.org"))
          "* %i%?\n%u\n")

         ("m" "Meeting" entry
          (file ,(cr-gtd-expand "inbox.org"))
          "* Meeting with %? :meeting:"
          :clock-in t)

         ("e" "Event" entry
          (file+headline ,(cr-gtd-expand "calendar.org") "Events")
          "* %i%?\n")

         ("j" "Journal" entry
          (file+olp+datetree ,(cr-gtd-expand "journal.org.gpg"))
          "* %^{prompt|journal-entry}\n%U\n%?")

         ("r" "Review templates")

         ("rd" "Daily Review" entry
          (file+olp+datetree ,(cr-gtd-expand "journal.org.gpg"))
          (file "templates/daily-review.org")
          :immediate-finish t
          :jump-to-captured t)

         ("rw" "Weekly Review" entry
          (file+olp+datetree ,(cr-gtd-expand "journal.org.gpg"))
          (file "templates/weekly-review.org")
          :immediate-finish t
          :jump-to-captured t)

         ("rm" "Monthly Review" entry
          (file+olp+datetree ,(cr-gtd-expand "journal.org.gpg"))
          (file "templates/monthly-review.org")
          :immediate-finish t
          :jump-to-captured t))))

(setq org-agenda-custom-commands
      (append
       org-agenda-custom-commands
       `(("i" "Inbox" tags "*"
          ((org-agenda-files (list ,(cr-gtd-expand "inbox.org")))))

         ("p" "Personal"
          ((agenda "")
           (tags "project")
           (stuck "")
           (todo "TODO|WAITING"))
          ((org-agenda-tag-filter '("-@work")))
          (,(cr-gtd-expand "exports/agenda-week.pdf")))

         ("r" "Weekly Review"
          ((agenda ""
                   ((org-agenda-span 8)
                    (org-agenda-start-day "-1w")
                    (org-agenda-show-log t)
                    (org-agenda-start-with-log-mode t)
                    (org-agenda-archives-mode t)))
           (tags "+project/DONE")
           (tags "-project/+DONE|+DELEGATED|+CANCELED")))

         ("l" "Log"
          ((agenda ""
                   ((org-agenda-show-log t)
                    (org-agenda-start-with-log-mode t)
                    (org-agenda-archives-mode t)
                    (org-agenda-include-inactive-timestamps t)))))

         ("g" . "GTD contexts")
         ("gh" "@Home"    tags-todo "@home")
         ("ge" "@Errands" tags-todo "@errands" nil (,(cr-gtd-expand "exports/agenda-errands.pdf")))
         ("go" "@Offline" tags-todo "@offline")
         ("gw" "@Work"    tags-todo "@work"))))

(defun cr-org-store-agenda-views ()
  "Export the agenda views to pdf.
Proceeds only if variable `org-agenda-files' contain files that
are more recent than the last export."
  (interactive)
  (require 'f)
  (let ((default-directory (expand-file-name "exports" cr-org-dir))
        (last-export-time-file ".last-export-time"))
    (when (catch 'require-update
            (dolist (agenda-file (org-agenda-files))
              (when (file-newer-than-file-p agenda-file last-export-time-file)
                (throw 'require-update t))))
      (progn
        (org-store-agenda-views)
        (f-touch last-export-time-file)))))

(run-with-idle-timer 180 t 'cr-org-store-agenda-views)

(provide 'cr-gtd-personal)
