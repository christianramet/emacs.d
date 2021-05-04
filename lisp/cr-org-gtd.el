(require 'org)
(require 'org-agenda)

(defun cr-org-expand (file)
  (expand-file-name file org-directory))

(custom-set-variables
 '(org-directory "~/nextcloud/org/")

 '(org-agenda-files (list (cr-org-expand "journal.org")
                          (cr-org-expand "todos.org")
                          (cr-org-expand "work-todos.org")))

 '(org-agenda-text-search-extra-files
   (append
    (list 'agenda-archives
          (cr-org-expand "inbox.org")
          (cr-org-expand "someday.org"))))

 '(org-refile-targets
   `((nil                                :maxlevel . 5)
     (,(cr-org-expand "inbox.org")       :level    . 0)
     (,(cr-org-expand "someday.org")     :maxlevel . 1)
     (,(cr-org-expand "todos.org")       :maxlevel . 2)
     (,(cr-org-expand "todos-work.org")  :maxlevel . 2)))

 '(org-capture-templates
   '(("i" "Inbox" entry
      (file "inbox.org")
      "* %i%?\n%u\n"
      :empty-lines 1)

     ("m" "Meeting" entry
      (file "inbox.org")
      "* Meeting with %? :meeting:"
      :empty-lines 1
      :clock-in t)

     ("j" "Journal" entry
      (file+olp+datetree "journal.org")
      "* %?\n%U\n"
      :empty-lines 1)

     ("r" "Review templates")

     ("rd" "Daily Review" entry
      (file+olp+datetree "journal.org")
      (file "templates/daily-review.org")
      :empty-lines 1
      :immediate-finish t
      :jump-to-captured t)

     ("rw" "Weekly Review" entry
      (file+olp+datetree "journal.org")
      (file "templates/weekly-review.org")
      :empty-lines 1
      :immediate-finish t
      :jump-to-captured t)

     ("rm" "Monthly Review" entry
      (file+olp+datetree "journal.org")
      (file "templates/monthly-review.org")
      :empty-lines 1
      :immediate-finish t
      :jump-to-captured t)))

 '(org-agenda-custom-commands
   `(("i" "Inbox" tags "*"
      ((org-agenda-files (list (cr-org-expand "inbox.org")))))

     ("p" "Personal"
      ((agenda "")
       (tags "project")
       (stuck "")
       (todo "TODO|WAITING"))
      ((org-agenda-tag-filter '("-@work")))
      ("agenda-week.html" "agenda-week.pdf"))

     ("w" "Work"
      ((agenda "")
       (tags "project")
       (stuck "")
       (todo "TODO|WAITING"))
      ((org-agenda-tag-filter '("+@work"))))

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

     ("y" "Year"
      ((agenda ""
               ((org-agenda-span 'year)
                (org-agenda-start-day "-1w"))))
      ((org-agenda-category-filter-preset '("-tickler")))
      ("agenda-year.html" "agenda-year.pdf"))

     ("g" . "GTD contexts")
     ("gh" "@Home"    tags-todo "@home")
     ("ge" "@Errands" tags-todo "@errands" nil ("agenda-errands.html" "agenda-errands.pdf"))
     ("go" "@Offline" tags-todo "@offline")
     ("gw" "@Work"    tags-todo "@work")))

 '(org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "|" "DELEGATED(g@/!)" "CANCELED(c@/!)")))

 '(org-tag-alist
   '((:startgroup . nil) ;; Context
     ("@home"     . ?h)
     ("@work"     . ?w)
     ("@errands"  . ?e)
     ("@offline"  . ?o)
     (:endgroup   . nil)
     (:startgroup . nil) ;; Energy
     ("focus"     . ?f)
     ("casual"    . ?c)
     ("fuzzy"     . ?z)
     (:endgroup   . nil)
     (:startgroup . nil) ;; Type
     ("project"   . ?p)
     ("meeting"   . ?m)
     (:endgroup   . nil)))

 '(org-tags-exclude-from-inheritance '("project"))
 '(org-stuck-projects '("project" ("TODO" "WAITING") nil "")))

(defun cr-org-store-agenda-views ()
  "Export the agenda views, only if org-agenda-files are more
recent than the last export."
  (interactive)
  (require 'f)
  (let ((default-directory (cr-org-expand "exports"))
        (last-export-time-file ".last-export-time"))
    (when (catch 'require-update
      (dolist (agenda-file org-agenda-files)
        (when (file-newer-than-file-p agenda-file last-export-time-file)
          (throw 'require-update t))))
      (progn
        (org-store-agenda-views)
        (f-touch last-export-time-file)))))

(run-with-idle-timer 180 t 'cr-org-store-agenda-views)

(provide 'cr-org-gtd)
