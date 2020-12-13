(require 'org)
(require 'org-agenda)
(require 'org-capture)

(setq org-directory "~/.gtd/"
      org-agenda-files '("~/.gtd/inbox.org"
                         "~/.gtd/todos.org"
                         "~/.gtd/todos-work.org"
                         "~/.gtd/calendar.org"
                         "~/.gtd/tickler.org"))

(setq org-refile-targets '(("todos.org" :maxlevel . 2)
                           ("todos-work.org" :maxlevel . 2)
                           ("tickler.org" :maxlevel . 2)
                           ("someday.org" :maxlevel . 2)
                           ("calendar.org" :maxlevel . 2)))

;; https://orgmode.org/manual/Template-elements.html#Template-elements
;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
(setq org-capture-templates
      '(("i" "Inbox" entry
         (file "inbox.org")
         "* %i%?\n%u\n")

        ("c" "Calendar" entry
         (file "calendar.org")
         "* %?\n%u\n")

        ("t" "Tickler" entry
         (file "tickler.org")
         "* %i%?\n%u\n")

        ("m" "Meeting" entry
         (file "inbox.org")
         "* Meeting with %? :meeting:" :clock-in t)

        ("j" "Journal" entry
         (file+olp+datetree "journal.org")
         "* %?\n%U\n")

        ("r" "Review templates")

        ("rd" "Daily Review" entry
         (file+olp+datetree "journal.org")
         (file "templates/daily-review.org") :clock-in t)

        ("rw" "Weekly Review" entry
         (file+olp+datetree "journal.org")
         (file "templates/weekly-review.org") :clock-in t)

        ("rm" "Monthly Review" entry
         (file+olp+datetree "journal.org")
         (file "templates/monthly-review.org") :clock-in t)))

(setq org-agenda-custom-commands '(("c" "Combined" ((agenda "") (todo "")))

                                   ("p" "Personal" ((agenda "") (tags-todo "-@work"))
                                    ((org-agenda-tag-filter (list "-@work"))))

                                   ("w" "Work" ((agenda "")(tags-todo "+@work"))
                                    ((org-agenda-tag-filter (list "+@work"))
                                     (org-agenda-span 2)))

                                   ("i" "Inbox" tags "*"
                                    ((org-agenda-files '("~/.gtd/inbox.org"))))))

(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                          (sequence "WAITING(w@/!)" "|" "CANCELED(c@/!)")))

(setq org-tag-alist '((:startgroup . nil) ;; Context
                      ("@home"     . ?h)
                      ("@work"     . ?w)
                      ("@errands"  . ?e)
                      ("@offline"  . ?o)
                      (:endgroup   . nil)
                      (:startgroup . nil) ;; Energy
                      ("focus"     . ?f)
                      ("casual"    . ?c)
                      ("fuzzy"     . ?z)
                      (:endgroup   . nil)))

(provide 'cr-org-gtd)
