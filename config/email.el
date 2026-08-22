;; -*- lexical-binding: t; -*-

(setq user-full-name "Beat Hagenlocher")

(after! mu4e
  (setq message-send-mail-function #'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t
        send-mail-function #'message-send-mail-with-sendmail
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        mail-specify-envelope-from 'header
        mu4e-drafts-folder "/drafts"
        mu4e-completing-read-function #'completing-read
        mu4e-confirm-quit nil
        mu4e-main-hide-personal-addresses t
        mu4e-change-filenames-when-moving t
        mu4e-attachment-dir "~/Downloads/"
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask
        mu4e-search-results-limit -1
        mu4e-search-skip-duplicates nil
        mu4e-update-interval 60
        mu4e-headers-date-format "%Y-%m-%d  %H:%M"
        ;; mu4e-headers-actions '(("move to" . mu4e-move))
        mu4e-headers-fields '((:human-date . 18)
                              (:flags . 6)
                              (:maildir . 15)
                              (:mailing-list . 10)
                              (:from . 22)
                              (:subject))
        mu4e-bookmarks '((:name "AG inbox" :query "maildir:/ag/Inbox" :key ?a)
                         (:name "AG actions" :query "maildir:/ag/action" :key ?r)
                         (:name "AG read/review" :query "maildir:/ag/read-review" :key ?c)
                         (:name "AG waiting for" :query "maildir:/ag/waiting-for" :key ?t)
                         (:name "AG incubate" :query "maildir:/ag/incubate" :key ?g)

                         (:name "bah inbox" :query "maildir:/posteo/Inbox" :key ?m)
                         (:name "bah action" :query "maildir:/posteo/action" :key ?n)
                         (:name "bah read/review" :query "maildir:/posteo/read-review" :key ?e)
                         (:name "bah waiting for" :query "maildir:/posteo/waiting-for" :key ?i)
                         (:name "bah incubate" :query "maildir:/posteo/incubate" :key ?o)

                         (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
                         (:name "Sent" :query "maildir:/ag/Sent OR maildir:/posteo/Sent" :key ?s)))
  (unbind-key "C-S-u" 'mu4e-compose-mode-map)
  (set-email-account!
   "bah"
   '((user-mail-address . "bah@posteo.de")
     (mu4e-sent-folder . "/posteo/Sent")
     (mu4e-trash-folder . "/posteo/Trash")
     (mu4e-compose-signature . nil)
     (mu4e-refile-folder . (lambda (msg)
                             (let* ((date (mu4e-message-field-at-point :date))
                                    (year (decoded-time-year (decode-time date))))
                               (concat "/posteo/Archive/"
                                       (number-to-string year))))))
   t)
  (set-email-account!
   "hagenlob"
   '((user-mail-address . "hagenlob@posteo.de")
     (mu4e-sent-folder . "/posteo/Sent")
     (mu4e-trash-folder . "/posteo/Trash")
     (mu4e-compose-signature . nil)
     (mu4e-refile-folder . (lambda (msg)
                             (let* ((date (mu4e-message-field-at-point :date))
                                    (year (decoded-time-year (decode-time date))))
                               (concat "/posteo/Archive/"
                                       (number-to-string year))))))
   t)
  (set-email-account!
   "ag"
   `((user-mail-address . "beat.hagenlocher@active-group.de")
     (mu4e-sent-folder . "/ag/Sent")
     (mu4e-trash-folder . "/ag/Trash")
     (mu4e-compose-signature . ,(concat
                                 "Beat Hagenlocher\n"
                                 "beat.hagenlocher@active-group.de\n\n"
                                 "+49 (7071) 70896-67\n\n"
                                 "Active Group GmbH\n"
                                 "Hechinger Str. 12/1\n"
                                 "72072 Tübingen\n"
                                 "Registergericht: Amtsgericht Stuttgart, HRB 224404\n"
                                 "Geschäftsführer: Dr. Michael Sperber"))
     (mu4e-refile-folder . (lambda (msg)
                             (let* ((date (mu4e-message-field-at-point :date))
                                    (year (decoded-time-year (decode-time date))))
                               (concat "/ag/archive/"
                                       (number-to-string year))))))
   t))

(defun bah/mu4e-mark-move-to (maildir)
  "Mark the message at point for moving to MAILDIR, then advance."
  (mu4e-mark-at-point 'move maildir)
  (mu4e-headers-next))

(map! :map mu4e-headers-mode-map

      :desc "Just execute mark"         :n "x"     (cmd! (mu4e-mark-execute-all t))

      :prefix ("i a" . "ag")
      :desc "ag: Move to action"      :n "m" (cmd! (bah/mu4e-mark-move-to "/ag/action"))
      :desc "ag: Move to archive"     :n "n" (cmd! (bah/mu4e-mark-move-to "/ag/archive"))
      :desc "ag: Move to read-review" :n "e" (cmd! (bah/mu4e-mark-move-to "/ag/read-review"))
      :desc "ag: Move to waiting-for" :n "i" (cmd! (bah/mu4e-mark-move-to "/ag/waiting-for"))
      :desc "ag: Move to incubate"    :n "o" (cmd! (bah/mu4e-mark-move-to "/ag/incubate"))

      :prefix ("i p" . "posteo")
      :desc "Posteo: Move to action"      :n "m" (cmd! (bah/mu4e-mark-move-to "/posteo/action"))
      :desc "Posteo: Move to archive"     :n "n" (cmd! (bah/mu4e-mark-move-to "/posteo/archive"))
      :desc "Posteo: Move to read-review" :n "e" (cmd! (bah/mu4e-mark-move-to "/posteo/read-review"))
      :desc "Posteo: Move to waiting-for" :n "i" (cmd! (bah/mu4e-mark-move-to "/posteo/waiting-for"))
      :desc "Posteo: Move to incubate"    :n "o" (cmd! (bah/mu4e-mark-move-to "/posteo/incubate"))
      )
