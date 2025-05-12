;; -*- lexical-binding: t; -*-

(setq user-full-name "Beat Hagenlocher")

(after! mu4e
  (setq message-send-mail-function #'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t
        send-mail-function #'message-send-mail-with-sendmail
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        mail-specify-envelope-from 'header
        ;; +mu4e-backend 'mbsync
        mu4e-drafts-folder "/drafts"
        mu4e-completing-read-function #'completing-read
        mu4e-confirm-quit nil
        mu4e-change-filenames-when-moving t
        mu4e-attachment-dir "~/Downloads/"
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask
        mu4e-search-results-limit -1
        mu4e-search-skip-duplicates nil
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:maildir . 15)
                              (:mailing-list . 10)
                              (:from . 22)
                              (:subject))
        mu4e-bookmarks '((:name "AG inbox" :query "maildir:/ag/Inbox" :key ?a)
                         (:name "AG actions" :query "maildir:/ag/actions" :key ?r)
                         (:name "AG read/review" :query "maildir:/ag/read-review" :key ?s)
                         (:name "AG waiting for" :query "maildir:/ag/waiting-for" :key ?t)
                         (:name "AG incubate" :query "maildir:/ag/incubate" :key ?g)

                         (:name "Posteo inbox" :query "maildir:/posteo/Inbox" :key ?m)
                         (:name "Posteo action" :query "maildir:/posteo/action" :key ?n)
                         (:name "Posteo read/review" :query "maildir:/posteo/read-review" :key ?e)
                         (:name "Posteo waiting for" :query "maildir:/posteo/waiting-for" :key ?i)
                         (:name "Posteo incubate" :query "maildir:/posteo/incubate" :key ?o)

                         (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
                         (:name "Sent" :query "maildir:/ag/Sent OR maildir:/posteo/Sent" :key ?s)))
  (set-email-account!
   "posteo"
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

;; (map! :mu4e-view-mode-map
;;       :desc "Mark for move"       "m m" #'mu4e-headers-mark-for-move
;;       :desc "Move to"             "m a" (cmd! )
;;       )
