;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/coding/org/")

(setq org-agenda-files '(
                         "~/coding/org/friends"
                         "~/coding/org/projects"
                         "~/coding/org/masters"
                         "~/coding/roam"
                         "~/coding/org/journal.org"
                         ))


(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

(after! (org)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NOW(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
(setq org-log-done t)
  (setq org-id-track-globally t)
  (setq org-id-extra-files (directory-files-recursively org-roam-directory "\.org$"))

  (setq org-format-latex-options '(:foreground default :background default :scale 2.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
             ("begin" "$1" "$" "$$" "\\(" "\\[")))

  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame))
        )

  ;; (setq org-link-frame-setup
  ;;       '((vm . vm-visit-folder-other-frame)
  ;;         (vm-imap . vm-visit-imap-folder-other-frame)
  ;;         (gnus . org-gnus-no-new-newso
  ;;         (file . find-file-other-window)
  ;;         (wl . wl-other-frame))
  ;;       )

  (evil-define-key nil org-capture-mode-map
    [remap evil-save-and-close] #'org-capture-finalize
    [remap evil-save-modified-and-close] #'org-capture-finalize
    [remap evil-quit] #'org-capture-kill)
  )

(setq vulpea-capture-inbox-file
  (format "%sinbox-%s.org" org-directory (system-name)))


(setq org-capture-templates
      '(("t" "todo" plain (file vulpea-capture-inbox-file)
         "* TODO %?\n%U\n")
        ("j" "Journal" entry
         (file+olp+datetree +org-capture-journal-file)
         "* %U %?\n%i\n%a" :prepend t)
        ))

(defun vulpea-capture-task ()
  "Capture a task."
  (interactive)
  (org-capture nil "t"))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (
         ("C-M-S-Y" . org-download-screenshot)
         )
  )
  :custom
  (org-download-screenshot-method "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")
  )
