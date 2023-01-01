;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Edwin Clement"
      user-mail-address "edwinclement08@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 24 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 24))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/coding/org/")

(setq doom-localleader-key ",")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(add-hook 'window-setup-hook #'toggle-frame-maximized)

(after! (treemacs)
  (setq treemacs-width 27))

(setq org-agenda-files '(
                         "~/coding/org/friends"
                         "~/coding/org/projects"
                         "~/coding/org/masters"
                         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SVG TAG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! svg-tag-mode)

(defun svg-hook()
  (setq svg-tag-tags
        '((":android:" . ((lambda (tag) (svg-tag-make "android")))))))

(after! (org)
  (add-hook 'org-mode-hook 'svg-hook)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! (org)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NOW(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

  (setq org-id-track-globally t)
  (setq org-id-extra-files (directory-files-recursively org-roam-directory "\.org$"))

  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file-other-window)
          (wl . wl-other-frame))
        )
  (evil-define-key nil org-capture-mode-map
    [remap evil-save-and-close] #'org-capture-finalize
    [remap evil-save-modified-and-close] #'org-capture-finalize
    [remap evil-quit] #'org-capture-kill)
  )

(after! (org ox-hugo)
  (setq org-hugo-base-dir "~/coding/org-hugo-export"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open in external browser when using WSL ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Roam ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-roam-directory "~/coding/roam")

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
         (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("m" "masters" plain "%?" :target
         (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+FILETAGS: Masters\n")
         :unnarrowed t)
        )
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Roam UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! (projectile)
  (setq projectile-project-search-path '( "~/coding/" ))
  )

(after! (treemacs winum)
    (setq winum-ignored-buffers-regexp
          (delete (regexp-quote (format "%sFramebuffer-" treemacs--buffer-name-prefix))
                  winum-ignored-buffers-regexp)))


(use-package! ox-publish
  :config
  (setq org-publish-project-alist
        '(
          ("org-notes"
           :base-directory "~/coding/org/"
           :base-extension "org"
           :publishing-directory "~/coding/org-html-export/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t
           )
          ("org-static"
           :base-directory "~/coding/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/coding/org-html-export/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("org" :components ("org-notes" "org-static"))
          ))
  )

(defun ox-hugo/export-all (&optional org-files-root-dir dont-recurse)
  "Export all Org files (including nested) under ORG-FILES-ROOT-DIR.

All valid post subtrees in all Org files are exported using
`org-hugo-export-wim-to-md'.

If optional arg ORG-FILES-ROOT-DIR is nil, all Org files in
current buffer's directory are exported.

If optional arg DONT-RECURSE is nil, all Org files in
ORG-FILES-ROOT-DIR in all subdirectories are exported. Else, only
the Org files directly present in the current directory are
exported.  If this function is called interactively with
\\[universal-argument] prefix, DONT-RECURSE is set to non-nil.

Example usage in Emacs Lisp: (ox-hugo/export-all \"~/org\")."
  (interactive)
  (let* ((org-files-root-dir (or org-files-root-dir default-directory))
         (dont-recurse (or dont-recurse (and current-prefix-arg t)))
         (search-path (file-name-as-directory (expand-file-name org-files-root-dir)))
         (org-files (if dont-recurse
                        (directory-files search-path :full "\.org$")
                      (directory-files-recursively search-path "\.org$")))
         (num-files (length org-files))
         (cnt 1))
    (if (= 0 num-files)
        (message (format "No Org files found in %s" search-path))
      (progn
        (message (format (if dont-recurse
                             "[ox-hugo/export-all] Exporting %d files from %S .."
                           "[ox-hugo/export-all] Exporting %d files recursively from %S ..")
                         num-files search-path))
        (dolist (org-file org-files)
          (with-current-buffer (find-file-noselect org-file)
            (message (format "[ox-hugo/export-all file %d/%d] Exporting %s" cnt num-files org-file))
            (org-hugo-export-wim-to-md :all-subtrees)
            (setq cnt (1+ cnt))))
        (message "Done!")))))


(map! :leader
      :prefix ("r" . "roam")
      "f" #'org-roam-node-find
      "i" #'org-roam-node-insert
      "m" #'org-roam-buffer-toggle
      "n" #'org-roam-capture
      "u" #'org-roam-ui-open
      )

(map! :leader
       (:prefix-map ("l" . "workspace")
        :desc "Display tab bar"           "`" #'+workspace/display
        :desc "Switch workspace"          "."   #'+workspace/switch-to
        :desc "Switch to last workspace"  "TAB"   #'+workspace/other
        :desc "New workspace"             "n"   #'+workspace/new
        :desc "New named workspace"       "N"   #'+workspace/new-named
        :desc "Load workspace from file"  "l"   #'+workspace/load
        :desc "Save workspace to file"    "s"   #'+workspace/save
        :desc "Delete session"            "x"   #'+workspace/kill-session
        :desc "Delete this workspace"     "d"   #'+workspace/delete
        :desc "Rename workspace"          "r"   #'+workspace/rename
        :desc "Restore last session"      "R"   #'+workspace/restore-last-session
        :desc "Next workspace"            "]"   #'+workspace/switch-right
        :desc "Previous workspace"        "["   #'+workspace/switch-left
        :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
        :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
        :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
        :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
        :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
        :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
        :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
        :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
        :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
        :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))

(map! :leader
      "TAB" #'evil-switch-to-windows-last-buffer
      )



(map! :leader
      ":" #'projectile--find-file
      "SPC" #'execute-extended-command
      )

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework/"
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t))
;; (require 'eaf-image-viewer)
;; (require 'eaf-terminal)
;; (require 'eaf-video-player)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-org-previewer)
;; (require 'eaf-music-player)
;; (require 'eaf-file-manager)
;; (require 'eaf-rss-reader)
;;(require 'eaf-mindmap)
;; (require 'eaf-jupyter)
;; (require 'eaf-camera)
;; (require 'eaf-system-monitor)
;;(require 'eaf-netease-cloud-music)
;;(require 'eaf-file-browser)
;;(require 'eaf-file-sender)
;;(require 'eaf-airshare)
;;(require 'eaf-vue-demo)

  (require 'eaf-pdf-viewer)
(require 'eaf-demo)
  (require 'eaf-browser)
;; If use Evil-mode Optional
(require 'eaf-evil)

(define-key key-translation-map (kbd "SPC")
    (lambda (prompt)
      (if (derived-mode-p 'eaf-mode)
          (pcase eaf--buffer-app-name
            ("browser" (if (eaf-call-sync "execute_function" eaf--buffer-id "is_focus")
                           (kbd "SPC")
                         (kbd eaf-evil-leader-key)))
            ("pdf-viewer" (kbd eaf-evil-leader-key))
            ("image-viewer" (kbd eaf-evil-leader-key))
            (_  (kbd "SPC")))
        (kbd "SPC"))))
