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
;; See 'C-h v doom-font' for documentation and more examples of what they accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 20 :weight 'Regular)
      doom-variable-pitch-font (font-spec :family "Barlow" :size 20 :weight 'Thin))
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

;; (add-hook 'window-setup-hook #'toggle-frame-maximized)

(after! (treemacs)
  (setq treemacs-width 27))

(load! "./org-config.el")
(load! "./org-roam-config.el")
(load! "./bindings.el")


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


(after! (projectile)
  (setq projectile-project-search-path '( "~/dev_ws/" "~/coding/" "~/university" ))
  )

(after! (treemacs winum)
  (setq winum-ignored-buffers-regexp
        (delete
         (regexp-quote (format "%sScoped-Buffer-" treemacs--buffer-name-prefix))
         winum-ignored-buffers-regexp))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; EAF setup  ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework/"
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t))

;; (require 'eaf-pdf-viewer)
;; (require 'eaf-demo)
;; (require 'eaf-browser)
;; (require 'eaf-evil)

;; (define-key key-translation-map (kbd "SPC")
;;     (lambda (prompt)
;;       (if (derived-mode-p 'eaf-mode)
;;           (pcase eaf--buffer-app-name
;;             ("browser" (if (eaf-call-sync "execute_function" eaf--buffer-id "is_focus")
;;                            (kbd "SPC")
;;                          (kbd eaf-evil-leader-key)))
;;             ("pdf-viewer" (kbd eaf-evil-leader-key))
;;             ("image-viewer" (kbd eaf-evil-leader-key))
;;             (_  (kbd "SPC")))
;;         (kbd "SPC"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(use-package! hyperbole)

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                  :time-grid t
                                  :scheduled today)
                           (:name "Masters"
                                  :tag "Masters")
                           (:name "Due today"
                            :deadline today)
                                                      (:name "Important"
                                  :priority "A")
                           (:name "Overdue"
                                  :deadline past)
                           (:name "Big Outcomes"
                            :tag "bo"))))


(defun edwin-frame-1 ()
  (interactive)
   (set-frame-size nil 90 25)
  )


(defun edwin-frame-2 ()
  (interactive)
   (set-frame-size nil 126 32)
  )

(defun edwin-frame-3 ()
  (interactive)
   (set-frame-size nil 130 40)
  )

(defun edwin-frame-4 ()
  (interactive)
   (set-frame-size nil 140 45)
  )

(defun edwin-center ()
  (interactive)
  (modify-frame-parameters
   nil `(
         (left . 1.0)
         (top  . 0.5)
         (user-position . t)

         ))
  )

(setq treesit-extra-load-path '("~/.emacs.d-doom/tree-sitter"))

(add-to-list 'exec-path "/usr/local/texlive/2023/bin/x86_64-linux")

(map! :map cdlatex-mode-map :i "TAB" #'cdlatex-tab)

(setq-default TeX-master nil)

(with-eval-after-load 'treemacs
  (defun treemacs-custom-filter (file _)
    (or
     (s-ends-with? ".aux" file)
     (s-ends-with? ".log" file)
     (s-ends-with? ".pre" file)
     (s-ends-with? ".fdb_latexmk" file)
     (s-ends-with? ".fls" file)
    ))
  (push #'treemacs-custom-filter treemacs-ignored-file-predicates))

(setq cdlatex-env-alist
      '(
        ("bmatrix" "\\begin{bmatrix}\n?\n\\end{bmatrix}\n" nil)
        ))


(setq cdlatex-command-alist
      '(
        ("bmat" "Insert Bmatrix"   "" cdlatex-environment ("bmatrix") t nil)
        ))
