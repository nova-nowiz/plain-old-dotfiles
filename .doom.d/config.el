;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


(load! "private.el") ;; This is for private information, for example name and email
;; create a private.el file with your private informations in it
;; example from default doom emacs config.el:
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "Full Name"
;;       user-mail-address "your@email.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Hack Nerd Font" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-challenger-deep)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(map! "C-h" #'windmove-left
      "C-j" #'windmove-down
      "C-k" #'windmove-up
      "C-l" #'windmove-right)

(map! :after company
      :map company-active-map
      "<tab>" #'company-complete-selection
      "<return>" nil
      "C-l" #'company-complete-common)



(after! org
  (map! :map org-mode-map)
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(d)" "WAIT(w)" "|" "DONE(v)" "CANCELLED(x)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "#2ecc71" :weight bold)
          ("DOING" :foreground "#3498db" :weight bold)
          ("WAIT" :foreground "#bdc3c7" :weight bold)
          ("DONE" :foreground "#7f8c8d" :weight bold)
          ("CANCELLED" :foreground "#e74c3c" :weight bold))
        org-priority-highest ?A
        org-priority-lowest ?G
        org-priority-default ?D
        org-priority-faces
        '((?A :foreground "#e74c3c" :weight bold)
          (?B :foreground "#e67e22" :weight bold)
          (?C :foreground "#f1c40f" :weight bold)
          (?D :foreground "#ecf0f1" :weight bold)
          (?E :foreground "#27ae60" :weight bold)
          (?F :foreground "#2ecc71" :weight bold)
          (?G :foreground "#3498db" :weight bold)))
  (setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"))

;; (use-package org-fancy-priorities
;;   :hook (org-mode . org-fancy-priorities-mode)
;;   :config
;;   (setq org-fancy-priorities-list '("VERY HIGH" "HIGH" "MID/HIGH" "MID" "LOW" "VERY LOW" "OPTIONAL")))

(add-hook 'after-init-hook #'global-emojify-mode)
(require 'gdscript-mode)
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map)

(after! ispell
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,fr_FR")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,fr_FR"))

(after! langtool
  (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
  (setq langtool-default-language auto))

(global-evil-matchit-mode 1)

;; http://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(after! company (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(require 'server)
(unless (server-running-p)
  (server-start))
(global-activity-watch-mode)
