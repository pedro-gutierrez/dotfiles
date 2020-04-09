(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

(package-initialize)
(menu-bar-mode -1)
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7f6d4aebcc44c264a64e714c3d9d1e903284305fd7e319e7cb73345a9994f5ef" default)))
 '(package-selected-packages (quote (terraform-mode hcl-mode evil nord-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background nil :foreground "white"))))
 '(mode-line-inactive ((t (:background nil :foreground "white")))))

;; '(vc-conflict-state ((t (:background nil))))
;; '(vc-edited-state ((,class (:foreground ,nord13))))
;; '(vc-locally-added-state ((,class (:underline ,nord14))))
;; '(vc-locked-state ((,class (:foreground ,nord10))))
;; '(vc-missing-state ((,class (:foreground ,nord11))))
;; '(vc-needs-update-state ((,class (:foreground ,nord12))))
;; '(vc-removed-state ((,class (:foreground ,nord11))))
;; '(vc-state-base ((,class (:foreground ,nord4))))
;; '(vc-up-to-date-state ((,class (:foreground ,nord8))))

(load-theme 'nord t)

;; Display line numbers
(global-display-line-numbers-mode)

;; Use evil mode
(require 'evil)
(evil-mode 1)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

;; Start with an empty buffer
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Turn off backup files
(setq make-backup-files nil)
;; This is a test


