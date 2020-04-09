(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.melpa.org/packages/"))
(package-initialize)


(when (not package-archive-contents)
  (package-refresh-contents))


(add-to-list 'default-frame-alist '(internal-border-width . 20))
(set-frame-parameter nil 'internal-border-width 10)
;(setq-default left-margin-width 10 right-margin-width 8) ; Define new widths.
(set-window-buffer nil (current-buffer)) ; Use them now.


(defvar my-packages '(ace-jump-mode ; Jump to any text on screen in a few keystrokes. Like Vim's EasyMotion.
                      ag ; Silver searcher integration for Emacs
                      autopair ; Insert matching delimiters, e.g. insert closing braces.
                      dash ; Dash provides modern functions for working with lists in Emacs Lisp.
                      dash-functional ; Useful combinators for Emacs Lisp.
                      diminish ; For hiding and shortening minor modes in the modeline
                      evil ; Evil mode implements Vim's modal bindings and text object manipulation.
                      evil-nerd-commenter
                      evil-anzu ;
                      general ; Functions for defining keybindings and leader keys. Complements Evil.
                      flx-ido ; Fuzzy matching for ido, which improves the UX of Projectile.
                      ido-completing-read+ ; Fancy completion all over Emacs, not just for buffers and files.
                      highlight-numbers ; Display numbers in a different color
                      go-mode ; For editing Go files.
                      hiwin ; For highlighting the active pane/window in Emacs.
                      js-comint ; For evaluating javascript code to a REPL.
                      less-css-mode ; Syntax highlighting for LESS CSS files.

                      projectile ; Find file in project (ala Vim's CTRL-P or Textmate's Cmd-T)
                      scss-mode ; For editing SCSS files.
                      smartparens ; For editing expressions in parentheses.
                      smex ; Makes the M-x command more useful by showing you recently used commands, etc.
                      wcheck-mode ; Spell checking
                      yaml-mode ; For editing YAML files
                      yasnippet)) ; Insert snippets using tab.

;; Ensure that every package above is installed. This is helpful when setting up Emacs on a new machine.
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Based on my anecdotal observations, this reduces the amount of display flicker during Emacs startup.
(setq redisplay-dont-pause t)

;; This allows Emacs to occupy the full screen width and height, so that nothing below it (e.g. the desktop)
;; is visible. The default behavior in Emacs is to allow only resizing by whole character-columns and rows.
(setq frame-resize-pixelwise t)


(defun set-env-vars-from-shell ()
  "This fetches a list of env vars exported in the interactive shell, and sets them as env vars within Emacs
   so that subshells run from Emacs have the same environment vars as if they were executed from a shell."
  ;; NOTE(philc): Doing this is necessary because if you launch Emacs.app on OSX not from a terminal, Emacs
  ;; not have the same environment as my user shell. I have many env vars (e.g. Ansible's env) which are
  ;; critical for executing my REPLs from within Emacs.
  (let* ((shell "zsh") ;; NOTE(philc): Change to your desired shell. You could also use the $SHELL env var.
         ;; NOTE(philc): Starting an interactive shell "-i" takes 1s on my machine, so this delays the startup
         ;; time of Emacs by that much.
         (env-vars (->> (util/call-process-and-check shell nil "-ic" "env")
                        (s-split "\n")
                        (-map (lambda (line) (s-split "=" line 1))))))
    (-each env-vars
      (lambda (pair)
        (when pair (setenv (first pair) (second pair)))))))

(defun set-exec-path-from-shell-PATH ()
  "Use the same PATH within Emacs as your shell."
  ;; From http://clojure-doc.org/articles/tutorials/emacs.html
  (let* ((shell "zsh") ;; NOTE(philc): Change to your desired shell. You could also use the $SHELL env var.
         (path-from-shell (shell-command-to-string (concat shell " -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system
  (set-exec-path-from-shell-PATH)
  (set-env-vars-from-shell))

;; Reload an open file from disk if it is changed outside of Emacs.
(global-auto-revert-mode 1)

;; Turn off backups and autosaves so we don't have ~ and # files strewn about the working directory.
(setq make-backup-files nil)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-default nil)

;; Disable Emacs' write-lock, which creates temporary .#files when saving. This crashes coffeescript --watch.
;; https://github.com/jashkenas/coffeescript/issues/985
(setq create-lockfiles nil)

;;  Don't ask confirmation to follow symlinks to edit files.
(setq vc-follow-symlinks t)

;; Save your minibuffer history across Emacs sessions.
(savehist-mode t)

;; Include the path when displaying buffer names which have the same filename open (e.g. a/foo.txt b/foo.txt)
(setq uniquify-buffer-name-style 'forward)

;; Whitespace & line wrapping.
;; (global-whitespace-mode t)
(with-eval-after-load "whitespace"
  (setq whitespace-line-column 110) ; When text flows past 110 chars, highlight it.
  ;; whitespace-mode by default highlights all whitespace. Show only tabs and trailing spaces.
  (setq whitespace-style '(face trailing lines-tail)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default tab-width 2)
(setq-default evil-shift-width 2)

;; Some modes have their own tab-width variables which need to be overridden.
(setq-default css-indent-offset 2)

(setq sentence-end-double-space nil) ; Don't add double spaces after periods when filling strings in quotes.
(setq-default fill-column 110) ; When wrapping with the Emacs fill commands, wrap at 110 chars.
(auto-fill-mode t) ; When typing across the fill-column, hard-wrap the line as you type.
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; Some modes, like markdown, turn off autofill. Force it!

;; Visually wrap long lines on word boundaries. By default, Emacs will wrap mid-word. Note that Evil doesn't
;; have good support for moving between visual lines versus logical lines. Here's the start of a solution:
;; https://lists.ourproject.org/pipermail/implementations-list/2011-December/001430.html
(global-visual-line-mode t)

;; Highlight the line the cursor is on. This is mostly to make it easier to tell which split is active.
;; (global-hl-line-mode)
;; Don't blink the cursor. I can easily see it, because the line the cursor is on is already highlighted.
(blink-cursor-mode -1)

;; Indent with spaces instead of tabs by default. Modes that really need tabs should enable indent-tabs-mode
;; explicitly. Makefile-mode already does that, for example. If indent-tabs-mode is off, replace tabs with
;; spaces before saving the file.
(setq-default indent-tabs-mode nil)
(add-hook 'write-file-hooks
          (lambda ()
            (if (not indent-tabs-mode)
                (untabify (point-min) (point-max)))
            nil))


(defun backward-delete-word ()
  "Deletes the word behind the cursor, and does not yank the contents to the clipboard."
  ; This implementation is based on backward-kill-word.
  (interactive)
  (delete-region (point) (progn (forward-word -1) (point))))

;; Emacs modes universally bind C-h to "help", but I use C-h for backspace. It's very difficult to redefine
;; C-h in many modes, like minibuffer-mode. This instead translates C-h to C-?. It's unclear to me exactly how
;; this works. See https://github.com/emacs-helm/helm/issues/24 for discussion.
(define-key key-translation-map [?\C-h] [?\C-?])

;; Disable the prompt we get when killing a buffer with a process. This affects clojure mode in particular,
;; when we want to restart the nrepl process.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Use smex to show the M-x command prompt. It has better completion support than the default M-x.
;; RecentF mode is the Emacs minor mode used when opening files via C-x C-f.
(require 'recentf)
(define-key recentf-mode-map (kbd "C-w") 'backward-delete-word)

;; The poorly-named winner mode saves the history of your window splits, so you can undo and redo changes to
;; your window configuration.
(winner-mode t)

;; Save buffers whenever they lose focus.
;; This obviates the need to hit the Save key thousands of times a day. Inspired by http://goo.gl/2z0g5O.
(dolist (f '(windmove-up windmove-right windmove-down windmove-left))
  (advice-add f :before (lambda (&optional args) (util/save-buffer-if-dirty))))

;; When switching focus out of the Emacs app, save the buffer.
(add-hook 'focus-out-hook 'util/save-buffer-if-dirty)

(setq global-leader-prefix ";")
(general-define-key
 :prefix global-leader-prefix
 :keymaps '(normal visual)
 "wn" 'create-new-column
 "wv" 'split-window-horizontally-and-focus
 "wh" 'split-window-vertically-and-focus
 "wk" (lambda () (interactive) (kill-buffer (current-buffer)))
 "wm" 'toggle-window-maximize
 "wr" 'evil-window-rotate-downwards
 "wR" 'evil-window-rotate-upwards
 "wb" 'balance-windows
 ;; winner-undo will undo the last change you made to your window configuration.
 "wu" 'winner-undo
 "we" 'narrow-ephemeral-window
 "wE" 'toggle-maximize-lower-right-window
 "q" 'dismiss-ephemeral-windows
 "wf" 'toggle-frame-fullscreen)



;;
;; Incremental search (isearch)
;;
;; Make highlighting during incremental search feel snappier.
(setq case-fold-search t) ; Make Emac searches case insensitive.
(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-max-at-a-time nil)
;; Hitting esacpe aborts the search, restoring your cursor to the original position, as it does in Vim.
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
;; Make C-h act the same as backspace, as it does in readline.
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
;; Make M-v paste the clipboard's text into the search ring.
(define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "C-w") 'isearch-del-word)

(defun trim-last-word-of-string (string)
  "Removes the last word from the given string. Word separators are -, _ and spaces. This is designed to
  perform the same function as kill-word, but on a string argument."
  (lexical-let ((i 0))
    (while (and (< i (length string))
                (string-match "[-_ ]+" string i))
      (setq i (second (match-data))))
    (if (= i 0)
      ""
      (substring string 0 (dec i)))))

(defun isearch-del-word (&optional arg)
  "Delete word from end of search string and search again. If search string is empty, just beep.
  This function definition is based on isearch-del-char, from isearch.el."
  (interactive "p")
  (if (= 0 (length isearch-string))
    (ding)
    (setq isearch-string (trim-last-word-of-string isearch-string)
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")))
  ;; Use the isearch-other-end as new starting point to be able
  ;; to find the remaining part of the search string again.
  (when isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/vASrP0P-tXM
(defun recenter-no-redraw (&optional arg)
  "Centers the viewport around the cursor."
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))

;; When pressing enter to confirm a search, or jumping to the next result, scroll the result into the center
;; of the window. This removes the UX problem of the result appearing at the bottom of the screen with little
;; context around it.
(defadvice evil-search-next (after isearch-recenter activate)
  (recenter-no-redraw))

(defadvice evil-search-previous (after isearch-recenter activate)
  (recenter-no-redraw))

(defadvice isearch-exit (before isearch-recenter activate)
  (recenter-no-redraw))


;;
;; Filename completions (i.e. CTRL-P or CMD-T in other editors)
;;
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(setq ido-use-filename-at-point 'guess)

;; You can customize the order in which files are sorted when Ido displays them in the minibuffer. There are certain file extensions I use more than others, so I tell Ido to emphasize those.
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))


;; By default, ido-switch-buffer will move your focus to another frame if the buffer is open there. I instead
;; want the desired buffer to open again within my current frame, even if it's already open in another frame.
(setq ido-default-buffer-method 'selected-window)
(with-eval-after-load "ido"
  (setq ido-enable-flex-matching t)
  (setq ido-use-virtual-buffers t)
  (setq ido-everywhere t)
  ;; Kill (unload) the highlighted buffer in the matches list.
  (define-key ido-file-completion-map (kbd "C-w") 'backward-delete-word)
  (define-key ido-buffer-completion-map (kbd "M-d") 'ido-kill-buffer-at-head))


;;
;; Ag (silver searcher)
;;
(require 'ag)

;; Use Projectile to determine what the current project is when invoking ag-project. Normally, AG will simply
;; find the surrounding .git directory and use that as the project.
(setq ag-project-root-function (lambda (f) (projectile-project-root)))

;; Note that ag mode configures itself to start in Evil's "motion" state.
;;(evil-define-key 'motion ag-mode-map
;;  ;; By default, ag's search results buffer opens in random windows. This also happens when opening one of the
;;  ;; files in the search results. Instead, use "o" to open the search result in the same buffer and "O" to
;;  ;; open in a new buffer. This mirrors Vim's convention of o and O.
;;  (kbd "RET") 'ag/open-search-result-in-same-window
;;  "o" 'ag/open-search-result-in-same-window
;;  "O" 'ag/open-search-result-in-window-to-right
;;  "gg" 'evil-goto-first-line)

(defun ag/open-search-result-in-same-window ()
  (interactive)
  (let ((ag-reuse-window t)) (compile-goto-error)))

(defun ag/open-search-result-in-window-to-right ()
  (interactive)
  (lexical-let ((move-right-or-create (lambda ()
                                        (message "called")
                                        (condition-case nil (windmove-right)
                                          (error (progn (split-window-right) (windmove-right)))))))
    (util/with-patch-function
     'pop-to-buffer (buffer &rest args) (progn (funcall move-right-or-create) (switch-to-buffer buffer))
     (compile-goto-error))))

(defun ag-project-in-current-window ()
  "Like `ag-project`, but shows the search output in the current window. If a selection is highlighted, use
   that as the search string rather than prompting."
  (interactive)
  (let* ((project-dir (ag/project-root default-directory))
         (search-string (if (region-active-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
                          (read-from-minibuffer "Search: " (ag/dwim-at-point))))) ; Taken from (ag) in ag.el.
    (util/with-patch-function
     'display-buffer (buffer &rest args) (progn (switch-to-buffer buffer) (selected-window))
     (ag/search search-string project-dir))))



;;
;; Projectile (find file from the root of the current project).
;;
(projectile-global-mode)
;; NOTE(philc): Using this cache is annoying because it gets stale if files appear on disk after a git pull.
;; However, in my large repos, without it, projectile-find-file takes about 1s to open, which is an
;; unacceptable delay.
(setq projectile-enable-caching t)

(defun restart-projectile-find-file-hook ()
  (remove-hook 'post-command-hook 'restart-projectile-find-file-hook)
  (let ((query previous-projectile-input))
    (makunbound 'previous-projectile-input)
    (projectile-find-file-with-initial-value query)))

(defun projectile-find-file-with-initial-value (initial-val)
  "Useful to call after reloading the project cache while the find file dialog still open."
  (interactive)
  (let ((file (projectile-completing-read "Find file: "
                                            (projectile-current-project-files) initial-val)))
      (find-file (expand-file-name file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)))


(defun replace-smart-quotes (beg end)
  "Replace any fancy non-ascii quote characters with plain ones. You can get fancy quotes when copying text
   from the web into Emacs."
  (interactive "r")
  (format-replace-strings smart-chars-to-ascii nil beg end))

;; Theme settings
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7f6d4aebcc44c264a64e714c3d9d1e903284305fd7e319e7cb73345a9994f5ef" default)))
 '(package-selected-packages
   (quote
    (ido-vertical-mode ido-completing-read+ ido-clever-match highlight-numbers dired-details+ autopair ag ace-jump-mode lsp-mode go-autocomplete auto-complete terraform-mode hcl-mode evil nord-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "color-250" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(font-lock-variable-name-face ((t (:foreground "cyan"))))
 '(highlight-numbers-number ((t (:foreground "#f0ad6d"))))
 '(mode-line ((t (:background nil :foreground "white"))))
 '(mode-line-inactive ((t (:background nil :foreground "white"))))
 '(smerge-base ((t (:background "brightblack"))))
 '(smerge-lower ((t (:background "brightblack"))))
 '(smerge-markers ((t (:background "brightblack"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "brightblack" :foreground "green"))))
 '(smerge-refined-removed ((t (:background "brightblack" :foreground "red"))))
 '(smerge-upper ((t (:background "brightblack" :foreground "red")))))


(load-theme 'nord t)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'yaml-mode-hook 'highlight-numbers-mode)

;; No top menu
(menu-bar-mode -1)

;; Display line numbers
(global-display-line-numbers-mode)

;; Use evil mode
(require 'evil)
(evil-mode 1)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

(with-eval-after-load 'evil
  (require 'evil-anzu))


;; Start with an empty buffer
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Turn off backup files
(setq make-backup-files nil)

;; Display matching parenthesis
(show-paren-mode 1)


;
;; YAML mode, for editing YAML files
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;
;; JSON
;;
(defun json-format ()
  "Pipe the current buffer into `jq .`, and replace the current buffer's contents."
  (interactive)
  ;; TODO(philc): Try to replace this with **json-pretty-print' and 'json-pretty-print-buffer' from Emacs 25.
  (save-excursion
    (call-process-region (point-min) (point-max) "jq" t (buffer-name) t ".")))



;; Go configuration
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setenv "GOPATH" "/Users/pedrogutierrez/.go")
(add-to-list 'exec-path "/Users/pedrogutierrez/.go/bin")


(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  ; Go oracle
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)


(with-eval-after-load 'go-mode
   (require 'go-autocomplete))
