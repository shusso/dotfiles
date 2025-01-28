;; Install Packages
(load "~/.emacs.d/init_packages.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          FUNCTION start          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(defun frame-back ()
  "Move to the previous frame."
  (interactive)
  (other-frame -1))

(defun window-back ()
  "Move to previous window in current buffer."
  (interactive)
  (other-window -1))

(defun toggle-comment-on-line()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; M-x list-colors-display
(setq org-todo-keyword-faces
      '(("TODO" . (:background "brightred" :foreground "brightwhite" :weight demibold :slant italic))
        ("ONGOING" . (:background "color-190" :foreground "color-172" :weight demibold :slant italic))
        ("ON_HOLD" . (:background "color-52" :weight bold))
        ("BLOCKED" . (:background "color-166" :foreground "color-82" :weight bold))
        ("COURSE" . (:background "color-198" :foreground "color-122" :weight bold))
        ("IDEA" . (:background "color-202" :foreground "color-226" :weight normal))
        ("DONE" . (:background "color-118" :foreground "color-124" :weight bold))
        ("LATER" . (:background "color-190" :foreground "color-195" :weight bold))
        ("INVALID" . (:background "color-192" :foreground "color-195" :weight bold))

        ("ON_REVIEW" . (:foreground "color-184" :weight normal))
        ("RESEARCH" . (:background "color-185" :weight bold))
        ("LEARNING" . (:foreground "color-58" :weight normal))
        ("ASSIGNMENTS" . (:foreground "color-38" :weight normal))
        ("VIDEOS" . (:foreground "color-44" :weight normal))
        ("MISC" . (:background "color-220" :foreground "color-196" :weight bold))
        ("BUG" . (:foreground "color-230" :background "brightred" :weight normal))
        ("IMPORTANT_DUE_SOON" . (:foreground "color-195" :background "brightred" :weight bold))
        ("IMPORTANT_NOT_DUE_SOON" . (:foreground "color-211" :weight bold))
        ("NOT_IMPORTANT_DUE_SOON" . (:foreground "color-203" :weight normal))
        ("NOT_IMPORTANT_NOT_DUE_SOON" . (:foreground "color-222" :weight normal))
        ))

(defun org-mode-export-hook ()
  (add-hook 'after-save-hook 'org-html-export-to-html t t))

(defun toggle-org-html-export-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (add-hook 'after-save-hook 'org-md-export-to-markdown nil t)
    (message "Enabled org html export on save for current buffer...")))


(defun 3-buffers ()
  "Open buffers with follow mode."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode t))

(defun 2-buffers ()
  "Open buffers with follow mode."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode t))

(defun switch-to-previous-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE"))))

(defun auto-complete-for-go ()
  "Enable auto-complete for go."
  (auto-complete-mode 1))

(defun my-go-mode-hook ()
  "My go mode hook."
  ; Call Gofmt before saving
  ; (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-c") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-C") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-\\") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-\'") 'previous-error)     ; Go to previous error or msg
  ;; Misc go stuff
  (auto-complete-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           FUNCTION end           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           MODES start            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; testing
(add-to-list 'load-path "~/.emacs.d/testing")

;; modern color theme
;; https://github.com/emacs-jp/replace-colorthemes
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/color-themes"))
;; M-x customize-face RET hl-line RET

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(autoload 'ace-jump-pop-mark "ace-jump-mode" "Ace jump back" t)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))

;; to open file from remote server do the following
;; C-x f
;; old: /ssh root@_ip_:/root/
;; /root@_ip_:/root/
(setq tramp-default-method "scp")

;(setq slime-default-lisp 'sbcl)
(setq inferior-lisp-program "sbcl")
(slime-setup)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
 '(add-to-list 'ac-modes 'slime-repl-mode))
(add-hook 'lisp-mode-hook
  (lambda ()
    (paredit-mode +1)))

;; clojure + cider ;;
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "<em>nrepl</em>")

;; Poping-up contextual documentation
(eval-after-load "cider"
    '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; paredit for formatting clojure s-expressions
(add-hook 'clojure-mode-hook 'paredit-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode) ;; only for clojure-mode

(setq-default projectile-mode t)

(setq ein:output-area-inlined-images t)
(setq print-level 1)
(setq print-length 1)
(setq print-circle t)

;; Allow automatically handing of created/expired meta data.
;; https://stackoverflow.com/a/13285957
;; (require 'org-expiry)
(setq
  org-expiry-created-property-name "CREATED" ; Name of property when an item is created
  org-expiry-inactive-timestamps   t         ; Don't have everything in the agenda view
)

(defun mrb/insert-created-timestamp()
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (org-expiry-insert-created)
  ;; (org-back-to-heading)
  (org-end-of-line)
  ;; (insert " ")
)


;; Insert Created timestamp always on TODO entries (heading/subheading)
;; Might not needed with org-meta-return..
(defadvice org-insert-todo-heading (after mrb/created-timestamp-advice activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (mrb/insert-created-timestamp)
)
(defadvice org-insert-todo-subheading (after mrb/created-timestamp-advice activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (mrb/insert-created-timestamp)
  )


;; Make it active
(ad-activate 'org-insert-todo-heading)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            MODES end             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          CUSTOMS start           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq-default indent-tabs-mode nil)
(setq tab-width 4)
'(python-guess-indent 4)
'(tab-width 4)

(add-hook 'python-mode-hook '(lambda () 
 (setq python-indent 4)))

;; Python Hook
(add-hook 'python-mode-hook
  (function (lambda ()
    (setq indent-tabs-mode nil
      tab-width 4))))

;(defvaralias 'c-basic-offset 'tab-width)
;(defvaralias 'cperl-indent-level 'tab-width)

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs)
  (global-font-lock-mode t)
))

;show column numbs
(column-number-mode 1)

(global-hl-line-mode t)

(setq initial-scratch-message nil)

(tool-bar-mode -1)
(menu-bar-mode -1)

;; Visual feedback on selections
(setq-default transient-mark-mode t)

;;show matching parenthesis
(show-paren-mode t)

;; autocomplete parenthesis
;;(setq electric-pair-mode t)
(electric-pair-mode t)

;; show current function in modeline
(which-function-mode t)

(blink-cursor-mode t)

;;scrolls one line down (like less)
;;uncommented for emacs23.1
;;(setq scroll-conservatively 1)

;;replace current selection with your typing
(delete-selection-mode t)

;;(setq shift-select-mode t)

;; Always end a file with a newline
(setq require-final-newline t)

;; Enable mouse support
(require 'mouse)
(xterm-mouse-mode t)
(setq mouse-sel-mode t)

(put 'upcase-region 'disabled nil)

;; winmove
(windmove-default-keybindings)

;; 24.4. wrap lines
(global-visual-line-mode 1)

;; better buffer switching
(ido-mode 1)

;; python shells
(setq python-shell-completion-native-enable nil)
;; ipython shell
;; disabled 14.05.20180 for corrupting ipython.sqlite
(setq python-shell-interpreter "ipython"
  python-shell-interpreter-args "-i --simple-prompt")

;; multiline editing for ipython 6.5
;; (setq python-shell-interpreter "ipython")
;;       python-shell-interpreter-args "-i --TerminalIPythonApp.interactive_shell_class=rlipython.TerminalInteractiveShell")

;; jupyter
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")

;; default
;; (setq python-shell-interpreter "python3"
;;       python-shell-interpreter-args "-i")

;; ace-window
(setq aw-dispatch-always 1)

;; auto-reload files
(global-auto-revert-mode t)

;; alpha controls transparency on GUI emacs
(set-frame-parameter (selected-frame) 'alpha '(98 98))
(add-to-list 'default-frame-alist '(alpha 98 98))
;; font
;; increase/decrease font for buffer: C-x C-+ , C-x C--
(set-frame-font "Source code pro 15" nil t)
;(set-frame-font "Source code pro 12" nil t)

;; https://www.emacswiki.org/emacs/WinnerMode
(winner-mode 1)
(when (fboundp 'winner-mode)
  (winner-mode 1))

(setq company-mode 1)
(add-hook 'after-init-hook 'global-company-mode)

;; hs-mode
(add-hook 'prog-mode-hook #'hs-minor-mode)

;search case insensitive
(setq case-fold-search t)

(setq comint-buffer-maximum-size 10240)
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-step 1)
(set-variable 'scroll-conservatively 1000)
(setq xterm-mouse-mode 1)


;; Testing
;(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

(setq print-level 1)
(setq print-length 1)
(setq print-circle t)

(setq compilation-scroll-output 'first-error)

;; octave
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-log-done t)
(setq org-agenda-window-setup 'current-window)
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook #'org-mode-export-hook)
(setq org-html-checkbox-type 'html)


(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)

;; eglot
(add-to-list 'eglot-server-programs '((c-mode) "clangd"))
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)


;; eglot java
;; git clone git@github.com:eclipse/eclipse.jdt.ls.git
;; cd eclipse.jdt.ls
;; mvn -Dmaven.test.skip=true clean verify ;; to ignore failing tests
(defconst eclipse-jdt-path
  "/Users/shusso/testing/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_1.5.700.v20200207-2156.jar"
  "Point to eclipse jdt jar.")

(defun my-eglot-eclipse-jdt-contact (interactive)
  "Contact with the jdt server input INTERACTIVE."
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp ":" eclipse-jdt-path))
    (unwind-protect (eglot--eclipse-jdt-contact nil)
      (setenv "CLASSPATH" cp))))

(setcdr (assq 'java-mode eglot-server-programs) #'my-eglot-eclipse-jdt-contact)
(add-hook 'java-mode-hook 'eglot-ensure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           CUSTOMS end            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        KEYBINDINGS start         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mouse scrolling up/down
(global-set-key [mouse-4] (lambda ()
                            (interactive)
                            (scroll-down 1)))
(global-set-key [mouse-5] (lambda ()
                            (interactive)
                            (scroll-up 1)))

;; move around in buffers
;; (global-set-key (kbd "C-x c <left>") 'windmove-left)
;; (global-set-key (kbd "C-c <right>") 'windmove-right)

(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;; switch to previous in buffer
(global-set-key (kbd "C-c l") 'switch-to-previous-buffer)

;; comment out current line
(global-set-key (kbd "C-x /") 'toggle-comment-on-line)

;; keybindings for ace jump
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; hide-show-mode
(global-set-key (kbd "C-c a") 'hs-show-all)
(global-set-key (kbd "C-c h l") 'hs-hide-level)
(global-set-key (kbd "C-c h a") 'hs-hide-all)
(global-set-key (kbd "C-c h b") 'hs-hide-block)
(global-set-key (kbd "C-c h s") 'hs-show-block)

;;magit
(global-set-key (kbd "C-c m") 'magit-status)

;; company
;(global-set-key (kbd "C-o") 'company-complete-common)
(define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)

;;list all frames
(global-set-key (kbd "C-c f f") 'select-frame-by-name)

;;rename frame
(global-set-key (kbd "C-c f n") 'set-frame-name)

;;frames
(global-set-key (kbd "C-c 3 b") '3-buffers)
(global-set-key (kbd "C-c 2 b") '2-buffers)
(global-set-key (kbd "C-x 5 p") 'frame-back)

;;buffer stuff
(global-set-key (kbd "C-x p") 'window-back)
(global-set-key (kbd "C-c b w") 'buffer-menu-other-window)
(global-set-key (kbd "C-c b m") 'buffer-menu)
(global-set-key (kbd "C-c b o") 'switch-to-buffer-other-window)
(global-set-key (kbd "C-c b f") 'switch-to-buffer-other-frame)
(global-set-key (kbd "C-c f m") 'follow-mode)
(global-set-key (kbd "C-c b b") 'balance-windows)

;; buffer-move
(global-set-key (kbd "M-n <up>")     'buf-move-up)
(global-set-key (kbd "M-n <down>")   'buf-move-down)
(global-set-key (kbd "M-n <left>")   'buf-move-left)
(global-set-key (kbd "M-n <right>")  'buf-move-right)
;; (global-set-key (kbd "<S-e-<right>")  'buf-move-right)

;; M-g = goto-line
(global-set-key "\M-g" 'goto-line)

;;enable tags search mode (run etags before this)
(global-set-key "\M-s" 'tags-search)

;; comment out current line
(global-set-key "\M-/" 'comment-line)

;; projectile
(global-set-key (kbd "M-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "M-s") 'projectile-switch-project)


(global-set-key (kbd "C-c d") 'neotree-toggle)

;;(global-set-key (kdb "") 'xref-find-otherw)

;; ace-window ;;
(global-set-key (kbd "M-i") 'ace-window)
(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
	(?m aw-swap-window "Swap Windows")
	(?M aw-move-window "Move Window")
	(?j aw-switch-buffer-in-window "Select Buffer")
	(?n aw-flip-window)
	(?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	(?c aw-split-window-fair "Split Fair Window")
	(?v aw-split-window-vert "Split Vert Window")
	(?b aw-split-window-horz "Split Horz Window")
	(?o delete-other-windows "Delete Other Windows")
	(?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")

;; unbind shell ring history
(define-key comint-mode-map (kbd "M-p") 'nil)
(define-key comint-mode-map (kbd "M-n") 'nil)

;; and rebind them
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

;; unbind common keys ;;
;; term/shell
(define-key term-raw-map (kbd "M-p") nil)
(define-key term-mode-map (kbd "M-p") nil)
(define-key term-raw-map (kbd "M-i") nil)
(define-key term-mode-map (kbd "M-i") nil)

;; (define-key term-raw-map (kbd "C-x o") nil)
;; (define-key term-mode-map (kbd "C-x p") nil)

(define-key term-raw-map (kbd "M-x") nil)
(define-key term-mode-map (kbd "M-x") nil)
(define-key term-mode-map (kbd "C-c SPC") nil)
(define-key term-raw-map (kbd "C-c SPC") nil)
(define-key term-mode-map (kbd "C-c d") nil)
(define-key term-raw-map (kbd "C-c d") nil)
(define-key term-mode-map (kbd "C-c m") nil)
(define-key term-raw-map (kbd "C-c m") nil)
(define-key term-mode-map (kbd "C-c l") nil)
(define-key term-raw-map (kbd "C-c l") nil)
;(define-key shell-mode-map (kbd "C-c C-t") 'elpy-test)
;; compilation
(define-key compilation-mode-map (kbd "M-p") nil)
(define-key compilation-mode-map (kbd "M-n") nil)
(define-key compilation-mode-map (kbd "M-n n") 'compilation-next-error)
(define-key compilation-mode-map (kbd "M-p p") 'compilation-previous-error)
(define-key compilation-mode-map (kbd "M-p x s") 'projectile-run-shell)
;; markdown
(define-key markdown-mode-map (kbd "M-p") nil)
(define-key markdown-mode-map (kbd "M-i") nil)
(define-key markdown-mode-map (kbd "C-c SPC") nil)
(define-key markdown-mode-map (kbd "C-c d") nil)
(define-key markdown-mode-map (kbd "C-c m") nil)
;; org
(define-key org-mode-map (kbd "M-p") nil)
(define-key org-mode-map (kbd "M-i") nil)
(define-key org-mode-map (kbd "C-c SPC") nil)
(define-key org-mode-map (kbd "C-c d") nil)
(define-key org-mode-map (kbd "C-c .") nil)
(define-key org-mode-map (kbd "C-c .") 'org-time-stamp)
(define-key org-mode-map (kbd "C-c m") nil)
(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c C-l") 'org-insert-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c .") 'org-time-stamp)

;; grep
(define-key grep-mode-map (kbd "M-p") nil)

(global-unset-key (kbd "M-s"))

;; so that projectile works
;;(setq mac-option-modifier 'meta)

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        KEYBINDINGS end           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(beacon-color "#cc6666")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "3325e2c49c8cc81a8cc94b0d57f1975e6562858db5de840b03338529c64f58d1" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "fe36e4da2ca97d9d706e569024caa996f8368044a8253dc645782e01cd68d884" "18bec4c258b4b4fb261671cf59197c1c3ba2a7a47cc776915c3e8db3334a0d25" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "40b961730f8d3c63537d6c3e6601f15c6f6381b9239594c7bf80b7c6a94d3c24" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "c5ded9320a346146bbc2ead692f0c63be512747963257f18cc8518c5254b7bf5" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "846b3dc12d774794861d81d7d2dcdb9645f82423565bfb4dad01204fa322dbd5" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "f7fed1aadf1967523c120c4c82ea48442a51ac65074ba544a5aefc5af490893b" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "1f1b545575c81b967879a5dddc878783e6ebcca764e4916a270f9474215289e5" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "3d47380bf5aa650e7b8e049e7ae54cdada54d0637e7bac39e4cc6afb44e8463b" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "6c98bc9f39e8f8fd6da5b9c74a624cbb3782b4be8abae8fd84cbc43053d7c175" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "aa6638f0cd2ba2c68be03220ea73495116dc6f0b625405ede34087c1babb71ae" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db" "c0a0c2f40c110b5b212eb4f2dad6ac9cac07eb70380631151fa75556b0100063" "2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c" "03e26cd42c3225e6376d7808c946f7bed6382d795618a82c8f3838cd2097a9cc" "4133d2d6553fe5af2ce3f24b7267af475b5e839069ba0e5c80416aa28913e89a" "1278c5f263cdb064b5c86ab7aa0a76552082cf0189acf6df17269219ba496053" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "84b14a0a41bb2728568d40c545280dbe7d6891221e7fbe7c2b1c54a3f5959289" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "82ef0ab46e2e421c4bcbc891b9d80d98d090d9a43ae76eb6f199da6a0ce6a348" "b5803dfb0e4b6b71f309606587dd88651efe0972a5be16ece6a958b197caeed8" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "db3e80842b48f9decb532a1d74e7575716821ee631f30267e4991f4ba2ddf56e" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "f490984d405f1a97418a92f478218b8e4bcc188cf353e5dd5d5acd2f8efd0790" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "35c096aa0975d104688a9e59e28860f5af6bb4459fd692ed47557727848e6dfe" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "f4158db802ae689ed0e156cd02c8a3c0e22c5e778578e8eea6d4afc3a9d0e629" "81c3de64d684e23455236abde277cda4b66509ef2c28f66e059aa925b8b12534" "95d0ed21bb0e919be7687a25ad59a1c2c8df78cbe98c9e369d44e65bfd65b167" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "7546a14373f1f2da6896830e7a73674ef274b3da313f8a2c4a79842e8a93953e" "0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "f4876796ef5ee9c82b125a096a590c9891cec31320569fc6ff602ff99ed73dca" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" "ff3c57a5049010a76de8949ddb629d29e2ced42b06098e046def291989a4104a" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "e27556a94bd02099248b888555a6458d897e8a7919fd64278d1f1e8784448941" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "73c69e346ec1cb3d1508c2447f6518a6e582851792a8c0e57a22d6b9948071b4" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "2a7beed4f24b15f77160118320123d699282cbf196e0089f113245d4b729ba5d" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "a77ced882e25028e994d168a612c763a4feb8c4ab67c5ff48688654d0264370c" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "2f4f50d98073c01038b518066840638455657dc91dd1a225286d573926f36914" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "89336ca71dae5068c165d932418a368a394848c3b8881b2f96807405d8c6b5b6" "addfaf4c6f76ef957189d86b1515e9cf9fcd603ab6da795b82b79830eed0b284" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "04589c18c2087cd6f12c01807eed0bdaa63983787025c209b89c779c61c3a4c4" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "db510eb70cf96e3dbd48f5d24de12b03db30674ea0853f06074d4ccf7403d7d3" "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "63aff36a40f41b28b0265ac506faa098fd552fac0a1813b745ba7c27efa5a943" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4" "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36" "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" "1f126eb4a1e5d6b96b3faf494c8c490f1d1e5ad4fc5a1ce120034fe140e77b88" "2ae4b0c50dd49a5f74edeae3e49965bf8853954b63c5712a7967ea0a008ecd5b" "5f4dfda04fbf7fd55228266c8aab73953d3087cea7fd06dd7f8ff1e4a497c739" "3ed2e1653742e5059e3d77af013ee90c1c1b776d83ec33e1a9ead556c19c694b" "b5cff93c3c6ed12d09ce827231b0f5d4925cfda018c9dcf93a2517ce3739e7f1" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "4bcdfc98cf64ce6145684dc8288fd87489cfa839e07f95f6c791d407624d04f8" "335ad769bcd7949d372879ec10105245255beec6e62213213835651e2eb0b8e0" "6c0d748fb584ec4c8a0a1c05ce1ae8cde46faff5587e6de1cc59d3fc6618e164" "6291d73aaeb6a3d7e455d85ca3865260a87afe5f492b6d0e2e391af2d3ea81dd" "01e0367d8c3249928a2e0ebc9807b2f791f81a0d2a7c8656e1fbf4b1dbaa404c" "6b4f7bde1ce64ea4604819fe56ff12cda2a8c803703b677fdfdb603e8b1f8bcb" "cb30d82b05359203c8378638dec5ad6e37333ccdda9dee8b9fdf0c902e83fad7" "28818b9b1d9e58c4fb90825a1b07b0f38286a7d60bf0499bc2dea7eea7e36782" "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043" "70b2d5330a8dd506accac4b51aaa7e43039503d000852d7d152aec2ce779d96d" "995d0754b79c4940d82bd430d7ebecca701a08631ec46ddcd2c9557059758d33" "b6f06081b007b57be61b82fb53f27315e2cf38fa690be50d6d63d2b62a408636" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "9d9b2cf2ced850aad6eda58e247cf66da2912e0722302aaa4894274e0ea9f894" "aaf783d4bfae32af3e87102c456fba8a85b79f6e586f9911795ea79055dee3bf" "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "62a6731c3400093b092b3837cff1cb7d727a7f53059133f42fcc57846cfa0350" "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "1a2cde373eff9ffd5679957c7ecfc6249d353e1ee446d104459e73e924fe0d8a" "a621dd9749f2651e357a61f8d8d2d16fb6cacde3b3784d02151952e1b9781f05" "53de65a1e7300e0f1a4f8bf317530a5008e9d06a0e2f8863b80dc56d77f844cf" "938f120eeda938eef2c36b4cc9609d1ad91b3a3666cd63a4be5b70b739004942" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "bce1c321471d37b875f99c83cb7b451fd8386001259e1c0909d6e078ea60f00b" "f19d195fa336e9904303eea20aad35036b79cfde72fa6e76b7462706acd52920" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "44f5578eccb2cde3b196dfa86a298b75fe39ceff975110c091fa8c874c338b50" "1342a81078bdac27f80b86807b19cb27addc1f9e4c6a637a505ae3ba4699f777" "80a23d559a5c5343a0882664733fd2c9e039b4dbf398c70c424c8d6858b39fc5" "1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb" "68b847fac07094724e552eeaf96fa4c7e20824ed5f3f225cad871b8609d50ace" "a5a2954608aac5c4dcf9659c07132eaf0da25a8f298498a7eacf97e2adb71765" "cc2f32f5ee19cbd7c139fc821ec653804fcab5fcbf140723752156dc23cdb89f" "d422c7673d74d1e093397288d2e02c799340c5dabf70e87558b8e8faa3f83a6c" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7" "daeaa8249f0c275de9e32ed822e82ff40457dabe07347fe06afc67d962a3b1e9" "ff6a8955945028387ed1a2b0338580274609fbb0d40cd011b98ca06bd00d9233" "5e402ccb94e32d7d09e300fb07a62dc0094bb2f16cd2ab8847b94b01b9d5e866" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "4e7e04c4b161dd04dc671fb5288e3cc772d9086345cb03b7f5ed8538905e8e27" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "1127f29b2e4e4324fe170038cbd5d0d713124588a93941b38e6295a58a48b24f" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70" "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "a02c000c95c43a57fe1ed57b172b314465bd11085faf6152d151385065e0e4b1" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "a513bb141af8ece2400daf32251d7afa7813b3a463072020bb14c82fd3a5fe30" "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(ein:output-area-inlined-images t)
 '(elpy-project-ignored-directories
   '(".tox" "build" "dist" ".cask" ".ipynb_checkpoints" ".pytest_cache" ".mypy_cache" ".venv" "build" "dist" ".pyc" "__pycache__"))
 '(elpy-rpc-python-command "python3")
 '(elpy-syntax-check-command "black")
 '(elpy-test-nose-runner-command '("nosetests" "--nologcapture" "--nocapture" "-s"))
 '(elpy-test-pytest-runner-command '("py.test" "--tb=short"))
 '(elpy-test-runner 'elpy-test-pytest-runner)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(exwm-floating-border-color "#d3c5a0")
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#383838")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(frame-brackground-mode 'dark)
 '(fringe-mode 4 nil (fringe))
 '(gnus-logo-colors '("#259ea2" "#adadad") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '((#1="#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     (#1# . 100)))
 '(hl-todo-keyword-faces
   '(("TODO" . #1="#dc752f")
     ("NEXT" . #1#)
     ("THEM" . "#2aa198")
     ("PROG" . #2="#268bd2")
     ("OKAY" . #2#)
     ("DONT" . #3="#d70008")
     ("FAIL" . #3#)
     ("DONE" . "#00af00")
     ("NOTE" . #4="#875f00")
     ("KLUDGE" . #4#)
     ("HACK" . #4#)
     ("TEMP" . #4#)
     ("FIXME" . #1#)
     ("XXX+" . #1#)
     ("\\?\\?\\?+" . #1#)))
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#a89984"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#79740e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#928374"))
 '(line-spacing 0.2)
 '(mac-pass-command-to-system nil)
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style 'chamfer)
 '(markdown-command "pandoc")
 '(nil nil t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#CC9393")
 '(org-agenda-files
   '("~/org/gtd.org" "~/org/links.org" "~/org/notes.org" "~/org/things_i_dont_know.org"))
 '(org-cycle-separator-lines -1)
 '(org-treat-insert-todo-heading-as-state-change t)
 '(package-selected-packages
   '(org-bullets org-contrib quick-peek projectile projectile-codesearch almost-mono-themes arjen-grey-theme atom-dark-theme atom-one-dark-theme autumn-light-theme ayu-theme bash-completion enlightened-theme dracula-theme poet-theme doom-themes gruvbox-theme company-quickhelp-terminal flycheck project eglot buffer-move abyss-theme alect-themes ample-theme ample-zen-theme anti-zenburn-theme afternoon-theme cherry-blossom-theme color-theme-cobalt color-theme-dg color-theme-github color-theme-heroku color-theme-tango cyberpunk-theme gruber-darker-theme hemisu-theme heroku-theme soothe-theme color-theme-sanityinc-solarized spacemacs-theme zenburn-theme monokai-theme color-theme-sanityinc-tomorrow dockerfile-mode swift-mode ein pyvenv ac-octave realgud cython-mode elpy yaml-mode slime rainbow-delimiters markdown-mode magit jedi company-anaconda color-theme-modern ace-window ace-jump-mode))
 '(pdf-view-midnight-colors '("#5f5f87" . "#ffffff"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".env" ".venv" "build" "dist" "parquet" ".pyc" "__pychache__" "resources" ".pytest_cache" ".mypy_cache"))
 '(rustic-ansi-faces
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCDC"])
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-log-types '((emacs)))
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t nil))))
