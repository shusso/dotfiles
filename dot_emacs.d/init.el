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
;; (defconst eclipse-jdt-path
;;   "/Users/shusso/testing/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_1.5.700.v20200207-2156.jar"
;;   "Point to eclipse jdt jar.")

;; (defun my-eglot-eclipse-jdt-contact (interactive)
;;   "Contact with the jdt server input INTERACTIVE."
;;   (let ((cp (getenv "CLASSPATH")))
;;     (setenv "CLASSPATH" (concat cp ":" eclipse-jdt-path))
;;     (unwind-protect (eglot--eclipse-jdt-contact nil)
;;       (setenv "CLASSPATH" cp))))

;; (setcdr (assq 'java-mode eglot-server-programs) #'my-eglot-eclipse-jdt-contact)
;; (add-hook 'java-mode-hook 'eglot-ensure)


;; gptel
(setq gptel-model 'gpt-4o
      gptel-backend (gptel-make-gh-copilot "Copilot"))


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
 '(beacon-color "#cc6666")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a"
     "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde"
     default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(ein:output-area-inlined-images t)
 '(elpy-project-ignored-directories
   '(".tox" "build" "dist" ".cask" ".ipynb_checkpoints" ".pytest_cache"
     ".mypy_cache" ".venv" "build" "dist" ".pyc" "__pycache__"))
 '(elpy-rpc-python-command "python3")
 '(elpy-syntax-check-command "black")
 '(elpy-test-nose-runner-command '("nosetests" "--nologcapture" "--nocapture" "-s"))
 '(elpy-test-pytest-runner-command '("py.test" "--tb=short"))
 '(elpy-test-runner 'elpy-test-pytest-runner)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data
           "/* XPM */\12static char *note[] = {\12/* width height num_colors chars_per_pixel */\12\"    10   11        2            1\",\12/* colors */\12\". c #358d8d\",\12\"# c None s None\",\12/* pixels */\12\"###...####\",\12\"###.#...##\",\12\"###.###...\",\12\"###.#####.\",\12\"###.#####.\",\12\"#...#####.\",\12\"....#####.\",\12\"#..######.\",\12\"#######...\",\12\"######....\",\12\"#######..#\" };"))
 '(exwm-floating-border-color "#d3c5a0")
 '(fci-rule-character-color "#202020")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(frame-brackground-mode 'dark)
 '(fringe-mode 4 nil (fringe))
 '(gnus-logo-colors '("#259ea2" "#adadad") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data
           "/* XPM */\12static char *gnus-pointer[] = {\12/* width height num_colors chars_per_pixel */\12\"    18    13        2            1\",\12/* colors */\12\". c #358d8d\",\12\"# c None s None\",\12/* pixels */\12\"##################\",\12\"######..##..######\",\12\"#####........#####\",\12\"#.##.##..##...####\",\12\"#...####.###...##.\",\12\"#..###.######.....\",\12\"#####.########...#\",\12\"###########.######\",\12\"####.###.#..######\",\12\"######..###.######\",\12\"###....####.######\",\12\"###..######.######\",\12\"###########.######\" };") t)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '((#1="#3C3D37" . 0) ("#679A01" . 20) ("#4BBEAE" . 30)
     ("#1DB4D0" . 50) ("#9A8F21" . 60) ("#A75B00" . 70)
     ("#F309DF" . 85) (#1# . 100)))
 '(hl-todo-keyword-faces
   '(("TODO" . #1="#dc752f") ("NEXT" . #1#) ("THEM" . "#2aa198")
     ("PROG" . #2="#268bd2") ("OKAY" . #2#) ("DONT" . #3="#d70008")
     ("FAIL" . #3#) ("DONE" . "#00af00") ("NOTE" . #4="#875f00")
     ("KLUDGE" . #4#) ("HACK" . #4#) ("TEMP" . #4#) ("FIXME" . #1#)
     ("XXX+" . #1#) ("\\?\\?\\?+" . #1#)))
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
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3"
     "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#CC9393")
 '(org-agenda-files
   '("~/org/gtd.org" "~/org/links.org" "~/org/notes.org"
     "~/org/things_i_dont_know.org"))
 '(org-cycle-separator-lines -1)
 '(org-treat-insert-todo-heading-as-state-change t)
 '(package-selected-packages
   '(abyss-theme ac-octave ace-jump-mode ace-window afternoon-theme
                 alect-themes almost-mono-themes ample-theme
                 ample-zen-theme anti-zenburn-theme arjen-grey-theme
                 atom-dark-theme atom-one-dark-theme
                 autumn-light-theme ayu-theme bash-completion
                 buffer-move cherry-blossom-theme color-theme-cobalt
                 color-theme-dg color-theme-github color-theme-heroku
                 color-theme-modern color-theme-sanityinc-solarized
                 color-theme-sanityinc-tomorrow color-theme-tango
                 company-anaconda company-quickhelp-terminal
                 cyberpunk-theme cython-mode dockerfile-mode
                 doom-themes dracula-theme eglot ein elpy
                 enlightened-theme flycheck gptel gruber-darker-theme
                 gruvbox-theme hemisu-theme heroku-theme jedi magit
                 markdown-mode monokai-theme org-bullets org-contrib
                 poet-theme project projectile projectile-codesearch
                 pyvenv quick-peek rainbow-delimiters realgud slime
                 soothe-theme spacemacs-theme swift-mode yaml-mode
                 zenburn-theme))
 '(pdf-view-midnight-colors '("#5f5f87" . "#ffffff"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox"
     ".svn" ".stack-work" ".env" ".venv" "build" "dist" "parquet"
     ".pyc" "__pychache__" "resources" ".pytest_cache" ".mypy_cache"))
 '(rustic-ansi-faces
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3"
    "#DCDCDC"])
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672") (40 . "#CF4F1F") (60 . "#C26C0F") (80 . "#E6DB74")
     (100 . "#AB8C00") (120 . "#A18F00") (140 . "#989200")
     (160 . "#8E9500") (180 . "#A6E22E") (200 . "#729A1E")
     (220 . "#609C3C") (240 . "#4E9D5B") (260 . "#3C9F79")
     (280 . "#A1EFE4") (300 . "#299BA6") (320 . "#2896B5")
     (340 . "#2790C3") (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-log-types '((emacs)))
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D"
                 "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF"
                 "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2"
                 "#F8F8F0"))
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t nil))))
