
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
      '(("TODO" . (:background "color-197" :foreground "color-229" :weight demibold :slant italic))
        ("ONGOING" . "color-100")
        ("ON_HOLD" . (:foreground "color-131" :weight bold))
        ("INVALID" . (:foreground "orange" :weight bold))
        ("BLOCKED" . (:foreground "color-124" :weight bold))
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

;; GOLANG ;;
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

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           FUNCTION end           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           MODES start            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; modern color theme
;; https://github.com/emacs-jp/replace-colorthemes
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/color-themes"))
;; M-x customize-face RET hl-line RET

;; package
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages") t)
(add-to-list 'package-archives '("orgmode". "https://orgmode.org/elpa/") t)

(require 'auto-complete)

;; requires ace-jump
;; https://github.com/winterTTr/ace-jump-mode
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(autoload 'ace-jump-pop-mark "ace-jump-mode" "Ace jump back" t)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))

(require 'linum)
(setq linum-format "%4d \u2502 ")

;; to open file from remote server do the following
;; C-x f
;; old: /ssh root@_ip_:/root/
;; /root@_ip_:/root/
(require 'tramp)
(setq tramp-default-method "scp")

;; slime
(require 'slime)
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

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode) ;; only for clojure-mode

;;elpy
(require 'elpy)                         ;
(elpy-enable)
;;http://elpy.readthedocs.io/en/latest/ide.html

;; company
(require 'company)

;; https://github.com/bbatsov/projectile
(require 'projectile)
(setq-default projectile-mode t)

;; https://github.com/jaypei/emacs-neotree
(require 'neotree)

;; https://github.com/abo-abo/ace-window
(require 'ace-window)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;;(eval-after-load 'flycheck (cons 'python-pylint (delq 'python-pylint flycheck-checkers)))
(flycheck-add-next-checker 'python-flake8 'python-pylint)
(setq flycheck-check-syntax-automatically '(save mode-enable))
;; the default value was '(save idle-change new-line mode-enabled)

;;(require 'flymake)
;;(setq flymake-no-changes-timeout nil)

(require 'term)

(require 'ein)
(require 'ein-output-area)
(setq ein:output-area-inlined-images t)

(setq print-level 1)
(setq print-length 1)
(setq print-circle t)

(require 'yaml-mode)

(require 'markdown-mode)

(require 'org)
(require 'org-agenda)

;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; '(org-bullets-bullet-list (quote ("◈" "◆" "◊" "✸")))

(require 'buffer-move)

;; Allow automatically handing of created/expired meta data.
;; https://stackoverflow.com/a/13285957
(require 'org-expiry)
;; Configure it a bit to my liking
(setq
  org-expiry-created-property-name "CREATED" ; Name of property when an item is created
  org-expiry-inactive-timestamps   t         ; Don't have everything in the agenda view
)

(defun mrb/insert-created-timestamp()
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (org-expiry-insert-created)
  (org-back-to-heading)
  (org-end-of-line)
  (insert " ")
)

;; Whenever a TODO entry is created, I want a timestamp
;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
(defadvice org-insert-todo-heading (after mrb/created-timestamp-advice activate)
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

;; set default auto intend to 4 spaces
;; Note! intend for modes have to be set seperately
(setq default-tab-width 4)

;; set auto intend in C and C++ mode to 4 spaces
(add-hook 'c-mode-hook (lambda () (setq c-basic-offset 4)))
(add-hook 'c++-mode-hook (lambda () (setq c-basic-offset 4)))
(setq c-indent-level 4)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

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
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")

;; multiline editing for ipython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --TerminalIPythonApp.interactive_shell_class=rlipython.TerminalInteractiveShell")

;; jupyter
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")

;; default
;; (setq python-shell-interpreter "python3"
;;       python-shell-interpreter-args "-i")

;; patch python shell in emacs 25
;; (with-eval-after-load 'python
;;   (defun python-shell-completion-native-try ()
;;     "Return non-nil if can trigger native completion."
;;     (let ((python-shell-completion-native-enable t)
;;           (python-shell-completion-native-output-timeout
;;            python-shell-completion-native-try-output-timeout))
;;       (python-shell-completion-native-get-completions
;;        (get-buffer-process (current-buffer))
;;        nil "_"))))

;; elpy ;;
(delete `elpy-module-highlight-indentation elpy-modules)

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

;;completions
(global-set-key (kbd "C-j") 'dabbrev-completion)
(global-set-key (kbd "C-o") 'dabbrev-expand)

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

;; neotree
(global-set-key (kbd "C-c d") 'neotree-toggle)
(global-set-key (kbd "C-c j") 'neotree-dir)

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
(define-key shell-mode-map (kbd "C-c C-t") 'elpy-test)
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

;; (global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
;; (defun un-indent-by-removing-4-spaces ()
;;   "remove 4 spaces from beginning of of line"
;;   (interactive)
;;   (save-excursion
;;     (save-match-data
;;       (beginning-of-line)
;;       ;; get rid of tabs at beginning of line
;;       (when (looking-at "^\\s-+")
;;         (untabify (match-beginning 0) (match-end 0)))
;;       (when (looking-at "^    ")
;;         (replace-match "")))))


;;(electric-indent-mode +1)



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
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#bcbcbc" "#d70008" "#5faf00" "#875f00" "#268bd2" "#800080" "#008080" "#5f5f87"])
 '(beacon-color "#cc6666")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "73c69e346ec1cb3d1508c2447f6518a6e582851792a8c0e57a22d6b9948071b4" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "2a7beed4f24b15f77160118320123d699282cbf196e0089f113245d4b729ba5d" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "a77ced882e25028e994d168a612c763a4feb8c4ab67c5ff48688654d0264370c" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "2f4f50d98073c01038b518066840638455657dc91dd1a225286d573926f36914" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "89336ca71dae5068c165d932418a368a394848c3b8881b2f96807405d8c6b5b6" "addfaf4c6f76ef957189d86b1515e9cf9fcd603ab6da795b82b79830eed0b284" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "04589c18c2087cd6f12c01807eed0bdaa63983787025c209b89c779c61c3a4c4" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "db510eb70cf96e3dbd48f5d24de12b03db30674ea0853f06074d4ccf7403d7d3" "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "63aff36a40f41b28b0265ac506faa098fd552fac0a1813b745ba7c27efa5a943" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4" "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36" "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" "1f126eb4a1e5d6b96b3faf494c8c490f1d1e5ad4fc5a1ce120034fe140e77b88" "2ae4b0c50dd49a5f74edeae3e49965bf8853954b63c5712a7967ea0a008ecd5b" "5f4dfda04fbf7fd55228266c8aab73953d3087cea7fd06dd7f8ff1e4a497c739" "3ed2e1653742e5059e3d77af013ee90c1c1b776d83ec33e1a9ead556c19c694b" "b5cff93c3c6ed12d09ce827231b0f5d4925cfda018c9dcf93a2517ce3739e7f1" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "4bcdfc98cf64ce6145684dc8288fd87489cfa839e07f95f6c791d407624d04f8" "335ad769bcd7949d372879ec10105245255beec6e62213213835651e2eb0b8e0" "6c0d748fb584ec4c8a0a1c05ce1ae8cde46faff5587e6de1cc59d3fc6618e164" "6291d73aaeb6a3d7e455d85ca3865260a87afe5f492b6d0e2e391af2d3ea81dd" "01e0367d8c3249928a2e0ebc9807b2f791f81a0d2a7c8656e1fbf4b1dbaa404c" "6b4f7bde1ce64ea4604819fe56ff12cda2a8c803703b677fdfdb603e8b1f8bcb" "cb30d82b05359203c8378638dec5ad6e37333ccdda9dee8b9fdf0c902e83fad7" "28818b9b1d9e58c4fb90825a1b07b0f38286a7d60bf0499bc2dea7eea7e36782" "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043" "70b2d5330a8dd506accac4b51aaa7e43039503d000852d7d152aec2ce779d96d" "995d0754b79c4940d82bd430d7ebecca701a08631ec46ddcd2c9557059758d33" "b6f06081b007b57be61b82fb53f27315e2cf38fa690be50d6d63d2b62a408636" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "9d9b2cf2ced850aad6eda58e247cf66da2912e0722302aaa4894274e0ea9f894" "aaf783d4bfae32af3e87102c456fba8a85b79f6e586f9911795ea79055dee3bf" "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "62a6731c3400093b092b3837cff1cb7d727a7f53059133f42fcc57846cfa0350" "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "1a2cde373eff9ffd5679957c7ecfc6249d353e1ee446d104459e73e924fe0d8a" "a621dd9749f2651e357a61f8d8d2d16fb6cacde3b3784d02151952e1b9781f05" "53de65a1e7300e0f1a4f8bf317530a5008e9d06a0e2f8863b80dc56d77f844cf" "938f120eeda938eef2c36b4cc9609d1ad91b3a3666cd63a4be5b70b739004942" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "bce1c321471d37b875f99c83cb7b451fd8386001259e1c0909d6e078ea60f00b" "f19d195fa336e9904303eea20aad35036b79cfde72fa6e76b7462706acd52920" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "44f5578eccb2cde3b196dfa86a298b75fe39ceff975110c091fa8c874c338b50" "1342a81078bdac27f80b86807b19cb27addc1f9e4c6a637a505ae3ba4699f777" "80a23d559a5c5343a0882664733fd2c9e039b4dbf398c70c424c8d6858b39fc5" "1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb" "68b847fac07094724e552eeaf96fa4c7e20824ed5f3f225cad871b8609d50ace" "a5a2954608aac5c4dcf9659c07132eaf0da25a8f298498a7eacf97e2adb71765" "cc2f32f5ee19cbd7c139fc821ec653804fcab5fcbf140723752156dc23cdb89f" "d422c7673d74d1e093397288d2e02c799340c5dabf70e87558b8e8faa3f83a6c" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7" "daeaa8249f0c275de9e32ed822e82ff40457dabe07347fe06afc67d962a3b1e9" "ff6a8955945028387ed1a2b0338580274609fbb0d40cd011b98ca06bd00d9233" "5e402ccb94e32d7d09e300fb07a62dc0094bb2f16cd2ab8847b94b01b9d5e866" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "4e7e04c4b161dd04dc671fb5288e3cc772d9086345cb03b7f5ed8538905e8e27" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "1127f29b2e4e4324fe170038cbd5d0d713124588a93941b38e6295a58a48b24f" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70" "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "a02c000c95c43a57fe1ed57b172b314465bd11085faf6152d151385065e0e4b1" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "a513bb141af8ece2400daf32251d7afa7813b3a463072020bb14c82fd3a5fe30" "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(elpy-project-ignored-directories
   (quote
    (".tox" "build" "dist" ".cask" ".ipynb_checkpoints" ".pytest_cache" ".mypy_cache" ".venv" "build" "dist" ".pyc" "__pycache__")))
 '(elpy-rpc-python-command "python3")
 '(elpy-syntax-check-command "black")
 '(elpy-test-nose-runner-command (quote ("nosetests" "--nologcapture" "--nocapture" "-s")))
 '(elpy-test-pytest-runner-command (quote ("py.test" "--tb=short")))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };")))
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#202020")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 4 nil (fringe))
 '(gnus-logo-colors (quote ("#259ea2" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    ((#1="#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     (#1# . 100))))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . #1="#dc752f")
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
     ("\\?\\?\\?+" . #1#))))
 '(linum-format (quote dynamic))
 '(mac-pass-command-to-system nil)
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(markdown-command "pandoc")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-cycle-separator-lines -1)
 '(package-selected-packages
   (quote
    (buffer-move abyss-theme alect-themes ample-theme ample-zen-theme anti-zenburn-theme afternoon-theme cherry-blossom-theme color-theme-cobalt color-theme-dg color-theme-github color-theme-heroku color-theme-tango cyberpunk-theme gruber-darker-theme hemisu-theme heroku-theme soothe-theme color-theme-sanityinc-solarized spacemacs-theme zenburn-theme monokai-theme color-theme-sanityinc-tomorrow dockerfile-mode flycheck-swift3 swift-mode ein pyvenv ac-octave realgud cython-mode elpy yaml-mode slime rainbow-delimiters neotree markdown-mode magit jedi flycheck company-anaconda color-theme-modern ace-window ace-jump-mode)))
 '(pdf-view-midnight-colors (quote ("#5f5f87" . "#ffffff")))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".env" ".venv" "build" "dist" "parquet" ".pyc" "__pychache__" "resources" ".pytest_cache" ".mypy_cache")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
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
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t nil))))
