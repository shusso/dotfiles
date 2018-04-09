;;; notes ;;;
;; C-h v major-mode RET
;; M-x apropos-command -mode$ RET
;; M-: major-mode RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          FUNCTION start          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun frame-back ()
  "move to the previous frame"
  (interactive)
  (other-frame -1))

(defun window-back ()
  "move to previous window in current buffer"
  (interactive)
  (other-window -1))

(defun toggle-comment-on-line()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))


(setq org-todo-keyword-faces
      '(("TODO" . org-warning) 
        ("ONGOING" . "yellow")
        ("ON_HOLD" . (:foreground "purple" :weight bold))
        ("INVALID" . (:foreground "orange" :weight bold))))


(defun hs-hide-leafs-recursive (minp maxp)
  "Hide blocks that do not contain further blocks in region (MINP MAXP)."
  (hs-minor-mode t)
  (when (hs-find-block-beginning)
    (setq minp (1+ (point)))
    (funcall hs-forward-sexp-func 1)
    (setq maxp (1- (point))))
  (unless hs-allow-nesting
    (hs-discard-overlays minp maxp))
  (goto-char minp)
  (let ((leaf t))
    (while (progn
             (forward-comment (buffer-size))
             (and (< (point) maxp)
                  (re-search-forward hs-block-start-regexp maxp t)))
      (setq pos (match-beginning hs-block-start-mdata-select))
      (if (hs-hide-leafs-recursive minp maxp)
          (save-excursion
            (goto-char pos)
            (hs-hide-block-at-point t)))
      (setq leaf nil))
    (goto-char maxp)
    leaf))


(defun hs-hide-leafs ()
  "Hide all blocks that do not contain further blocks. The hook `hs-hide-hook' is run; see `run-hooks'."
  (interactive)
  (hs-minor-mode t)
  (hs-life-goes-on
   (save-excursion
     (message "Hiding blocks ...")
     (save-excursion
       (goto-char (point-min))
       (hs-hide-leafs-recursive (point-min) (point-max)))
     (message "Hiding blocks ... done"))
   (run-hooks 'hs-hide-hook)))

;;function un/fold set to f11. ;was f3
(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun jao-toogle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

;; Open buffers with follow mode
(defun 3-buffers ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode t))

(defun 2-buffers ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode t))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;; GOLANG ;;
(defun auto-complete-for-go ()
  (auto-complete-mode 1))

(defun my-go-mode-hook ()  
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



;; modern color theme
;; https://github.com/emacs-jp/replace-colorthemes
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/color-themes"))
;; M-x customize-face RET hl-line RET

;; package
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

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

;; https://github.com/proofit404/anaconda-mode
(require 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
;; C-M-i	anaconda-mode-complete
;; M-.	    anaconda-mode-find-definitions
;; M-,	    anaconda-mode-find-assignments
;; M-r	    anaconda-mode-find-references
;; M-*	    anaconda-mode-go-back
;; M-?	    anaconda-mode-show-doc


;; company
(require 'company)

(require 'projectile)
(setq-default projectile-mode t)

;; https://github.com/bbatsov/projectile/blob/master/doc/usage.md
;; C-c p C-h    Help
;; C-c p o       Multi Occur
;; C-c p f	    Display a list of all files in the project. With a prefix argument it will clear the cache first.
;; C-c p F	    Display a list of all files in all known projects.
;; C-c p 4 f	Jump to a project's file using completion and show it in another window.
;; C-c p d	    Display a list of all directories in the project. With a prefix argument it will clear the cache first.
;; C-c p 4 d	Switch to a project directory and show it in another window.
;; C-c p 5 d	Switch to a project directory and show it in another frame.
;; C-c p T	    Display a list of all test files(specs, features, etc) in the project.
;; C-c p t      Toggle between implementation and test file
;; C-c p 4 t    Jump to implementation/testfile in other window
;; C-c p 5 t    -''- in other frame
;; C-c p s g	Run grep on the files in the project.
;; C-c p 4 b	Switch to a project buffer and show it in another window.
;; C-c p 5 b	Switch to a project buffer and show it in another frame.
;;
;; Ignore dirs
;; M-x customize-variable [RET]
;; projectile-globally-ignored-files
;; [INS]
;; [ Apply and Save ]

(require 'go-mode)
(require 'go-autocomplete)
(require 'go-guru)

(require 'neotree)
;; n next line ， p previous line。
;; SPC or RET or TAB Open current item if it is a file. Fold/Unfold current item if it is a directory.
;; g Refresh
;; A Maximize/Minimize the NeoTree Window
;; H Toggle display hidden files
;; C-c C-n Create a file or create a directory if filename ends with a ‘/’
;; C-c C-d Delete a file or a directory.
;; C-c C-r Rename a file or a directory.
;; C-c C-c Change the root directory.
;; C-c C-p Copy a file or a directory.

(require 'ace-window)
;x - delete window
;m - swap (move) window
;c - split window fairly, either vertically or horizontally
;v - split window vertically
;b - split window horizontally
;n - select the previous window
;i - maximize window (select which window)
;o - maximize current window

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (eval-after-load 'flycheck (cons 'python-pylint (delq 'python-pylint flycheck-checkers)))
(flycheck-add-next-checker 'python-flake8 'python-pylint)
;; C-c ! s  flycheck-select-checker

(require 'term)
;; C-c C-j  Terminal line-mode
;; C-c C-k  Terminal mode


(require 'ein)
;; start jupyter notebook
;; M-x ein:notebooklist-login (use the token as password)
;; M-x ein:notebooklist-open
(setq print-level 1)
(setq print-length 1)
(setq print-circle t)


(require 'yaml-mode)

(require 'markdown-mode)
;; C-c C-c e  export
;; C-C C-c v  export & preview
;; M-x markdown-export-and-preview RET

(require 'org)

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

;; to fix the stupid python.el 8 tab crap
;; (setq tab-width 4)
;;             (set-variable 'py-indent-offset 4)
;;             (set-variable 'python-indent-guess-indent-offset nil)

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

;; Enable wheelmouse support by default
(cond (window-system
       (mwheel-install)
))
;;finnish language to work properly:
(set-input-mode nil nil 'foo)

(put 'upcase-region 'disabled nil)

;; winmove
(windmove-default-keybindings)


;; 24.4. wrap lines
(global-visual-line-mode 1)

;; hook for golang
(add-hook 'go-mode-hook 'my-go-mode-hook)
;; hook for golang compile
(add-hook 'go-mode-hook 'my-go-mode-hook)

(add-hook 'go-mode-hook 'auto-complete-for-go)

;; (with-eval-after-load 'go-mode
;;    (require 'go-autocomplete))

;; better buffer switching
(ido-mode 1)

;; ipython shell
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

;; C-c C-p    start shell
;; C-c C-c    send current buffer to python
;; C-c C-r    send selected code (C-SPC) to python


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


;; ace-window
(setq aw-dispatch-always 1)

;; auto-reload files
(global-auto-revert-mode t)

(set-frame-parameter (selected-frame) 'alpha '(88 70))
(add-to-list 'default-frame-alist '(alpha 88 70))

;; winner mode
;; https://www.emacswiki.org/emacs/WinnerMode
;; C-x 1     1 window
;; C-x 2     2 windows
;; C-c left  go to previous (backward)
;; C-c right go to previous (forward)

(winner-mode 1)
(when (fboundp 'winner-mode)
  (winner-mode 1))


(setq company-mode 1)
(add-hook 'after-init-hook 'global-company-mode)


;; (eval-after-load "company"
;;   '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))


;search case insensitive
(setq case-fold-search t)

(setq comint-buffer-maximum-size 10240)
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-step 1)
(set-variable 'scroll-conservatively 1000)
(setq xterm-mouse-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           CUSTOMS end            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        KEYBINDINGS start         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move around in buffers
(global-set-key (kbd "C-c <left>") 'windmove-left) 
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;; switch to previous in buffer
(global-set-key (kbd "C-c l") 'switch-to-previous-buffer)

;; comment out current line
(global-set-key (kbd "C-x /") 'toggle-comment-on-line)

;; keybindings for ace jump
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; hide/show blocks
(global-set-key (kbd "C-c k") 'hs-hide-leafs-recursive)
(global-set-key (kbd "C-c h") 'hs-hide-leafs)
(global-set-key (kbd "C-c a") 'hs-show-all)
(global-set-key [f11] 'jao-toggle-selective-display)

;;grep
(global-set-key (kbd "C-c g") 'grep)
(global-set-key (kbd "C-c n") 'occur-next-error)
(global-set-key (kbd "C-c b n") 'next-buffer)
(global-set-key (kbd "C-c b p") 'previous-buffer)

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

;; M-g = goto-line
(global-set-key "\M-g" 'goto-line)

;;enable tags search mode (run etags before this)
(global-set-key "\M-s" 'tags-search)

;; comment out current line
(global-set-key "\M-/" 'comment-line)

;; projectile
(global-set-key (kbd "M-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "?\s-d") 'projectile-find-dir)
(define-key projectile-mode-map [?\s-g] 'projectile-grep)

;; neotree
(global-set-key (kbd "C-c d") 'neotree-toggle)
(global-set-key (kbd "C-c j") 'neotree-dir)

;; ace-window ;;
(global-set-key (kbd "M-i") 'ace-window)


;; unbind shell ring history
(define-key comint-mode-map (kbd "M-p") 'nil)
(define-key comint-mode-map (kbd "M-n") 'nil)

;; and rebind them
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

;; unbind common keys in shell modes
(define-key term-raw-map (kbd "M-p") nil)
(define-key term-mode-map (kbd "M-p") nil)
(define-key term-raw-map (kbd "M-i") nil)
(define-key term-mode-map (kbd "M-i") nil)
(define-key term-raw-map (kbd "M-x") nil)
(define-key term-mode-map (kbd "M-x") nil)
(define-key term-mode-map (kbd "C-c SPC") nil)
(define-key term-raw-map (kbd "C-c SPC") nil)
(define-key term-mode-map (kbd "C-c d") nil)
(define-key term-raw-map (kbd "C-c d") nil)
(define-key term-mode-map (kbd "C-c m") nil)
(define-key term-raw-map (kbd "C-c m") nil)
(define-key markdown-mode-map (kbd "M-p") nil)
(define-key markdown-mode-map (kbd "M-i") nil)
(define-key markdown-mode-map (kbd "C-c SPC") nil)
(define-key markdown-mode-map (kbd "C-c d") nil)
(define-key markdown-mode-map (kbd "C-c m") nil)
(define-key org-mode-map (kbd "M-p") nil)
(define-key org-mode-map (kbd "M-i") nil)
(define-key org-mode-map (kbd "C-c SPC") nil)
(define-key org-mode-map (kbd "C-c d") nil)
(define-key org-mode-map (kbd "C-c m") nil)


(global-unset-key (kbd "M-s"))
;; python pdb
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Commands-of-GUD.html
;; M-x pdb <RET> Run pdb (like this): python3 -m pdb foo.py
;; C-x C-a C-b breakpoint
;; C-x C-a C-d remove breakpoint

;; M-a Move back to the beginning of the sentence (backward-sentence)
;; M-e Move forward to the end of the sentence (forward-sentence).
;; M-k Kill forward to the end of the sentence (kill-sentence).
;; C-x <del> Kill back to the beginning of the sentence (backward-kill-sentence)

;; M-c capitalize
;; M-; comment end of the line

(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        KEYBINDINGS end           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   (quote
    ("77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "db510eb70cf96e3dbd48f5d24de12b03db30674ea0853f06074d4ccf7403d7d3" "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "63aff36a40f41b28b0265ac506faa098fd552fac0a1813b745ba7c27efa5a943" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4" "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36" "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" "1f126eb4a1e5d6b96b3faf494c8c490f1d1e5ad4fc5a1ce120034fe140e77b88" "2ae4b0c50dd49a5f74edeae3e49965bf8853954b63c5712a7967ea0a008ecd5b" "5f4dfda04fbf7fd55228266c8aab73953d3087cea7fd06dd7f8ff1e4a497c739" "3ed2e1653742e5059e3d77af013ee90c1c1b776d83ec33e1a9ead556c19c694b" "b5cff93c3c6ed12d09ce827231b0f5d4925cfda018c9dcf93a2517ce3739e7f1" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "4bcdfc98cf64ce6145684dc8288fd87489cfa839e07f95f6c791d407624d04f8" "335ad769bcd7949d372879ec10105245255beec6e62213213835651e2eb0b8e0" "6c0d748fb584ec4c8a0a1c05ce1ae8cde46faff5587e6de1cc59d3fc6618e164" "6291d73aaeb6a3d7e455d85ca3865260a87afe5f492b6d0e2e391af2d3ea81dd" "01e0367d8c3249928a2e0ebc9807b2f791f81a0d2a7c8656e1fbf4b1dbaa404c" "6b4f7bde1ce64ea4604819fe56ff12cda2a8c803703b677fdfdb603e8b1f8bcb" "cb30d82b05359203c8378638dec5ad6e37333ccdda9dee8b9fdf0c902e83fad7" "28818b9b1d9e58c4fb90825a1b07b0f38286a7d60bf0499bc2dea7eea7e36782" "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043" "70b2d5330a8dd506accac4b51aaa7e43039503d000852d7d152aec2ce779d96d" "995d0754b79c4940d82bd430d7ebecca701a08631ec46ddcd2c9557059758d33" "b6f06081b007b57be61b82fb53f27315e2cf38fa690be50d6d63d2b62a408636" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "9d9b2cf2ced850aad6eda58e247cf66da2912e0722302aaa4894274e0ea9f894" "aaf783d4bfae32af3e87102c456fba8a85b79f6e586f9911795ea79055dee3bf" "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "62a6731c3400093b092b3837cff1cb7d727a7f53059133f42fcc57846cfa0350" "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "1a2cde373eff9ffd5679957c7ecfc6249d353e1ee446d104459e73e924fe0d8a" "a621dd9749f2651e357a61f8d8d2d16fb6cacde3b3784d02151952e1b9781f05" "53de65a1e7300e0f1a4f8bf317530a5008e9d06a0e2f8863b80dc56d77f844cf" "938f120eeda938eef2c36b4cc9609d1ad91b3a3666cd63a4be5b70b739004942" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "bce1c321471d37b875f99c83cb7b451fd8386001259e1c0909d6e078ea60f00b" "f19d195fa336e9904303eea20aad35036b79cfde72fa6e76b7462706acd52920" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "44f5578eccb2cde3b196dfa86a298b75fe39ceff975110c091fa8c874c338b50" "1342a81078bdac27f80b86807b19cb27addc1f9e4c6a637a505ae3ba4699f777" "80a23d559a5c5343a0882664733fd2c9e039b4dbf398c70c424c8d6858b39fc5" "1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb" "68b847fac07094724e552eeaf96fa4c7e20824ed5f3f225cad871b8609d50ace" "a5a2954608aac5c4dcf9659c07132eaf0da25a8f298498a7eacf97e2adb71765" "cc2f32f5ee19cbd7c139fc821ec653804fcab5fcbf140723752156dc23cdb89f" "d422c7673d74d1e093397288d2e02c799340c5dabf70e87558b8e8faa3f83a6c" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7" "daeaa8249f0c275de9e32ed822e82ff40457dabe07347fe06afc67d962a3b1e9" "ff6a8955945028387ed1a2b0338580274609fbb0d40cd011b98ca06bd00d9233" "5e402ccb94e32d7d09e300fb07a62dc0094bb2f16cd2ab8847b94b01b9d5e866" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "4e7e04c4b161dd04dc671fb5288e3cc772d9086345cb03b7f5ed8538905e8e27" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "1127f29b2e4e4324fe170038cbd5d0d713124588a93941b38e6295a58a48b24f" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70" "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "a02c000c95c43a57fe1ed57b172b314465bd11085faf6152d151385065e0e4b1" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "a513bb141af8ece2400daf32251d7afa7813b3a463072020bb14c82fd3a5fe30" "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" default)))
 '(package-selected-packages
   (quote
    (yaml-mode slime rainbow-delimiters neotree markdown-mode magit jedi go-projectile go-complete go-autocomplete flycheck ein csharp-mode company-anaconda color-theme-modern ace-window ace-jump-mode)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".env" "build" "dist"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t nil))))
