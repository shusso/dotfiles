;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TODO               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; grep option
;;; path
;;; exclude dirs
; hide show

;; c-x {}  ;; in/decrease split screen size
;; c-x z   ;; repeat previous command
;;     z   ;; pressin this will repeat forever


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TEST               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set-specifier default-toolbar-visible-p nil)
;;(set-specifier default-toolbar-visible-p nil)
;;(set-specifier bottom-toolbar-visible-p nil)
;; (set-specifier left-toolbar-visible-p nil)
;; (set-specifier right-toolbar-visible-p nil)
;; (set-specifier top-toolbar-visible-p nil)


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           FUNCTION end           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           MODES start            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp")

;; package
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; requires
;; http://blogs.unity3d.com/2010/01/15/emacs-mode-for-unity-javascript/
(require 'unityjs-mode)
(autoload 'unityjs-mode "unityjs-mode" "Major mode for editing Unity Javascript code." t)


;;requires
;;http://cx4a.org/software/auto-complete/
(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto_comp/ac-dict")
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'web-mode)
(add-hook 'python-mode-hook 'auto-complete-mode)

;; requires  enh-ruby
(setq enh-ruby-program "~/.rvm/rubies/default/bin/ruby")
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; requires robe
;; use M-x robe-mode ;; M-x robe-start
(add-hook 'ruby-mode-hook 'robe-mode)


;; requires ace-jump
;; https://github.com/winterTTr/ace-jump-mode
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(autoload 'ace-jump-pop-mark "ace-jump-mode" "Ace jump back" t)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))

;; requires webmode
;; http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; requires
;; http://www.emacswiki.org/emacs/CSharpMode
;(require 'csharp-mode)

(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(defun my-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
; Set indentation level to 4 spaces (instead of 2)
  (setq c-basic-offset 4)
; Set the extra indentation before a substatement (e.g. the opening brace in
; the consequent block of an if statement) to 0 (instead of '+)
  (c-set-offset 'substatement-open 0)
)
(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)


;; (setq auto-mode-alist
;;       (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


(require 'linum)

;; requires
;; http://chrispoole.com/project/ac-python/
;;(require 'ac-python)



;; requires:
;; https://github.com/magit/magit
(add-to-list 'load-path "~/.emacs.d/magit")
(require 'magit)
(require 'magit-svn)


;; To use psvn.el put the following line in your .emacs:
(require 'psvn)

;search case insensitive
(setq case-fold-search t)

(setq comint-buffer-maximum-size 10240)
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-step 1)
(set-variable 'scroll-conservatively 1000)
(setq xterm-mouse-mode 1)
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

;; auto complete
;; (require 'ac-cider)
;; (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;; (add-hook 'cider-mode-hook 'ac-cider-setup)
;; (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (add-to-list 'ac-modes 'cider-mode)
;;           (add-to-list 'ac-modes 'cider-repl-mode)))
;; company (auto complete)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(global-set-key (kbd "C-i") #'company-complete)


;; Poping-up contextual documentation
(eval-after-load "cider"
    '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; paredit for formatting clojure s-expressions
(add-hook 'clojure-mode-hook 'paredit-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode) ;; only for clojure-mode


;; http://tkf.github.io/emacs-jedi/latest/
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(require 'projectile)
(setq-default projectile-mode t)

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

;;(tool-bar-mode nil)
;;(menu-bar-mode nil)

;; Visual feedback on selections
(setq-default transient-mark-mode t)

;;show matching parenthesis
(show-paren-mode t)

;; show current function in modeline
(which-func-mode t)

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
(global-set-key (kbd "C-c o") 'other-frame)
(global-set-key (kbd "C-c p") 'frame-back)

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


;;
;; find-tags M-.
;;             list: tab or shift-/
;; next hit M-,


;; not worky yetii
;; (defun open-from-remote (user ip)
;;   "open file from remote server"
;;   (interactive)
;;   (find-file "/"))
;; (global-set-key (kbd "C-c r f") 'open-from-remote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        KEYBINDINGS end           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
;;   ;; Your init file should contain only one such instance.
;;  '(auto-compression-mode t nil (jka-compr))
;;  '(case-fold-search t)
;;  '(current-language-environment "UTF-8")
;;  '(default-input-method "rfc1345")
;;  '(global-font-lock-mode t nil (font-lock))
;;  '(show-paren-mode t nil (paren))
;;  '(tool-bar-mode nil nil (tool-bar))
;;  '(transient-mark-mode t))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
;;   ;; Your init file should contain only one such instance.
;;  '(default ((t (:stipple nil :background "black" :foreground "green1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :family "adobe-courier"))))
;;  '(region ((((class color) (background light)) (:background "white" :foreground "white" :box (:line-width 2 :color "grey75" :style released-button) :overline "white")))))



;;;; TESTING
;; (custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 ;; '(column-number-mode t)
 ;; '(global-hl-line-mode t)
 ;; '(inhibit-startup-screen t)
 ;; '(mouse-highlight t)
 ;; '(show-paren-mode t))
 ;; '(setq initial-scratch-message nil)
 ;; '(setq-default transient-mark-mode t)

;;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 ;;'(default ((t (:stipple nil :background "black" :foreground "green1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "xos4" :family "Terminus")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 4)
 '(package-selected-packages
   (quote
    (go-projectile projectile go-mode jedi slime rainbow-delimiters auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
