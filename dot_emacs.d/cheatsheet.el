;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  GENERAL                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-h v         major-mode RET
;; M-x           apropos-command -mode$ RET
;; M-:           major-mode RET
;; C-h k <key>   describe function bound to <key>
;; C-h b         list the current key-bindings for the focus buffer
;; C-h m         describe mode
;; C-h l         show the keys you have pressed
;; <prefix> C-h  show all key-bindings beginning with <prefix>
;; M-c capitalize
;; M-; comment end of the line
;; M-a Move back to the beginning of the sentence (backward-sentence)
;; M-e Move forward to the end of the sentence (forward-sentence).
;; M-k Kill forward to the end of the sentence (kill-sentence).
;; C-x <del> Kill back to the beginning of the sentence (backward-kill-sentence)


;;;; TERM ;;;;
;; C-c C-j  Terminal line-mode
;; C-c C-k  Terminal mode


;;;; MARKDOWN-MODE ;;;;
;; C-c C-c e  export
;; C-C C-c v  export & preview
;; M-x markdown-export-and-preview RET


;;;; PYTHON-SHELL ;;;;
;; C-c C-p    start shell
;; C-c C-c    send current buffer to python
;; C-c C-r    send selected code (C-SPC) to python


;;;; GDP ;;;;
;; M-x pdb <RET> Run pdb (like this): python3 -m pdb foo.py
;; C-x C-a C-b breakpoint
;; C-x C-a C-d remove breakpoint


;;;; WINNER-MODE ;;;;
;; https://www.emacswiki.org/emacs/WinnerMode
;; C-x 1     1 window
;; C-x 2     2 windows
;; C-c left  go to previous (backward)
;; C-c right go to previous (forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    ELPY                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://elpy.readthedocs.io/en/latest/ide.html
;; M-x elpy-config                check config
;; M-x pyenv-activate/deactivate  activate venv
;; M-x pyenv-workon               autocomplete existing venvs

;; Keys
;; C-c C-f (elpy-find-file)
;; C-c C-s (elpy-rgrep-symbol)
;; C-x 4 M-. (elpy-goto-definition-other-window)
;; C-c C-o (elpy-occur-definitions)  list of definitions of classes and functions.
;; M-. (xref-find-definitions)
;; C-x 4 . (xref-find-definition-other-window)
;; M-, (xref-pop-marker-stack)
;; M-? (xref-find-references)
;; C-M-. (xref-find-apropos)
;; C-c C-z (elpy-shell-switch-to-shell)
;; M-x elpy-shell-toggle-dedicated-shell
;; C-c C-y e (elpy-shell-send-statement)
;; C-c C-y C-e (elpy-shell-send-statement-and-step)
;; C-c C-y E (elpy-shell-send-statement-and-go)
;; C-c C-y r (elpy-shell-send-region-or-buffer)
;; C-c C-d (elpy-doc)
;; C-c C-t (elpy-test)
;; M-x elpy-set-test-runner
;; M-x elpy-profile-buffer-or-region


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                PROJECTILE                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/bbatsov/projectile/blob/master/doc/usage.md
;; C-c p C-h    Help
;; C-c p o       Multi Occur
;; C-c p f          Display a list of all files in the project. With a prefix argument it will clear the cache first.
;; C-c p F          Display a list of all files in all known projects.
;; C-c p 4 f    Jump to a project's file using completion and show it in another window.
;; C-c p d          Display a list of all directories in the project. With a prefix argument it will clear the cache first.
;; C-c p 4 d    Switch to a project directory and show it in another window.
;; C-c p 5 d    Switch to a project directory and show it in another frame.
;; C-c p T          Display a list of all test files(specs, features, etc) in the project.
;; C-c p t      Toggle between implementation and test file
;; C-c p 4 t    Jump to implementation/testfile in other window
;; C-c p 5 t    -''- in other frame
;; C-c p s g    Run grep on the files in the project.
;; C-c p 4 b    Switch to a project buffer and show it in another window.
;; C-c p 5 b    Switch to a project buffer and show it in another frame.
;; C-c x s      Run term
;;
;; Ignore dirs
;; M-x customize-variable [RET]
;; projectile-globally-ignored-files
;; [INS]
;; [ Apply and Save ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    EIN                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/millejoh/emacs-ipython-notebook#keybindings---notebook
;; Usage:
;;   start jupyter notebook (jupyter-notebook)
;;   M-x ein:notebooklist-login (use the token as password from above command)
;;   M-x ein:notebooklist-open


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  NEOTREE                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 ACE-WINDOW                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;M-i activate ace-window
;;<prefix> x Delete Window
;;<prefix> m Swap Windows
;;<prefix> M Move Window
;;<prefix> j Select Buffer
;;<prefix> n aw-flip-window
;;<prefix> u Switch Buffer Other Window
;;<prefix> c Split Fair Window
;;<prefix> v Split Vert Window
;;<prefix> b Split Horz Window
;;<prefix> o Delete Other Windows
;;<prefix> ? aw-show-dispatch-help
