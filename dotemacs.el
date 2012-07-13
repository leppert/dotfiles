;; -*- emacs-lisp -*-
;;
;; I like my emacs to share as many behaviors as possible with OS
;; X and bash, to which end I've customized all three.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PATHS

(add-to-list 'load-path "~/.emacs.d/")
(let ((default-directory "~/.emacs.d/vendor/"))
      (normal-top-level-add-to-load-path '("expand-region" "geiser-0.1.4")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS INTEGRATION

;; Make sure the path is set up for programs launched
;; via Spotlight, the Dock, Finder, &c, by running:
;; $ defaults write $HOME/.MacOSX/environment PATH "$PATH"

;; use OS X's Spotlight for M-x locate
(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

;; I'll be sending files from the command line
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PACKAGE MANAGER

;; more (and more up-to-date) packages than plain ELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENCODING

;; always utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; APPEARANCE

;; typeface
(add-to-list 'default-frame-alist '(font . "Menlo-12"))

;; turn off splash screen messages
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)

;; drop useless chrome
(menu-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode -1)
(scroll-bar-mode -1)

;; global line numbering
(setq linum-format "%4d ")
(global-linum-mode 1)

;; but no highlight on the current line, nor word highlight on page
;(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'idle-highlight-mode)
(global-hl-line-mode -1)

;; slightly more generous line-spacing
(setq-default line-spacing 2)

;; speed up screen re-paint
(setq redisplay-dont-pause t)

;; bring on the color theme
(load-theme 'twilight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INPUT MAPPING

;; make M-up and M-down the same as C-up and C-down, the
;; former matching my bashrc's input settings
(global-set-key (kbd "<M-up>") 'backward-paragraph)
(global-set-key (kbd "<M-down>") 'forward-paragraph)

;; command + up/down/left/right = file start/end, line start/end
;; in other OS X input boxes
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)
(global-set-key (kbd "<s-left>") 'move-beginning-of-line)
(global-set-key (kbd "<s-right>") 'move-end-of-line)

;; normalize with bash's C-w, use Mac-style command-X for cut region
(global-set-key (kbd "C-w") 'backward-kill-word)

;; add readline's backward-kill-line
(defun backward-kill-line ()
  "kill from point to the start of line"
  (interactive)
  (kill-line 0))
(global-set-key (kbd "C-x <C-backspace>") 'backward-kill-line)

;; prefer regexp in my backward search, bash-compatible binding
(global-set-key (kbd "^R") 'isearch-backward-regexp)

;; command-f, the default OSX search keybinding => regexp forward search
(global-set-key (kbd "s-f") 'isearch-forward-regexp)
;; command-r, forward-replace
(global-set-key (kbd "s-r") 'query-replace-regexp)

;; moving between windows, normalize with iTerm2 and (mod'd) tmux
(global-set-key [M-s-left] 'windmove-left)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-up] 'windmove-up)
(global-set-key [M-s-down] 'windmove-down)

;; enhanced completion library
(global-set-key (kbd "M-/") 'hippie-expand)

;; shift-select mode, normalized with Mac OS X
(setq shift-select-mode t)

;; Mac OS X-style font-size control
(define-key global-map (kbd "s-+") 'text-scale-increase)
(define-key global-map (kbd "s--") 'text-scale-decrease)

;; typing after selection kills the region
(delete-selection-mode 1)

;; undo-tree-mode with aliases that match OS X undo/redo
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "s-z") 'undo) ; command+z
(global-set-key (kbd "s-Z") 'redo) ; command+shift+z

;; expand-region is super handy
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; OS X Lion fullscreen mode, not yet in cocoa emacs HEAD
;;(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROGRAMMING/LANGUAGES

;; tell me about my whitespace, clean it up on save
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook
          'whitespace-cleanup
          nil t)
(whitespace-mode)

;; four space tabs in general
(setq-default tab-width 4)
(setq c-basic-offset 4)

;; auto-complete-mode - popup help
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-use-quick-help t)
(setq ac-auto-show-menu 0.)
(setq ac-quick-help-delay 0.3)
(ac-config-default)
(ac-flyspell-workaround)
(define-key ac-complete-mode-map [tab] 'ac-expand)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; TODO customize ac-complete for color theme
;(set-face-background 'ac-candidate-face "#366060")
;(set-face-foreground 'ac-selection-face "#1f1f1f")
;(set-face-background 'ac-selection-face "#8cd0d3")
;(set-face-foreground 'ac-selection-face "#1f1f1f")

;; compile shortcut
(define-key global-map (kbd "s-k") 'compile)

;; two space tabs in coffee
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; adjust paredit's key bindings so they don't override my
;; bash compatible preferences from above
(eval-after-load 'paredit
  '(progn
     ;; not just in lisp mode(s)
     (global-set-key (kbd "M-(") 'paredit-wrap-round)
     (global-set-key (kbd "M-[") 'paredit-wrap-square)
     (global-set-key (kbd "M-{") 'paredit-wrap-curly)

     (global-set-key (kbd "M-)") 'paredit-close-round-and-newline)
     (global-set-key (kbd "M-]") 'paredit-close-square-and-newline)
     (global-set-key (kbd "M-}") 'paredit-close-curly-and-newline)

     ;; fights with my preferred navigation keys
     (dolist (binding (list (kbd "M-<up>") (kbd "M-<down>")))
       (define-key paredit-mode-map binding nil))))

;;;; geiser for racket
;(require 'geiser)
;(setq geiser-active-implementations '(racket))

;;;; clojure-mode uses lein repl
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")))

;;;; sbcl with quicklisp under slime
;; XXX temporarily commented out because it fights with clojure
;;(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq slime-net-coding-system 'utf-8-unix) ; prefer utf-8

;; add auto-completion for slime
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'slime-repl-mode))

;; no need to show trailing whitepsace in the repl
(add-hook 'slime-repl-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; local copy of the HyperSpec for CL
(setq common-lisp-hyperspec-root
      "file:/Users/jack/lisp/HyperSpec/")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROSE AND NOTES

(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdown documents" t)
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)$" . markdown-mode) auto-mode-alist))

;; use MacSpell until ns-spell-checker support is ported to cocoa emacs
;; https://github.com/ruda/macspell
(setq ispell-program-name "~/bin/macspell.py")
(setq ispell-extra-args '("--encoding=utf8" "--auto-lang=yes"))

;; TODO bring in latex customizations from old .emacs

