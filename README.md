My configuration of bash+inputrc and emacs under Mac OS X.

After the release of emacs 24.1, I took the time to jettison an
enormous quantity of elisp that had accumulated in my dotfiles since
1985. Most of the custom features implemented by that code now have
analogues in the base emacs or in one of the many excellent packages
available via the ELPA-compatible repositories.

If you would like to test out this configuration, I recommend
installing emacs using this brew recipe:

$ brew install emacs --cocoa --srgb

In addition, one should install:

* The Python system spellcheck to ispell protocol package from
[here](https://github.com/ruda/macspell) so that emacs will have
access to the system-wide spelling infrastructure.

* This collection of packages via ELPA/MELPA/Marmalade (using M-x
  package-list-packages):
    * ac-slime-20120524
    * archive-contents
    * archives
    * auto-complete-20120327
    * builtin-packages
    * clojure-mode-20120531
    * clojurescript-mode-20120521
    * coffee-mode-20120522
    * css-mode-1.0
    * elisp-slime-nav-20111111
    * expand-region
    * find-file-in-project-20110903
    * go-mode-20120613
    * haml-mode-20101019
    * haskell-mode-20120612
    * idle-highlight-mode-20110817
    * ido-ubiquitous-20120412
    * inf-ruby-20120403
    * js2-mode-20120617
    * magit-20120616
    * magithub-20120209
    * markdown-mode-20120225
    * paredit-20110508
    * popup-20120331
    * ruby-block-20111101
    * ruby-electric-1.1
    * ruby-end-20120403
    * ruby-mode-20110630
    * slime-20120612
    * slime-ritz-20120612
    * smex-20120301
    * starter-kit-20120518
    * starter-kit-eshell-20120316
    * starter-kit-js-20110930
    * starter-kit-lisp-20120504
    * starter-kit-ruby-20120128
    * undo-tree-20120511
    * yaml-mode-20120227

* [Powerline](http://emacsfodder.github.com/blog/2012/06/20/powerline-enhanced/),
which pretties up the mode-line quite a bit.

* Also, watch the video on [Expand region](https://github.com/emacsmirror/expand-region).
