My configuration of bash+inputrc and emacs under Mac OS X.

After the release of emacs 24.1, I took the time to jettison an
enormous quantity of elisp that had accumulated in my dotfiles since
1985. Most of the custom features implemented by that code now have
analogues in the base emacs or in one of the many excellent packages
available via the ELPA-compatible repositories.

If you would like to test out this configuration, I recommend
installing emacs using this
[homebrew](http://mxcl.github.com/homebrew/) recipe:

$ brew install emacs --cocoa --srgb

In addition, one should install
[macspell](https://github.com/ruda/macspell), a Python-ObjC bridge
application that provides ispell compatible access to the OS X system
spellchecker.

This configuration will automatically install a somewhat large number
of packages via ELPA/MELPA/Marmalade the first time it's run if
they're not already installed.

Watch this video on
[Expand region](https://github.com/emacsmirror/expand-region).

If one intends to hack clojure, Common Lisp or elisp, it would be wise
to have a look at the
[paredit cheat sheet](http://www.emacswiki.org/pics/static/PareditCheatsheet.png).
Do not fight paredit. Paredit is your friend.
