## NAVIGATION

One of the goals of this configuration is to avoid the cognitive load
of switching keyboard navigation habits between applications. Luckily,
most of the default emacs control codes are also supported by OSX text
editing panels and bash/zsh (via inputrc).

This table contains the most commonly used bindings between the
different modes, most of which are normalized. A pattern to keep in
mind with regard to "arrow navigation" is that the option key acts as
a magnifier (move by words and paragraphs instead of characters and
lines), and control+option does something similar (move by "units of
code", a sexp in Lisp, other syntactic constructs in other languages).

Note: one generally should not move very far using any form of arrow
navigation. The preferred method of jumping around inside a file in
emacs is to use ctrl-s to search forward or ctrl-r to search backward.

| Key           | emacs                     | shell/inputrc      | OSX
|:--------------|:-------------------------:|:------------------:|------:|
| Left/Right             | Left/right character    | *         | *
| Up/Down                | Up/Down character       | *         | *
| Option-Left/Right      | Left/right word         | *         | *
| Option-Up/Down         | Up/Down paragraph       | *         | *
| Ctrl-Option-Left/Right | Left/right unit of code | *         | X
| Command-Left/Right     | Begin/End line          | X         | *
| Command-Up/Down        | Begin/End document      | X         | *
| fn-Left/Right          | Begin/End document      | X         | *
| fn-Up/Down             | Up/Down Page            | X         | *
| command-option-l/r/u/d | Switch visible buffer   | *         | X
| Delete            | Delete char left          | *              | *
| fn+Delete         | Delete word left          | *              | *
| fn+option+Delete  | Delete word right         | *              | *
| Ctrl-a        | Beginning of line         | *                  | *
| Ctrl-b        | Back one character        | *                  | *
| Ctrl-c        | Command (start sequence)  | Cancel current job | X
| Ctrl-d        | Kill character forward    | *                  | *
| Ctrl-e        | End of line               | *                  | *
| Ctrl-f        | Forward one character     | *                  | *
| Ctrl-g        | Abort current action      | *                  | X
| Ctrl-h        | Help (start sequence)     | Delete             | Delete
| Ctrl-i (TAB)  | Indent line               | Complete           | X
| Ctrl-j        | Newline + indent          | Enter              | X
| Ctrl-k        | Kill line forward         | *                  | *
| Ctrl-l        | Center on current line    | Clear, show line   | *
| Ctrl-m        | Enter                     | Enter              | X
| Ctrl-n        | Next line                 | *                  | *
| Ctrl-o        | Insert line after cursor  | X                  | *
| Ctrl-p        | Previous line             | *                  | *
| Ctrl-q        | Literal insert next char  | Continue output    | *
| Ctrl-r        | Reverse search            | *                  | X
| Ctrl-s        | Search forward            | Stop output        | X
| Ctrl-t        | Transpose characters      | *                  | *
| Ctrl-u        | Universal argument        | *                  | X
| Ctrl-v        | Page down                 | X                  | *
| Ctrl-w        | Kill word backward        | *                  | X
| Ctrl-x        | Execute (start seq)       | *                  | X
| Ctrl-y        | Yank previously killed    | *                  | *
| Ctrl-z        | Suspend process           | *                  | X

The usual OSX command key bindings are mostly supported. Command-S
saves, Command-F "finds" (searches forward), cut and paste, selection,
and so on all operate normally.

Command-Z is undo, Shift-Command-Z is redo. This functionality is
provided by a package called
[undo-tree](http://www.emacswiki.org/emacs/UndoTree), which is similar
to the vi package of the same name. It allows one to see recent
changes as a decision tree and partially back out changes by choosing
branches (like a mini RCS in the editor).

## BUFFERS, FRAMES AND FILES

Ctrl-x Ctrl-f is "find file," which allows one to find files quickly
using command completion in the mini-buffer.

These commands both work with
[tramp](http://www.gnu.org/software/tramp/), which is a great, great
feature. One can open a file on a remote host via sftp by specifying
its name like this:

hostname:/path/to/file

Ctrl-x Ctrl-b is the command to switch the current frame (like a pane
in tmux, basically a subwindow) to a buffer by name with command
completion. It remembers recently open files, so it makes an any way
to open anything one has been working on without hunting around in the
filesystem.

Split the current frame in two vertically by hitting Ctrl-x 2,
horizontally by Ctrl-x 3. Close the current frame (but not the
underling buffer/file) with Ctrl-x 0. Close all *but* the current
frame with Ctrl-x 1.

I also add a keybinding to Safari that normalize its tab-switching
command to Chrome's default of command+option left/right, which is
also supported by iTerm2. This emacs configuration borrows that
binding with a twist: one can switch to any visible buffer by using
command+option left/right/up/down.

## COMPILATION

The key command-k is bound to compile the current buffer (same as in
Xcode). This allows one to type a command that will be re-used for
subsequent invocations. Useful for running test programs as well
compiling source.

## SOURCE CONTROL

[Magit](http://philjackson.github.com/magit/) is a nice integrated git
for emacs. There are others, but this is my favorite.

## DYNAMIC LANGUAGES

SLIME, run-ruby, run-python, and so on. This is where the real power
of emacs shines, but it's too much to explain right now. TK.
