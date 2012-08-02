#!/bin/bash

##########################################
# OS X + emacs oriented bash configuration

: ${HOME=~}
: ${LOGNAME=$(/usr/bin/id -un)}
: ${UNAME=$(/usr/bin/uname)}

########################################## SHELL OPTIONS

# system bashrc
test -r /etc/bashrc &&
      . /etc/bashrc

# notify of bg job completion
set -o notify
set -o emacs

# shell opts. see bash(1) for details
shopt -s cdspell >/dev/null 2>&1
shopt -s extglob >/dev/null 2>&1
shopt -s histappend >/dev/null 2>&1
shopt -s hostcomplete >/dev/null 2>&1
shopt -s interactive_comments >/dev/null 2>&1
shopt -u mailwarn >/dev/null 2>&1
shopt -s no_empty_cmd_completion >/dev/null 2>&1

unset MAILCHECK

# default umask
umask 0022

########################################## PATH

export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/X11R6/bin
export MANPATH=/usr/local/share/man:/usr/local/man:/usr/share/man:/usr/X11R6/man

# classpath for clojure's contrib
export CLASSPATH=$CLASSPATH:/usr/local/Cellar/clojure-contrib/1.2.0/clojure-contrib.jar

########################################## ENVIRONMENT

# detect interactive shell
case "$-" in
    *i*) INTERACTIVE=yes ;;
    *)   unset INTERACTIVE ;;
esac

# detect login shell
case "$0" in
    -*) LOGIN=yes ;;
    *)  unset LOGIN ;;
esac

# enable en_US locale w/ utf-8 encodings if not already configured
: ${LANG:="en_US.UTF-8"}
: ${LANGUAGE:="en"}
: ${LC_CTYPE:="en_US.UTF-8"}
: ${LC_ALL:="en_US.UTF-8"}
export LANG LANGUAGE LC_CTYPE LC_ALL

# always use PASSIVE mode ftp
: ${FTP_PASSIVE:=1}
export FTP_PASSIVE

# ignore backups and python bytecode
FIGNORE="~:.pyc"

# history stuff
HISTCONTROL=ignoreboth
HISTFILESIZE=10000
HISTSIZE=10000

########################################## PAGER / EDITOR

export EDITOR=emacs
export PAGER=less

########################################## PROMPT

# set prompt to host:cd $ in off-white
PS1="\[\033[0;37m\]\h:\W\\$ \[\033[0;29m\]"

########################################## BASH COMPLETION

# ssh hostname completion from known_hosts
_complete_ssh_hosts ()
{
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    comp_ssh_hosts=`sed -e 's/^  *//' -e '/^#/d' -e 's/[, ].*//' -e '/\[/d' ~/.ssh/known_hosts | sort -u`
    COMPREPLY=( $(compgen -W "${comp_ssh_hosts}" -- $cur))
    return 0
}
complete -F _complete_ssh_hosts ssh
complete -c -f command sudo

########################################## LS AND DIRCOLORS

export CLICOLOR=1
alias ls="ls -hBG"
alias ll="ls -lAhBG"

########################################## GO

export GOROOT=~/go
export GOPATH=~/mygo
export PATH=$GOROOT/bin:$PATH

########################################## RUBY

export PATH=$HOME/.rbenv/bin:$PATH
eval "$(rbenv init -)"

########################################## MACOS X / DARWIN

if [ "$UNAME" = Darwin ]; then
    # emacs should open in Cocoa Emacs
    alias emacs=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
    export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient

    # syncs the path with MacOSX programs not run from the shell, for
    # example those launched by the Finder, Spotlight, &c
    defaults write $HOME/.MacOSX/environment PATH "$PATH"
fi

########################################## MOTD / FORTUNE

test -n "$INTERACTIVE" -a -n "$LOGIN" && {
    echo
    fortune -a
    echo
}

