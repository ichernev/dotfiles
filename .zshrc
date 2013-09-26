# completion
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _complete _ignored
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'r:|[._-]=** r:|=**' '+m:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' original false
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' verbose true
zstyle ":completion:*:commands" rehash 1
zstyle :compinstall filename '/home/iskren/.zshrc'
zstyle ":completion:*:commands" rehash 1 # auto rehash

fpath=(~/.zdir/completion $fpath)    # add local complation function path
autoload -U ~/.zdir/completion/*(:t) # autoload all completion functions

autoload -U compinit; compinit

# history
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt APPEND_HISTORY           # append rather than overwrite history file.
# setopt EXTENDED_HISTORY         # save timestamp and runtime information
# setopt HIST_EXPIRE_DUPS_FIRST   # allow dups, but expire old ones when I hit HISTSIZE
# setopt HIST_FIND_NO_DUPS        # don't find duplicates in history
# setopt HIST_IGNORE_ALL_DUPS     # ignore duplicate commands regardless of commands in between
setopt HIST_IGNORE_DUPS         # ignore duplicate commands
# setopt HIST_REDUCE_BLANKS       # leave blanks out
# setopt HIST_SAVE_NO_DUPS        # don't save duplicates
# setopt INC_APPEND_HISTORY       # write after each command
# setopt SHARE_HISTORY            # share history between sessions

# misc
setopt AUTO_CD                  # commands that are directories are entered into with cd
setopt NOMATCH                  # file patterns that do not match fail loudly
unsetopt BEEP                   # do not beep on error
unsetopt EXTENDED_GLOB          # do not treat # ~ and ^ as part of patterns
unsetopt NOTIFY                 # do not report status of background jobs immediately
setopt INTERACTIVECOMMENTS      # enable the use of '#' char for comments on the cmd line

# bindings
bindkey -e
bindkey "\e[3~" delete-char       # del
bindkey "\e[7~" beginning-of-line # home
bindkey "\e[8~" end-of-line       # end
bindkey "\e[2~" overwrite-mode    # insert
# eval "$(sed -n 's/^/bindkey /; s/: / /p' /etc/inputrc)"

# color names
autoload -U colors; colors

# prompt
autoload -Uz promptinit; promptinit
fpath=(~/.zdir/functions $fpath)
autoload -U ~/.zdir/functions/*(:t)
setopt PROMPT_SUBST                           # enable variable/function substitution in prompt
add-zsh-hook precmd update_current_git_vars   # update variables needed for prompt before it is drawn

# PS1="[%{$fg[green]%}%n%{$reset_color%} %{$fg[yellow]%}%c%{$reset_color%}$(prompt_git_info)]%% "
export PROMPT='[%F{magenta}ugly%f %F{yellow}%1~%f$(prompt_git_info)$(prompt_pkg_update)%F{white}$(screen_title)%f]%% '

last_pkg_update() {
  if [ -f "/var/log/pacman.log" ]; then
    grep -- '-Syu\|-Suy' /var/log/pacman.log \
      | tail -n1 \
      | gawk ' match($0, /\[([^]]*)\]/, ary) { print ary[1] } '
  elif [ -f "/var/cache/apt/pkgcache.bin" ]; then
    ls -l "/var/cache/apt/pkgcache.bin" | gawk ' { print $6, $7 } '
  else
    date # output now, so it won't prompt for updates
  fi
}

prompt_pkg_update() {
  secs_since_upd=$(($(date '+%s') - $(date "-d$(last_pkg_update)" '+%s')))
  if [ $secs_since_upd -gt $((2 * 7 * 24 * 60 * 60)) ]; then
    echo " %F{red}update%f"
  else
    echo ""
  fi
}

screen_title() {
  [ -z "$STY" ] && return
  echo "$STY" | gawk ' BEGIN { FS = "." } { printf(" %s", $2) } '
}

# aliases
alias ls='ls --color=auto'
alias ll='ls -l'
alias cdf='colordiff | less -R'

for cmd in poweroff pm-suspend reboot energy_state; do
  alias $cmd="sudo $cmd"
done
alias cal='cal -m'
alias feh='feh -ZFd'
alias sc='sudo systemctl'
alias poweroff='sc poweroff'
alias reboot='sc reboot'

# local config
[ -x ~/.localrc ] && . ~/.localrc
