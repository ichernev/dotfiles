#!/bin/bash

set -e

my_github_url() {
  if [ "$GIT_SSH" -eq 1 ]; then
    echo "git@github.com:ichernev/$1.git"
  else
    echo "git://github.com/ichernev/$1.git"
  fi
}

github_url() {
  echo "git://github.com/$1/$2.git"
}

safe_link() {
  src=$1
  dst=$([ $# -eq 2 ] && echo $2 || echo $1)
  if [ ! -L $dst ]; then
    if [ -e $dst ]; then
      echo "move old $dst to $dst.bac"
      mv $dst $dst.bac
    fi
    echo "linking $dst"
    ln -s "$HOME/.dotfiles/$src" "$dst"
  fi
}

xdiff() {
  df=$(diff $1 $2)
  if [ -n "$df" ]; then
    echo "$1:"
    echo "$df"
  fi
}

setup_commands() {
  needed=(git vim sudo ssh xrandr)
  for cmd in $needed; do
    if ! which $cmd &> /dev/null; then
      echo "You need to install $cmd"
      exit 1
    fi
  done
}

setup_sshkeys() {
  local ans
  if [ ! -f "$HOME/.ssh/id_rsa.pub" ]; then
    echo -n "generate ssh keys (y/n)? "
    read ans
    if [ $ans == 'y' -o $ans == 'Y' ]; then
      ssh-keygen
    fi
  fi

  if ssh git@github.com &> /dev/null; then
    :
  else
    local retval=$?
    if [ $retval -eq 1 ]; then
      GIT_SSH=1
    else
      GIT_SSH=0
      echo -n "No git ssh access. Clone read-only (y/n)? "
      read ans
      if [ $ans == 'n' -o $ans == 'N' ]; then
        echo "Add to github keys:"
        echo "---------------------------"
        cat "$HOME/.ssh/id_*.pub"
        exit 0
      fi
    fi
  fi
}

setup_vim() {
  pushd "$HOME"

  if [ ! -d .vim/.git ]; then
    echo "cloning vim repo"
    git clone "$(my_github_url vimfiles)" .vim
  fi

  pushd .vim
  set +e
  git stash save | grep -q 'Saved working directory'
  local stashed=$?
  set -e
  echo '# Updating vim'
  git pull -r
  [ $stashed -eq 0 ] && git stash pop
  echo '# Updating submodules'
  git submodule init
  git submodule update
  popd # .vim

  if [ ! -L .vimrc ]; then
    if [ -e .vimrc ]; then
      echo "moved old .vimrc to .vimrc.bac"
      mv .vimrc .vimrc.bac
    fi
    echo "linking .vimrc"
    ln -s .vim/.vimrc .vimrc
  fi

  popd # $HOME
}

setup_git() {
  pushd "$HOME"

  safe_link .gitconfig

  popd # $HOME
}

setup_zsh() {
  pushd "$HOME"

  safe_link .zshrc
  safe_link .zshenv
  safe_link .zdir

  popd # $HOME
}

setup_xmonad() {
  pushd "$HOME"

  if ! which xmonad &> /dev/null; then
    if ! which cabal &> /dev/null; then
      echo "install cabal"
      return
    fi
    echo "installing xmonad with cabal"
    cabal install xmonad xmonad-contrib
  fi

  safe_link .Xdefaults
  safe_link .xinitrc
  safe_link .xmonad
  safe_link .xmobarrc

  pushd .xmonad
  make
  popd # .xmonad

  popd # $HOME
}

setup_scripts() {
  [[ ! -d "$HOME/bin" ]] && mkdir $HOME/bin

  pushd "$HOME/bin"

  for script in $(ls "$HOME/.dotfiles/bin/"); do
    script_name="$(basename $script)"
    safe_link "bin/$script_name" "$script_name"
  done

  popd # $HOME/bin
}

setup_rc() {
  # rc.conf
  if [ -e /etc/rc.conf ]; then
    xdiff /etc/rc.conf etc/rc.conf
  else
    echo "setting up /etc/rc.conf"
    sudo cp etc/rc.conf /etc/rc.conf
  fi

  # time
  if [ -L /etc/localtime ]; then
    tzl=$(readlink -f /etc/localtime)
    echo "timezone set to $(date +"%Z") (${tzl##/usr/share/zoneinfo/})"
  else
    pushd /etc
    echo "setting timezone to UTC"
    sudo unlink localtime
    sudo ln -s /usr/share/zoneinfo/UTC localtime
    popd
  fi

  # locale
  if [ -e /etc/locale.conf ]; then
    xdiff /etc/locale.conf etc/locale.conf
  else
    echo "setting up /etc/locale.conf"
    sudo cp etc/locale.conf /etc/locale.conf
  fi

  if [ -e /etc/locale.gen ]; then
    xdiff /etc/locale.conf etc/locale.conf
  else
    echo "setting up /etc/locale.gen"
    sudo cp etc/locale.gen /etc/locale.gen
    sudo locale-gen
  fi

  # vconsole
  if [ -e /etc/vconsole.conf ]; then
    xdiff /etc/vconsole.conf etc/vconsole.conf
  else
    echo "setting up /etc/vconsole.conf"
    sudo cp etc/vconsole.conf /etc/vconsole.conf
  fi

  # hostname
  if [ -e /etc/hostname ]; then
    echo "hostname set to $(cat /etc/hostname)"
  else
    sudo vim /etc/hostname
  fi
}

setup_X() {
  l="etc/X11/xorg.conf.d"
  r="/etc/X11/xorg.conf.d"

  for file in $l/*.conf; do
    file=${file##$l/}
    if [ -e "$r/$file" ]; then
      xdiff "$r/$file" "$l/$file"
    else
      echo "installing $r/$file"
      sudo mkdir -p $r
      sudo cp "$l/$file" "$r/$file"
    fi
  done
}

setup_nenv() {
  if [ -d "$HOME/.nenv" ]; then
    pushd $HOME/.nenv
    # if there is a conflict non zero exit will abort scrip
    git pull -r
    popd # $HOME/.nenv
  else
    git clone "git://github.com/ryuone/nenv.git" "$HOME/.nenv"
    echo "install nodes with 'nenv install v0.8.9'"
  fi
}

setup_rbenv() {
  if [ -d "$HOME/.rbenv" ]; then
    pushd $HOME/.rbenv
    # if there is a conflict non zero exit will abort scrip
    git pull -r
    popd # $HOME/.rbenv
  else
    git clone "git://github.com/sstephenson/rbenv.git" "$HOME/.rbenv"
    mkdir -p $HOME/.rbenv/plugins
    pushd $HOME/.rbenv/plugins
    git clone "git://github.com/sstephenson/ruby-build.git"
    popd
    echo "install rubies with 'rbenv install irb'"
  fi
}

if [ $# -eq 0 ]; then
  set commands rc X sshkeys vim git zsh xmonad scripts nenv rbenv
fi

for thing; do
  setup_$thing
done
