#!/bin/sh

set -e

my_github_url() {
  if [ $GIT_SSH -eq 1 ]; then
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

setup_commands() {
  if ! which git &> /dev/null; then
    echo "You need to install git"
    exit 1
  fi
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

  if [ ! -d .vim ]; then
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

if [ $# -eq 0 ]; then
  set commands sshkeys vim git zsh xmonad scripts
fi

for thing; do
  setup_$thing
done
