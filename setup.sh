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

  if [ ! -L .gitconfig ]; then
    if [ -e .gitconfig ]; then
      echo "move old .gitconfig to .gitconfig.bac"
      mv .gitconfig .gitconfig.bac
    fi
    echo "linking .gitconfig"
    ln -s .dotfiles/.gitconfig .gitconfig
  fi

  popd # $HOME
}

for thing in commands sshkeys vim git; do
  setup_$thing
done
