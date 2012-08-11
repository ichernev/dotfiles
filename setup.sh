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
    echo -n "generate keys (y/n)? "
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
  git stash save | grep -q 'Saved working directory'
  stashed=$?
  git pull -r
  [ $stashed -eq 0 ] && git stash pop
  git submodule init
  git submodule update
  popd

  if [ ! -L .vimrc ]; then
    if [ -e .vimrc ]; then
      echo "moved old .vimrc to .vimrc.bac"
      mv .vimrc .vimrc.bac
    fi
    echo "linking .vimrc"
    ln -s .vim/.vimrc .vimrc
  fi
}

for thing in commands sshkeys vim; do
  setup_$thing
done
