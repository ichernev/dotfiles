#!/bin/sh

set -e

setup_commands() {
  if ! which git &> /dev/null; then
    echo "You need to install git"
    exit 1
  fi
}

setup_vim() {
  REPO="git@github.com:ichernev/vimfiles.git"
  pushd "$HOME"

  if [ ! -d .vim ]; then
    echo "cloning vim repo"
    git clone "$REPO" .vim
  fi

  pushd .vim
  git stash save
  git pull -r
  git stash pop
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

for thing in commands vim; do
  setup_$thing
done
