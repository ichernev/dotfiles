#!/bin/bash

sudo add-apt-repository ppa:git-core/ppa

sudo apt-get install zsh git xmonad vim-gtk rxvt-unicode-256color fonts-droid \
  suckless-tools htop xsel feh curl gawk

sudo cp etc/default/locale /etc/default/locale
