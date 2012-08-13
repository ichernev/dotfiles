if [ -d "$HOME/.rbenv" ]; then
  export PATH="$HOME/.rbenv/shims:$HOME/.rbenv/bin:$PATH"
  source "$HOME/.rbenv/completions/rbenv.zsh"
fi

if [ -d "$HOME/.nvm" ]; then
  source "$HOME/.nvm/nvm.sh"
fi
