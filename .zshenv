if [ -d "$HOME/.rbenv" ]; then
  export PATH="$HOME/.rbenv/shims:$HOME/.rbenv/bin:$PATH"
  source "$HOME/.rbenv/completions/rbenv.zsh"
fi

if [ -d "$HOME/.nenv" ]; then
  export PATH="$HOME/.nenv/shims:$HOME/.nenv/bin:$PATH"
fi
