if [ -d "$HOME/.rbenv" ]; then
  export PATH="$HOME/.rbenv/shims:$HOME/.rbenv/bin:$PATH"
  source "$HOME/.rbenv/completions/rbenv.zsh"
fi

# nenv initialization goes in /etc/imo.bashrc
