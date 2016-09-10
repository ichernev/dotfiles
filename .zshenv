if [ -d "$HOME/.rbenv" ]; then
  export PATH="$HOME/.rbenv/shims:$HOME/.rbenv/bin:$PATH"
  source "$HOME/.rbenv/completions/rbenv.zsh"
fi

if [ -d "$HOME/bin" ]; then
  if ! echo ":$PATH:" | grep -q ":$HOME/bin:"; then
    export PATH="$HOME/bin:$PATH"
  fi
fi

if [ -d "/usr/local/go/bin" ]; then
  export PATH="$PATH:/usr/local/go/bin"
fi

export GOPATH=$HOME/go
if [ -d "$GOPATH/bin" ]; then
  export PATH="$PATH:$GOPATH/bin"
fi

# nenv initialization goes in /etc/imo.bashrc
