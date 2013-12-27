if [ -d "$HOME/.rbenv" ]; then
  export PATH="$HOME/.rbenv/shims:$HOME/.rbenv/bin:$PATH"
  source "$HOME/.rbenv/completions/rbenv.zsh"
fi

if [ -d "$HOME/.nenv" ]; then
  export PATH="$HOME/.nenv/shims:$HOME/.nenv/bin:$PATH"
fi

ANDROID_BUNDLE="$HOME/opt/adt-bundle-linux-x86_64-20130917"
if [ -d "$ANDROID_BUNDLE" ]; then
  # sdk/tools has a folder ant ... go figure
  if [ -d "$ANDROID_BUNDLE/eclipse/plugins/org.apache.ant_1.8.3.v201301120609/bin" ]; then
    export PATH="$PATH:$ANDROID_BUNDLE/eclipse/plugins/org.apache.ant_1.8.3.v201301120609/bin"
  fi
  export PATH="$PATH:$ANDROID_BUNDLE/sdk/tools:$ANDROID_BUNDLE/sdk/platform-tools"
fi
unset ANDROID_BUNDLE
