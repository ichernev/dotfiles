if [ -d "$HOME/.rbenv" ]; then
  export PATH="$HOME/.rbenv/shims:$HOME/.rbenv/bin:$PATH"
  source "$HOME/.rbenv/completions/rbenv.zsh"
fi

if [ -d "$HOME/.nenv" ]; then
  export PATH="$HOME/.nenv/shims:$HOME/.nenv/bin:$PATH"
fi

ANDROID_BUNDLE="$HOME/Android/Sdk"
if [ -d "$ANDROID_BUNDLE" ]; then
  # sdk/tools has a folder ant ... go figure
  # if [ -d "$ANDROID_BUNDLE/eclipse/plugins/org.apache.ant_1.8.3.v201301120609/bin" ]; then
  #   export PATH="$PATH:$ANDROID_BUNDLE/eclipse/plugins/org.apache.ant_1.8.3.v201301120609/bin"
  # fi
  export PATH="$PATH:$ANDROID_BUNDLE/tools:$ANDROID_BUNDLE/platform-tools"
fi
unset ANDROID_BUNDLE

if [ -d $HOME/bin ]; then
  export PATH="$HOME/bin:$PATH"
fi
