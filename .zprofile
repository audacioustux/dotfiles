. "$HOME/.cargo/env"

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias devlap="ssh audacioustux@devlap"
alias ubuntu20="lxc exec maximum-snail -- sudo --login --user ubuntu"

# opam configuration
test -r /Users/tanjimhossain/.opam/opam-init/init.zsh && . /Users/tanjimhossain/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# scala coursier
export PATH="$PATH:/Users/tanjimhossain/Library/Application Support/Coursier/bin"

# graal / java
export GRAAL_NAME="graalvm-ee-java11-21.1.0"
export GRAALVM_HOME="/Library/Java/JavaVirtualMachines/$GRAAL_NAME/Contents/Home"
export JAVA_HOME=$GRAALVM_HOME
export PATH="$JAVA_HOME/bin:$PATH"

