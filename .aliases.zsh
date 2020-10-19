alias d="kitty +kitten diff"
alias vim=nvim

alias es='exercism submit src/lib.rs'

# connect to headlessTux
alias htux_vnc="ssh -fL 9901:localhost:5900 192.168.31.69 sleep 10; vncviewer localhost:9901"
alias htux="ssh 192.168.31.69"

# direnv
alias di="echo dotenv > .envrc && touch .env && direnv allow"

alias লস=ls
alias চড=cd
alias সুডও=sudo

# kitty
function cast() { kitty @ send-text --match title:".*" $1\\x0d }

