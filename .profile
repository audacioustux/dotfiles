export EDITOR=/usr/bin/nvim

# add go path
export GOPATH=$(go env GOPATH)
export PATH="$PATH:${GOPATH//://bin:}/bin"

# add cargo path
export PATH="$HOME/.cargo/bin:$PATH"

# add npm path
export PATH=$PATH:$HOME/.npm-global/bin
# add yarn path
export PATH="$PATH:`yarn global bin`"

# add nvm path
export NVM_DIR="$HOME/.nvm"

# escripts
export PATH=$PATH:$HOME/.mix/escripts

# emacs/bin
export PATH=$PATH:$HOME/.emacs.d/bin

# Enable history in IEX through Erlang(OTP)
export ERL_AFLAGS="-kernel shell_history enabled"

