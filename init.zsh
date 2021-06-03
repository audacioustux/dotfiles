cp .profile.template ~/.profile

ln .aliases.zsh ~/
ln .zshrc ~/
ln .tmux.conf ~/
ln .alacritty.yml ~/
ln .tool-versions ~/

mkdir -p .config/nvim
ln .config/nvim/init.vim ~/.config/nvim/
ln .config/nvim/coc-settings.json ~/.config/nvim/
