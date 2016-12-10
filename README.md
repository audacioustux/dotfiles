***.vimrc***
------

> My VIM dot file

![Terminix - molokai dark](https://raw.githubusercontent.com/audacioustux/.vimrc/master/Screenshot_20160904_152902.png "Terminix - molokai dark")

# Installation
    $ git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
    $ cd ~
    $ wget https://raw.githubusercontent.com/audacioustux/dotfiles/master/.vimrc
    $ vim +PluginInstall +qall
    $ cd /usr/share/fonts
    $ sudo curl -fLo "Droid Sans Mono for Powerline Nerd Font Complete.otf" https://raw.githubusercontent.com/ryanoasis/nerd-fonts/master/patched-fonts/DroidSansMono/complete/Droid%20Sans%20Mono%20for%20Powerline%20Nerd%20Font%20Complete.otf
    $ sudo fc-cache -f -v

Preferred terminal emulators: Terminix, Gnome-Terminal. Install themes from [Here](https://github.com/Mayccoll/Gogh "https://github.com/Mayccoll/Gogh") ( preferred Chalk, molokai dark ). Change terminal emulator font to *droidsansmonoforpowerline Nerd Font Regular*.

# Custom keybindings
 - Paste Toggle = `<f12>`
 - UndoTree = `<f5>`
 - C/C++ Build and Run = `<f9>`
 - NERDTree toggle = `<f2>`

# Plugins
 - Vundle = Plugin manager
 - Nerdtree = file browser
 - vim-airline =  status/tabline
 - vim-airline-themes = airline theme ( default: wombat )
 - vim-devicons = adds filetype glyphs (icons) to other plugins
 - undotree = Undo Tree
 - vim-cpp-enhanced-highlight = Additional syntax highlighting
 - syntastic = Syntex checker

***init.vim/nvimrc (neovim)***
------

> My NEOVIM dot file

![Terminator - molokai dark](https://raw.githubusercontent.com/audacioustux/dotfiles/master/Screenshot_%E0%A7%A8%E0%A7%A6%E0%A7%A7%E0%A7%AC%E0%A7%A7%E0%A7%A8%E0%A7%A6%E0%A7%AD_%E0%A7%A7%E0%A7%AC%E0%A7%A7%E0%A7%AA%E0%A7%A7%E0%A7%AE.png "Terminator - molokai dark")

# Installation
    $ curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    $ cd ~/.config/nvim
    $ wget https://raw.githubusercontent.com/audacioustux/dotfiles/master/init.vim \
    https://raw.githubusercontent.com/audacioustux/dotfiles/master/ycm_extra_conf.py
    $ nvim +PlugInstall +qall
    $ cd /usr/share/fonts
    $ sudo curl -fLo "Droid Sans Mono for Powerline Nerd Font Complete.otf" https://raw.githubusercontent.com/ryanoasis/nerd-fonts/master/patched-fonts/DroidSansMono/complete/Droid%20Sans%20Mono%20for%20Powerline%20Nerd%20Font%20Complete.otf
    $ sudo fc-cache -f -v

Preferred terminal emulators: Terminix, Gnome-Terminal, terminator. Install themes from [Here](https://github.com/Mayccoll/Gogh "https://github.com/Mayccoll/Gogh") ( preferred Chalk, molokai dark ). Change terminal emulator font to *droidsansmonoforpowerline Nerd Font Regular*.

# Custom keybindings
 - Paste Toggle = `<f12>`
 - UndoTree = `<f5>`
 - C/C++ Build and Run = `<f9>`
 - NERDTree toggle = `<f2>`

# Plugins
 - vim-plug = Plugin manager
 - Nerdtree = file browser
 - vim-airline =  status/tabline
 - vim-airline-themes = airline theme ( default: wombat )
 - vim-devicons = adds filetype glyphs (icons) to other plugins
 - undotree = Undo Tree
 - vim-nerdtree-syntax-highlight = filetype highlighter
 - vim-cpp-enhanced-highlight = Additional syntax highlighting
 - syntastic = Syntex checker
 - YouCompleteMe = linter
