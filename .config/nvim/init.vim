" install vim-plug if needed.
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source ~/.config/nvim/init.vim
endif

" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin(stdpath('config').'/plugged')

" Make sure you use single quotes
" elixir
Plug 'elixir-editors/vim-elixir'
Plug 'elixir-lsp/elixir-ls', { 'do': { -> g:ElixirLS.compile() } }
" language server protocol (lsp)
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" statusline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" theme
Plug 'morhetz/gruvbox'
" vertical lines at each indentation level
Plug 'Yggdroot/indentLine'
" comment manipulation
Plug 'preservim/nerdcommenter'
" fzf - fuzzy search
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
" icons
Plug 'ryanoasis/vim-devicons'
" git
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'airblade/vim-gitgutter'
" tags
Plug 'majutsushi/tagbar'
" Surround.vim is all about 'surroundings': parentheses, brackets, quotes, XML tags, and more
Plug 'tpope/vim-surround'
" repeat command with . (dot)
Plug 'tpope/vim-repeat'
" multiple cursor
Plug 'terryma/vim-multiple-cursors'
" rust
Plug 'rust-lang/rust.vim'
" emmet
Plug 'mattn/emmet-vim'
" undo tree
Plug 'simnalamburt/vim-mundo'
" move line/selection left/right up/down
Plug 'matze/vim-move'
" just to solve disappearing quotes in json because of indentLines plug [patch-1]
Plug 'elzr/vim-json'
" fancy start screen
Plug 'mhinz/vim-startify'
" carbon - take/make screenshot
Plug 'kristijanhusak/vim-carbon-now-sh'
" snippets
Plug 'honza/vim-snippets'
" better c++11/14/17 highlight
Plug 'octol/vim-cpp-enhanced-highlight'
" autosave
" Plug '907th/vim-auto-save'
" generate tags automatically
" Plug 'craigemery/vim-autotag'
" Plug 'ludovicchabant/vim-gutentags'
" Plug 'skywind3000/gutentags_plus'
" for clojure (lisp)
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-fireplace'
" rainbow parentheses
Plug 'luochen1990/rainbow'
" vim motion on speed
Plug 'easymotion/vim-easymotion'
" color highlighter
Plug 'ap/vim-css-color'
" dart & flutter
Plug 'dart-lang/dart-vim-plugin'

" Initialize plugin system
call plug#end()

" the prefix to use for leader commands
let mapleader=" "

" switches to absolute line numbers automatically when relative numbers don't make sense.
set number relativenumber
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu | set rnu   | endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu | set nornu | endif
augroup END
" toggle line number mode
nnoremap <silent> <F7> :set relativenumber!<cr>

" enable highlighting of the current cursor position (slows down rendering!)
" set cursorline
" set cursorcolumn

" set syntax theme
syntax on
set termguicolors " try `:set notermguicolors` for comparison
colorscheme gruvbox
set background=dark

" set column guide
set textwidth=80
set colorcolumn=+1

" reduce travel time to reach <Esc> key
map! kj <Esc>
map! jk <Esc>

" set mouse mode
set mouse=a

" bind to system clipboard (install xclip, see :h clipboard)
set clipboard+=unnamedplus

" don't highlight all matched
set nohlsearch

" Make searching case insensitive
set ignorecase
" ... unless the query has capital letters.
set smartcase

" If opening buffer, search first in opened windows.
set switchbuf=usetab

" keep hlsearch while searching
set incsearch
augroup incsearchHighlight
  autocmd!
  autocmd CmdlineEnter /,\? :set hlsearch
  autocmd CmdlineLeave /,\? :set nohlsearch
augroup END

" keep the cursor a bit far than the edges (bottom & top) always
set scrolloff=10
" for Horizontal scrolling
set sidescroll=1
set sidescrolloff=15

" prevent common mistake of pressing q: instead of :q ( happens often :/ )
map q: :q

" turn on spellcheck for comments
" set spell spelllang=en 

" no temp shits
set nobackup
set noswapfile

" Auto reload if file was changed somewhere else
set autoread
au CursorHold * checktime

" auto center on matched string
noremap n nzz
noremap N Nzz

" Ctrl-s to save
nnoremap <C-s> :w<CR>
inoremap <C-s> <ESC>:w<CR>

" iterate buffers
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>
nnoremap <C-d> :bdelete<CR>

" more natural split
set splitbelow
set splitright

" iterate splits 
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" set title
set title

" indent related shits
set smartindent
set tabstop=4 
set softtabstop=4
set shiftwidth=4
set expandtab

" show hidden characters
set list
set listchars=tab:→\ ,space:·,nbsp:␣,trail:•,precedes:«,extends:»

" automatically wrap left and right
set whichwrap+=<,>,h,l,[,]

" don't jump wrapped line
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap <Down> gj
nnoremap <Up> gk
vnoremap <Down> gj
vnoremap <Up> gk

" fold
set foldmethod=syntax
set foldlevelstart=0

" better wrap
set wrap linebreak
set showbreak=" "

"""""""""""""""""""""""""""""
" Coc - lsp
""""""""""""""""""""""""""""""""
command! -nargs=0 Prettier :CocCommand prettier.formatFile

let g:coc_global_extensions = ['coc-explorer', 'coc-elixir', 'coc-diagnostic', 'coc-json', "coc-rust-analyzer", "coc-pairs", "coc-tsserver", "coc-json", "coc-html", "coc-css", "coc-python", "coc-snippets", "coc-syntax", "coc-word", "coc-tag", "coc-vetur", "coc-dictionary", "coc-vimlsp", "coc-flutter", "coc-clangd", "coc-eslint", "coc-prettier", "coc-metals"]

" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=50

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes
" disable signcolumn for tagbar, nerdtree, as thats useless
autocmd FileType tagbar setlocal signcolumn=no

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" use <cr> (return/enter key) for completion
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)
" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

""""""""""""""""""""""""""""""""
" Elixir - thanks to: https://bernheisel.com/blog/vim-elixir-ls-plug/
""""""""""""""""""""""""""""""""
" Define elixir_ls dictionary
let g:ElixirLS = {}
let ElixirLS.path = stdpath('config').'/plugged/elixir-ls'
let ElixirLS.lsp = ElixirLS.path.'/release/language_server.sh'
let ElixirLS.cmd = join([
        \ 'asdf install &&',
        \ 'mix do',
        \   'local.hex --force --if-missing,',
        \   'local.rebar --force,',
        \   'deps.get,',
        \   'compile,',
        \   'elixir_ls.release'
        \ ], ' ')

" run it in background
function ElixirLS.on_stdout(_job_id, data, _event)
  let self.output[-1] .= a:data[0]
  call extend(self.output, a:data[1:])
endfunction

let ElixirLS.on_stderr = function(ElixirLS.on_stdout)

function ElixirLS.on_exit(_job_id, exitcode, _event)
  if a:exitcode[0] == 0
    echom '>>> ElixirLS compiled'
  else
    echoerr join(self.output, ' ')
    echoerr '>>> ElixirLS compilation failed'
  endif
endfunction

function ElixirLS.compile()
  let me = copy(g:ElixirLS)
  let me.output = ['']
  echom '>>> compiling ElixirLS'
  let me.id = jobstart('cd ' . me.path . ' && git pull && ' . me.cmd, me)
endfunction

" update the Elixir language server
call coc#config('elixir', {
  \ 'command': g:ElixirLS.lsp,
  \ 'filetypes': ['elixir', 'eelixir']
  \})
call coc#config('elixir.pathToElixirLS', g:ElixirLS.lsp)

"""""""""""""""""""""""""""""""
" Coc-explorer
"""""""""""""""""""""""""""""""
nmap <leader>tt :CocCommand explorer --width 25<CR>
nmap <leader>e :CocCommand explorer --position floating<CR>

"""""""""""""""""""""""""""""""
" tags
"""""""""""""""""""""""""""""""
let g:tagbar_type_elixir = {
    \ 'ctagstype' : 'elixir',
    \ 'kinds' : [
        \ 'p:protocols',
        \ 'm:modules',
        \ 'e:exceptions',
        \ 'y:types',
        \ 'd:delegates',
        \ 'f:functions',
        \ 'c:callbacks',
        \ 'a:macros',
        \ 't:tests',
        \ 'i:implementations',
        \ 'o:operators',
        \ 'r:records'
    \ ],
    \ 'sro' : '.',
    \ 'kind2scope' : {
        \ 'p' : 'protocol',
        \ 'm' : 'module'
    \ },
    \ 'scope2kind' : {
        \ 'protocol' : 'p',
        \ 'module' : 'm'
    \ },
    \ 'sort' : 0
\ }
nnoremap <silent> <leader>tc :TagbarToggle<CR>

"""""""""""""""""""""""""""
" splits
"""""""""""""""""""""""""""
map - <C-W>-
map + <C-W>+

nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>

""""""""""""""""""""""""""
" Rust
""""""""""""""""""""""""""
" run rustfmt on save
let g:rustfmt_autosave = 1

"""""""""""""""""""""""""
" fzf
"""""""""""""""""""""""""
map! <C-\> <esc>\
map <C-\> <esc>\
nnoremap \f :Files<CR>
nnoremap \g :GFiles<CR>
nnoremap \b :Buffers<CR>
nnoremap \m :Maps<CR>
nnoremap \h :History<CR>
nnoremap \ch :History:<CR>
nnoremap \sh :History/<CR>
nnoremap \c :Commands<CR>
nnoremap \t :Tags<CR>
nnoremap \bt :BTags<CR>

""""""""""""""""""""""""
" Mundo - undo tree
""""""""""""""""""""""""
" Enable persistent undo so that undo history persists across vim sessions
set undofile
set undodir=~/.vim/undo

nnoremap <leader>tu :MundoToggle<CR>

""""""""""""""""""""""
" patch-1
""""""""""""""""""""""
let g:vim_json_syntax_conceal = 0

""""""""""""""""""""""
" Carbon
""""""""""""""""""""""
" ec - Execute Carbon
vnoremap <leader>ec :CarbonNowSh<CR>

"""""""""""""""""""""
" startify
"""""""""""""""""""""
let g:startify_fortune_use_unicode=1

"""""""""""""""""""
" cpp
"""""""""""""""""""
let c_no_curly_error=1

""""""""""""""""""""""""""
" handle too large files
""""""""""""""""""""""""""
" file is larger than 10mb
let g:LargeFile = 1024 * 1024 * 2
augroup LargeFile 
  au!
  autocmd BufReadPre * let f=getfsize(expand("<afile>")) | if f > g:LargeFile || f == -2 | call LargeFile() | endif
augroup END

function! LargeFile()
 " no syntax highlighting etc
 setlocal eventignore+=FileType
 " no wrap
 setlocal nowrap
 " no spell check
 setlocal nospell
 " no hidden character rendering
 setlocal nolist
 " disable signcolumn
 setlocal signcolumn=no
 " disable colorcolumn
 setlocal colorcolumn=
 " save memory when other file is viewed
 setlocal bufhidden=unload
 " is read-only (write with :w new_filename)
 setlocal buftype=nowrite
 " no undo possible
 setlocal undolevels=-1

 execute "silent! CocDisable"

 let b:airline_disable_statusline = 1

 " display message
 autocmd VimEnter *  echo "The file is larger than " . (g:LargeFile / 1024 / 1024) . " MB, so some options are changed (see .vimrc for details)."
endfunction

""""""""""""""""""""""""""""
" airline statusline
""""""""""""""""""""""""""""
let g:airline_theme='simple'
let g:airline#extensions#tabline#enabled = 1
" ease buffer switching by pressing ctrl + <buffer_number> ex. C-2
let g:airline#extensions#tabline#buffer_nr_show = 1

""""""""""""""""""""""""""
" autosave
""""""""""""""""""""""""""
" enable AutoSave on Vim startup
let g:auto_save = 1

""""""""""""""""""""""""
" rainbow brackets
""""""""""""""""""""""""
let g:rainbow_active = 1

"""""""""""""""""""""""""""""""""""""
" make life a liltle less miserable
"""""""""""""""""""""""""""""""""""""
nnoremap <Leader><Leader> :source $MYVIMRC<cr>
nnoremap <Leader>v :e $MYVIMRC<cr>

"""""""""""""""""""""""""""""""""""
" ctags - gutentags
"""""""""""""""""""""""""""""""""""
" enable gtags module
let g:gutentags_modules = ['ctags', 'gtags_cscope']

" rg (ripgrep) takes care of .gitignore
let g:gutentags_file_list_command = 'rg --files'

" config project root markers.
let g:gutentags_project_root = ['.root']

" generate datebases in my cache directory, prevent gtags files polluting my project
let g:gutentags_cache_dir = expand('~/.cache/tags')

" change focus to quickfix window after search (optional).
let g:gutentags_plus_switch = 1

"""""""""""""""""""""""""""""
" dart & flutter
"""""""""""""""""""""""""""""
let g:lsc_auto_map = v:true

