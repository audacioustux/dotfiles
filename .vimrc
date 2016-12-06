" don't be compatible :)
set nocompatible

call plug#begin()
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'vim-syntastic/syntastic'
Plug 'octol/vim-cpp-enhanced-highlight', { 'for': ['c', 'cpp', 'objc'] }
Plug 'vim-jp/cpp-vim', { 'for': ['c', 'cpp', 'objc'] }
Plug 'ervandew/supertab'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ryanoasis/vim-devicons'
Plug 'mbbill/undotree'
Plug 'tomasr/molokai'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
" Autocompleter and snippets {{{
function! BuildYCM(info)
  " - force:  set on PlugInstall! or PlugUpdate!
  if a:info.status == 'installed' || a:info.force
    !./install.py --clang-completer
  endif
endfunction
Plug 'Valloric/YouCompleteMe', { 'do': function('BuildYCM'), 'on': [] }
Plug 'SirVer/ultisnips', { 'on': [] }
Plug 'honza/vim-snippets'
" Defer YouCompleteMe and UltiSnips loading until insert mode is entered {{{
augroup load_us_ycm
  autocmd!
  autocmd InsertEnter * if !exists(':UltiSnipsEdit')
        \|      call plug#load('ultisnips', 'YouCompleteMe')
        \|  else
        \|      call plug#load('YouCompleteMe')
        \|  endif
        \|  autocmd! load_us_ycm
augroup END
" }}}
call plug#end()
filetype plugin indent on
syntax enable
set t_Co=256
colorscheme molokai
let g:molokai_original = 1
set encoding=utf8
syntax enable
set background=dark
set number
set autoindent
set tabstop=4 shiftwidth=4 expandtab softtabstop=4
set lazyredraw
set ttyfast
set mouse=a
set hidden
set virtualedit=onemore
set scrolloff=3
set backspace=indent,eol,start  " Backspace for dummies
set linespace=0                 " No extra spaces between rows
set number                      " Line numbers on
set showmatch                   " Show matching brackets/parenthesis
set incsearch                   " Find as you type search
set hlsearch                    " Highlight search terms
set winminheight=0              " Windows can be 0 line high
set ignorecase                  " Case insensitive search
set smartcase                   " Case sensitive when uc present
set wildmenu                    " Show list instead of just completing
set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
set scrolljump=5                " Lines to scroll when cursor leaves screen
set scrolloff=3                 " Minimum lines to keep above and below cursor
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace
set pastetoggle=<F12>
autocmd FileType nerdtree setlocal nolist
let g:webdevicons_enable = 1
" Airline----------
set laststatus=2
let g:airline_theme='wombat'
let g:airline_powerline_fonts                   = 1
let g:airline_detect_modified                   = 1
let g:airline_detect_paste                      = 1
let g:airline#extensions#syntastic#enabled      = 1
let g:airline#extensions#ycm#enabled            = 1
let g:airline#extensions#ycm#error_symbol       = 'E:'
let g:airline#extensions#ycm#warning_symbol     = 'W:'
" UndoTree------------
nnoremap <F5> :UndotreeToggle<CR>
if !exists('g:undotree_WindowLayout')
  let g:undotree_WindowLayout = 3
endif
" undotree window width
if !exists('g:undotree_SplitWidth')
  if exists('g:undotree_ShortIndicators')
    let g:undotree_SplitWidth = 15
  else
    let g:undotree_SplitWidth = 20
  endif
endif
" syntastic----------
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_cpp_checkers = ['gcc']
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -Wall -std=c++0x'
let g:syntastic_cpp_check_header = 1

let g:syntastic_error_symbol = "✗"
let g:syntastic_warning_symbol = "⚠"
highlight SyntasticError guibg=#FF7033
highlight SyntasticStyleWarning guibg=#3CDEBB
function! SyntasticCheckHook(errors)
        if !empty(a:errors)
            let g:syntastic_loc_list_height = min([len(a:errors), 10])
        endif
endfunction
" YouCompleteMe configuration {{{
if !exists('g:ycm_semantic_triggers')
    let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers.cpp = ['->', '.', '::', 're!gl']
let g:ycm_use_ultisnips_completer = 1
let g:ycm_add_preview_to_completeopt = 1
let g:ycm_always_populate_location_list = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_confirm_extra_conf = 0
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_seed_identifiers_with_syntax = 1
let g:ycm_global_ycm_extra_conf = '~/.config/nvim/ycm_extra_conf.py'
let g:ycm_extra_conf_vim_data   = [ '&filetype' ]
let g:ycm_extra_conf_globlist = [
\ '~/*' ]
" YouCompleteMe and UltiSnips compatibility, with the helper of supertab
let g:ycm_key_list_select_completion   = ['<C-j>', '<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-k>', '<C-p>', '<Up>']

let g:SuperTabDefaultCompletionType    = '<C-n>'
let g:SuperTabCrMapping                = 0

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
"{{{
" NERDTree --------------
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDTreeQuitOnOpen = 1
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeMouseMode=2
let g:NERDTreeWinSize=20
let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$','\.out$', '\.bzr$', '\.o$', '\.dat$', '\.jpg$', '\.jpeg$']
" Nerdtree toogle
nmap <f2> :NERDTree<CR>
imap <f2> <Esc>:NERDTree<CR>
" build and run
nmap <f9> :update<CR> :!clear<CR> :! g++ -o "%:p:h/.%:t.out" "%" -std=c++11 && "%:p:h/.%:t.out"<CR>
imap <f9> <Esc>:update<CR> :!clear<CR> :! g++ -o "%:p:h/.%:t.out" "%" -std=c++11 && "%:p:h/.%:t.out"<CR>
" Highlight advance---------
let g:cpp_class_scope_highlight = 1
let g:cpp_experimental_template_highlight = 1
let c_no_curly_error=1
