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

" language server protocol (lsp)
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" status/tab-line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" theme
" Plug 'sainnhe/gruvbox-material'
" Plug 'morhetz/gruvbox'
Plug 'sainnhe/sonokai'
" theme sync with tmux
Plug 'edkolev/tmuxline.vim'
" vertical line at each indent level
Plug 'Yggdroot/indentLine'
" transparent background
Plug 'tribela/vim-transparent'
" fzf
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
" cpp better syntax
Plug 'jackguo380/vim-lsp-cxx-highlight'
" undo tree - visualization
Plug 'simnalamburt/vim-mundo'
" multicursor
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
" for easier vim+tmux window navigation
Plug 'christoomey/vim-tmux-navigator'
" a collection of language packs
Plug 'sheerun/vim-polyglot'
" elixir
Plug 'tpope/vim-dadbod'
Plug 'elixir-editors/vim-elixir'
" snippets
Plug 'honza/vim-snippets'

" Initialize plugin system
call plug#end()

let g:coc_global_extensions = ['coc-explorer', 'coc-java', 'coc-json', 'coc-xml', 'coc-rust-analyzer', 'coc-tsserver', 'coc-clangd', 'coc-cmake', 'coc-eslint', 'coc-vetur', 'coc-prettier', 'coc-elixir', 'coc-snippets', 'coc-git', 'coc-pairs', 'coc-diagnostic', 'coc-metals']

" the prefix to use for leader commands
let mapleader=" "
" Use <c-space> as leader in insert mode.
if has('nvim')
  imap <c-space> <esc><leader>
else
  imap <c-@> <esc><leader>
endif

" sync clipboard to OS
set clipboard=unnamed

" switches to absolute line numbers automatically when relative numbers don't make sense.
set number relativenumber
augroup numberModeToggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu | set rnu   | endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu | set nornu | endif
augroup END
" toggle line number mode
nnoremap <silent> <leader>nu :set relativenumber!<cr>

augroup cursorLineToggle
  autocmd!
  autocmd InsertEnter * set cul | hi! link CursorLineNr Green
  autocmd InsertLeave * set nocul | hi! link CursorLineNr FG
augroup END

" set syntax theme
syntax on
set termguicolors " try `:set notermguicolors` for comparison
let g:sonokai_style = 'shusia'
let g:sonokai_enable_italic = 1
let g:sonokai_current_word = 'bold'
let g:sonokai_diagnostic_text_highlight = 1
let g:sonokai_diagnostic_line_highlight = 1
let g:sonokai_better_performance = 1
set background=dark
colorscheme sonokai
" set airline theme
let g:airline_theme='sonokai'

" indent related shits
set smartindent
set tabstop=4 
set softtabstop=4
set shiftwidth=4
set expandtab

" set column guide
set textwidth=80
set colorcolumn=+1

" Make searching case insensitive
set ignorecase
" ... unless the query has capital letters.
set smartcase
" highlight all results
set nohlsearch 
" show search results as you type
set incsearch 

" keep the cursor a bit far than the edges (bottom & top) always
set scrolloff=10
" for Horizontal scrolling
set sidescroll=1
set sidescrolloff=15

" enable mouse functionalities in all mode
set mouse=a

" prevent common mistake of pressing q: instead of :q ( happens often :/ )
map q: :q

" no temp shits
set nobackup
set noswapfile
" persistent undo history
let s:undoDir = "/tmp/.undodir_" . $USER
if !isdirectory(s:undoDir)
    call mkdir(s:undoDir, "", 0700)
endif
let &undodir=s:undoDir
set undofile
" mundo
nnoremap <leader>tm :MundoToggle<CR>

" Auto reload if file was changed somewhere else
set autoread
au CursorHold * checktime

" Ctrl-w to save
nnoremap <C-w> :w<CR>
inoremap <C-w> <ESC>:w<CR>

" auto center on matched string
noremap n nzz
noremap N Nzz

" resize splits
nnoremap <silent> <Leader>+ :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <silent> <Leader>- :exe "resize " . (winheight(0) * 2/3)<CR>
nnoremap <silent> <Leader>< :exe "vertical resize " . (winwidth(0) * 3/2)<CR>
nnoremap <silent> <Leader>> :exe "vertical resize " . (winwidth(0) * 2/3)<CR>

" when splitting horizontally, show split window on bottom
set splitbelow
"  when splitting vertically, show split window on right
set splitright

" set title
set title

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

" Configuration for vim-scala
au BufRead,BufNewFile *.sbt set filetype=scala

""""""""""""""""""""""""""""""
" Coc
""""""""""""""""""""""""""""""
" Set internal encoding of vim, not needed on neovim, since coc.nvim using some
" unicode characters in the file autoload/float.vim
set encoding=utf-8

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

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

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

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>x  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

" coc-explorer - file manager/tree
nnoremap <leader>e :CocCommand explorer --position floating<CR>

"""""""""" coc-snippets
" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'

" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <Plug>(coc-snippets-expand-jump)

"""""""""""""""""""""""""""'
" fzf
"""""""""""""""""""""""""""'
nnoremap <leader>f :Files<CR>
nnoremap <leader>gf :GFiles<CR>
nnoremap <leader>gf? :GFiles?<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>h :History<CR>
nnoremap <leader>tt :Tags<CR>
nnoremap <leader>ag :Ag<CR>
nnoremap <leader>rg :Rg<CR>
nnoremap <leader>l :Lines<CR>
nnoremap <leader>m :Marks<CR>
nnoremap <leader>w :Windows<CR>
nnoremap <leader>lc :Locate<CR>
nnoremap <leader>gc :Commits<CR>
nnoremap <leader>ht :HelpTags<CR>
nnoremap <leader><leader>s :Snippets<CR>
nnoremap <leader><leader>ft :Filetypes<CR>
nnoremap <leader><leader>hc :History:<CR>
nnoremap <leader><leader>hs :History/<CR>
nnoremap <leader><leader>m :Maps<CR>
nnoremap <leader><leader>c :Colors<CR>
nnoremap <leader><leader>bc :BCommits<CR>
nnoremap <leader><leader>l :BLines<CR>
nnoremap <leader><leader>t :BTags<CR>

"""""""""""""""""""""""""""""
" patch
"""""""""""""""""""""""""""""
" for italic in tmux - https://gist.github.com/gutoyr/4192af1aced7a1b555df06bd3781a722
set t_ZH=[3m
set t_ZR=[23m

" use asdf node version for cock
let g:coc_node_path = trim(system('asdf which node'))
