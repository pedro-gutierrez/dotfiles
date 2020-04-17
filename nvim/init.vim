call plug#begin("~/.vim/plugged")
Plug 'sheerun/vim-polyglot'
Plug 'vim-erlang/vim-erlang-runtime'
Plug 'elixir-editors/vim-elixir'
Plug 'arcticicestudio/nord-vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
" File Explorer with Icons
"Plug 'scrooloose/nerdtree'
"Plug 'ryanoasis/vim-devicons'
" File Search
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'fatih/vim-go'
"Plug 'ctrlpvim/ctrlp.vim'
call plug#end()


let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog  = '/usr/local/bin/python3'

let g:erlang_show_errors = 1
let g:mix_format_on_save = 1

let g:coc_global_extensions = ['coc-emmet', 'coc-css', 'coc-html', 'coc-json', 'coc-prettier', 'coc-tsserver']

nmap =j :%!python -m json.tool<CR>

set encoding=UTF-8
colorscheme nord

syntax on
syntax enable
filetype plugin indent on
set tabstop     =4
set softtabstop =4
set shiftwidth  =4
set expandtab

set nobackup
set noswapfile
set title
set visualbell
set noerrorbells

set backspace=indent,eol,start
set autoindent
set copyindent

set showmatch

set ignorecase
set smartcase
set smarttab

set redrawtime=10000
set ruler
set showmatch


set tw=72
set fo+=t

set autowrite

set browsedir=current

autocmd Filetype yaml setlocal ts=2 sw=2 sts=2 expandtab
autocmd Filetype scm setlocal ts=2 sw=2 sts=2 expandtab
autocmd Filetype dart setlocal ts=4 sw=4 sts=4 expandtab

set number

set splitbelow

set viminfo^=%



set laststatus=0

map <C-k> :bd! <CR>
"map <C-c> :q <CR>
map <C-x> :on <CR>
map <S-T> :term <CR>
map <C-f> :Sexplore <CR>
nnoremap <S-Tab> :bn<CR>
map <C-B> :buffers <CR>

""let g:NERDTreeShowHidden = 1
""let g:NERDTreeMinimalUI = 1
""let g:NERDTreeIgnore = []
""let g:NERDTreeStatusline = ''
""" Automaticaly close nvim if NERDTree is only thing left open
""autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
""" Toggle
""nnoremap <silent> <C-b> :NERDTreeToggle<CR>

nnoremap <C-p> :FZF<CR>
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit'
  \}
" requires silversearcher-ag
" used to ignore gitignore files
let $FZF_DEFAULT_COMMAND = 'ag -g ""'

" open new split panes to right and below
set splitright
set splitbelow

" turn terminal to normal mode with escape
tnoremap <Esc> <C-\><C-n>

" use alt+hjkl to move between split/vsplit panels
tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

" start terminal in insert mode
au BufEnter * if &buftype == 'terminal' | :startinsert | endif

" open terminal on ctrl+;
" uses zsh instead of bash
function! OpenTerminal()
  split term://zsh
  resize 10
endfunction
nnoremap <c-n> :call OpenTerminal()<CR>

" Color customizations
set termguicolors
" hi Search cterm=NONE ctermfg=grey ctermbg=NONE
hi Search cterm=NONE ctermfg=NONE ctermbg=NONE gui=NONE guifg=#616E88 guibg=NONE
"hi QuickFixLine cterm=NONE ctermfg=grey ctermbg=NONE
hi QuickFixLine cterm=NONE ctermfg=NONE ctermbg=NONE gui=NONE guifg=#616E88 guibg=NONE
hi StatusLine cterm=NONE ctermfg=NONE ctermbg=NONE gui=NONE guifg=#616E88 guibg=NONE
hi StatusLineNC cterm=NONE ctermfg=NONE ctermbg=NONE gui=NONE guifg=#616E88 guibg=NONE
highlight LineNr term=bold cterm=NONE ctermfg=NONE ctermbg=NONE gui=NONE guifg=#616E88 guibg=NONE
""hi LineNr term=bold ctermfg=black ctermbg=NONE gui=NONE cterm=NONE
hi Normal guibg=NONE ctermbg=NONE



" -------------------------------------------------------------------------------------------------
" coc.nvim default settings
" -------------------------------------------------------------------------------------------------

" if hidden is not set, TextEdit might fail.
set hidden
" Better display for messages
set cmdheight=2
" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300
" don't give |ins-completion-menu| messages.
set shortmess+=c
" always show signcolumns
"set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use U to show documentation in preview window
nnoremap <silent> U :call <SID>show_documentation()<CR>

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
vmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

" disable vim-go :GoDef short cut (gd)
" this is handled by LanguageClient [LC]
let g:go_def_mapping_enabled = 0
