set nocompatible
set hidden
set number
set nowrap
set tabstop=4
set backspace=indent,eol,start
set autoindent
set copyindent
set shiftwidth=4
set shiftround
set showmatch
set ignorecase
set smartcase
set smarttab
set hlsearch
set incsearch
set history=1000
set undolevels=1000
set wildignore=*.swp,*.bak,*.pyc,*.class
set title
set visualbell
set noerrorbells
set nobackup
set noswapfile


call plug#begin()
Plug 'sheerun/vim-polyglot'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'hashivim/vim-terraform'

Plug 'elixir-lang/vim-elixir'
Plug 'avdgaag/vim-phoenix'
Plug 'mmorearty/elixir-ctags'
Plug 'mattreduce/vim-mix'
Plug 'BjRo/vim-extest'
Plug 'frost/vim-eh-docs'
Plug 'slashmili/alchemist.vim'
Plug 'tpope/vim-endwise'
Plug 'jadercorrea/elixir_generator.vim'
Plug 'mhinz/vim-mix-format'
Plug 'vim-erlang/vim-erlang-tags'
Plug 'vim-erlang/vim-erlang-runtime'
Plug 'vim-erlang/vim-erlang-omnicomplete'
Plug 'vim-erlang/vim-erlang-compiler'
call plug#end()

" Terraform
let g:terraform_align=1
let g:terraform_fmt_on_save=1

" Elixir 
let g:mix_format_on_save = 1

" YAML
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

highlight VertSplit cterm=NONE
set fillchars+=vert:\ 
"highlight StatusLine ctermfg=black ctermbg=NONE cterm=NONE

" Search
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit'
 \}

" let g:fzf_layout = { 'down': '~30%' }
let g:fzf_preview_window = []

" Custom shortcuts
map <C-p> :Files<CR>
map <C-k> :bd <CR>
map <C-x> :on <CR>
map <C-f> :Sexplore <CR>
map <C-B> :Buffers <CR>
nmap <C-j> :call CocAction('jumpDefinition', 'drop')<CR>
nnoremap ; :

" Use Q for formatting the current paragraph (or selection)
vmap Q gq
nmap Q gqap

map <A-w> <C-w>w
map <A-h> <C-w>h
map <A-j> <C-w>j
map <A-k> <C-w>k
map <A-l> <C-w>l

nmap <silent> ,/ :nohlsearch<CR>
