call plug#begin("~/.vim/plugged")
Plug 'dense-analysis/ale'
Plug 'sheerun/vim-polyglot'
Plug 'vim-erlang/vim-erlang-runtime'
Plug 'elixir-editors/vim-elixir'
Plug 'mhinz/vim-mix-format'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'fatih/vim-go'
Plug 'airblade/vim-rooter'
Plug 'jreybert/vimagit'
Plug 'hashivim/vim-terraform'
Plug 'vim-syntastic/syntastic'
Plug 'juliosueiras/vim-terraform-completion'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'arcticicestudio/nord-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'
Plug 'rakr/vim-one'
call plug#end()

let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog  = '/usr/local/bin/python3'

let g:erlang_show_errors = 1
let g:mix_format_on_save = 1

nmap =j :%!python -m json.tool<CR>

set encoding=UTF-8

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

set redrawtime =10000
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

set laststatus=2

map <C-a> :%bd <CR>
map <C-k> :bd! <CR>
map <C-x> :on <CR>
"map <S-T> :term <CR>
map <C-f> :Sexplore <CR>
nnoremap <S-Tab> :bn<CR>
map <C-B> :Buffers <CR>

nnoremap <C-p> :Files<CR>
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

" start terminal in insert mode
au BufEnter * if &buftype == 'terminal' | :startinsert | endif

""" Color customizations
""colorscheme nord
""set termguicolors
""hi Search cterm=NONE ctermfg=grey ctermbg=NONE
""hi Search cterm=NONE ctermfg=NONE ctermbg=NONE gui=NONE guifg=yellow guibg=#616E88
""hi QuickFixLine cterm=NONE ctermfg=grey ctermbg=NONE
""hi QuickFixLine cterm=NONE ctermfg=NONE ctermbg=NONE gui=NONE guifg=#616E88 guibg=NONE
""hi StatusLine cterm=NONE ctermfg=NONE ctermbg=NONE gui=NONE guifg=#616E88 guibg=NONE
""hi StatusLineNC cterm=NONE ctermfg=NONE ctermbg=NONE gui=NONE guifg=#616E88 guibg=NONE
""hi LineNr term=bold cterm=NONE ctermfg=NONE ctermbg=NONE gui=NONE guifg=#616E88 guibg=NONE
""hi LineNr term=bold ctermfg=black ctermbg=NONE gui=NONE cterm=NONE
""hi Normal guibg=NONE ctermbg=NONE
""hi VertSplit guibg=#2e3340 guifg=#2e3340 ctermbg=DarkGray ctermfg=black

"hi Pmenu ctermfg=0 ctermbg=11
"hi PmenuSel ctermfg=white guifg=white
"hi PmenuThumb ctermfg=15 ctermbg=0 guifg=white guibg=DarkGrey

""hi Comment ctermbg=NONE ctermfg=DarkGray

set termguicolors
set background=light
let g:airline_theme='one'
colorscheme one
hi Normal guibg=NONE ctermbg=NONE
hi SignColumn guibg=NONE
"" Use cyan for identifiers
call one#highlight('Identifier', '56b6c2', '', '')
call one#highlight('yamlConstant', 'c678dd', '', '')

" This unsets the last search pattern" register by hitting return
nnoremap <CR> :noh<CR><CR>

let g:netrw_browse_split = 0
"
au filetype go inoremap <buffer> . .<C-x><C-o>
au filetype go noremap <buffer> <C-]> :GoDef <CR>

"" COC Settings
set cmdheight=2
set updatetime=300
set shortmess+=c

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>"
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

"" Change cursor shape
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"

set showtabline=0
set guioptions-=e
set laststatus=2


"" Custom utilities

function! GitStatus()
  execute "! git status"
endfunction

function! GitPushAll()
  call inputsave()
  call GitStatus()
  let msg = input('[Git Push] Enter commit message: ')
  call inputrestore()
  execute "! git add . ; git commit -m '" . msg . "'; git push"
endfunction

noremap <S-P> :call GitPushAll() <CR>
noremap <S-S> :call GitStatus() <CR>



function! TerraformInit()
    execute "! cd tf; terraform init"
endfunction

function! TerraformPlan()
    execute "! cd tf; terraform plan"
endfunction

noremap <C-t>i :call TerraformInit() <CR>
noremap <C-t>p :call TerraformPlan() <CR>


function! SearchReplace()
  call inputsave()
  let file = input('[Replace] File pattern: ')
  let search = input('[Replace] Search: ')
  let replace = input('[Replace] Replace: ')
  call inputrestore()
  execute "! echo \" . file . "\""
  execute "! find . -name \"" . file . "\" -exec sed -i '' 's/" . search . "/" . replace . "/g' {} \\;"
endfunction

noremap <C-x>s :call SearchReplace() <CR>

noremap <C-x>w :%s/\s\+$//e <CR>

set noshowmode

function! SyntaxItem()
  return synIDattr(synID(line("."),col("."),1),"name")
endfunction

let g:airline_section_z = '%{SyntaxItem()}'

"" Show the syntax highlight group at the current cursor position
""map <C-x>z :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
""\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
""\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
