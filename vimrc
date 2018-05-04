execute pathogen#infect()
call plug#begin('~/.vim/plugged')
Plug 'vim-erlang/vim-erlang-runtime'
Plug 'elmcast/elm-vim'
Plug 'pangloss/vim-javascript'
Plug 'ryym/vim-riot'
Plug 'fatih/vim-go'
"Plug 'itchyny/vim-haskell-indent'
"Plug 'vim-airline/vim-airline'
call plug#end()


call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'nbouscal/vim-stylish-haskell'
call vundle#end()

"set termguicolors
"colorscheme colibri

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_erlc_include_path = "ebin"

let g:syntastic_javascript_checkers = ['jshint']

syntax on
filetype plugin indent on
set tabstop     =4
set softtabstop =4 
set shiftwidth  =4
set expandtab

let g:elm_format_autosave = 1
let g:erlang_show_errors = 1
set sessionoptions-=options

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

"set laststatus=2
set statusline=
set statusline +=%1*\ %n\ %*            "buffer number
set statusline +=%5*%{&ff}%*            "file format
set statusline +=%3*%y%*                "file type
set statusline +=%4*\ %<%F%*            "full path
set statusline +=%2*%m%*                "modified flag
set statusline +=%1*%=%5l%*             "current line
set statusline +=%2*/%L%*               "total lines
set statusline +=%1*%4v\ %*             "virtual column number
set statusline +=%2*0x%04B\ %*          "character under cursor

set autochdir                   " Changes the cwd to the directory of the current
                                " buffer whenever you switch buffers.
set browsedir=current 

autocmd Filetype yaml setlocal ts=2 sw=2 sts=2 expandtab
autocmd Filetype scm setlocal ts=2 sw=2 sts=2 expandtab
let g:syntastic_python_python_exec = 'python3'
