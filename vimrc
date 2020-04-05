execute pathogen#infect()
call plug#begin('~/.vim/plugged')
Plug 'vim-erlang/vim-erlang-runtime'
Plug 'elmcast/elm-vim'
Plug 'pangloss/vim-javascript'
Plug 'ryym/vim-riot'
Plug 'fatih/vim-go'
Plug 'dart-lang/dart-vim-plugin'
Plug 'thosakwe/vim-flutter'
Plug 'elixir-editors/vim-elixir'
Plug 'mhinz/vim-mix-format'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'dense-analysis/ale'
Plug 'arcticicestudio/nord-vim'
Plug 'ervandew/supertab'
Plug 'vim-scripts/AutoComplPop'
Plug 'mileszs/ack.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'vim-scripts/taglist.vim'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'
Plug 'artur-shaik/vim-javacomplete2'
Plug 'hdiniz/vim-gradle'
"Plug 'itchyny/vim-haskell-indent'
"Plug 'vim-airline/vim-airline'
call plug#end()


call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'nbouscal/vim-stylish-haskell'
Plugin 'slashmili/alchemist.vim'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'mileszs/ack.vim'
call vundle#end()

"set termguicolors
" colorscheme colibri

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_erlc_include_path = "ebin"

let g:syntastic_javascript_checkers = ['jshint']

let g:syntastic_go_checkers = ['errcheck', 'go']

syntax on
filetype plugin indent on
set tabstop     =4
set softtabstop =4
set shiftwidth  =4
set expandtab

let g:elm_format_autosave = 1
let g:erlang_show_errors = 1
let dart_format_on_save = 1
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
autocmd Filetype dart setlocal ts=4 sw=4 sts=4 expandtab

let g:syntastic_python_python_exec = 'python3'

let g:mix_format_on_save = 1
"let g:mix_format_options = '--check-formatted'

"source ~/.flutter.vim
autocmd BufWritePre * %s/\s\+$//e

set runtimepath^=~/.vim/bundle/ctrlp.vim

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

set redrawtime=10000
set ruler
set showmatch

nnoremap <C-Tab> :bn<CR>
nnoremap <C-S-Tab> :bp<CR>

set tw=72
set fo+=t

set autowrite

map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>
autocmd FileType go nmap <leader>b  <Plug>(go-build)
autocmd FileType go nmap <leader>r  <Plug>(go-run)
autocmd Filetype go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
autocmd Filetype go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
autocmd Filetype go command! -bang AS call go#alternate#Switch(<bang>0, 'split')
autocmd Filetype go command! -bang AT call go#alternate#Switch(<bang>0, 'tabe')
autocmd FileType go nmap <Leader>i <Plug>(go-info)
let g:go_auto_type_info = 1

let g:go_list_type = "quickfix"
let g:go_fmt_command = "goimports"
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1

set number
highlight LineNr term=bold cterm=NONE ctermfg=DarkGray ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE

nmap =j :%!python -m json.tool<CR>

colorscheme nord


let g:fzf_action = {
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit'
      \ }
nnoremap <c-p> :FZF<cr>
augroup fzf
  autocmd!
  autocmd! FileType fzf
  autocmd  FileType fzf set laststatus=0 noshowmode noruler
    \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
augroup END

""" This is the default extra key bindings
""let g:fzf_action = {
""  \ 'ctrl-t': 'tab split',
""  \ 'ctrl-x': 'split',
""  \ 'ctrl-v': 'vsplit' }
""
""" An action can be a reference to a function that processes selected lines
""function! s:build_quickfix_list(lines)
""  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
""  copen
""  cc
""endfunction
""
""let g:fzf_action = {
""  \ 'ctrl-q': function('s:build_quickfix_list'),
""  \ 'ctrl-t': 'tab split',
""  \ 'ctrl-x': 'split',
""  \ 'ctrl-v': 'vsplit' }
""
""" Default fzf layout
""" - down / up / left / right
""let $FZF_DEFAULT_OPTS = '--reverse'
""let g:fzf_layout = { 'down': '~10%' }
""
""" You can set up fzf window using a Vim command (Neovim or latest Vim 8 required)
""let g:fzf_layout = { 'window': 'enew' }
""let g:fzf_layout = { 'window': '-tabnew' }
""let g:fzf_layout = { 'window': '10new' }
""
""" Customize fzf colors to match your color scheme
""" - fzf#wrap translates this to a set of `--color` options
""let g:fzf_colors =
""\ { 'fg':      ['fg', 'Normal'],
""  \ 'bg':      ['bg', 'Normal'],
""  \ 'hl':      ['fg', 'Comment'],
""  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
""  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
""  \ 'hl+':     ['fg', 'Statement'],
""  \ 'info':    ['fg', 'PreProc'],
""  \ 'border':  ['fg', 'Ignore'],
""  \ 'prompt':  ['fg', 'Conditional'],
""  \ 'pointer': ['fg', 'Exception'],
""  \ 'marker':  ['fg', 'Keyword'],
""  \ 'spinner': ['fg', 'Label'],
""  \ 'header':  ['fg', 'Comment'] }
""
""" Enable per-command history
""" - History files will be stored in the specified directory
""" - When set, CTRL-N and CTRL-P will be bound to 'next-history' and
"""   'previous-history' instead of 'down' and 'up'.
""let g:fzf_history_dir = '~/.local/share/fzf-history'

map <C-g> :Ag <CR>
map <C-f> :Files <CR>

let g:easytags_async = 1


autocmd FileType java setlocal omnifunc=javacomplete#Complete
nmap <F4> <Plug>(JavaComplete-Imports-AddSmart)
imap <F4> <Plug>(JavaComplete-Imports-AddSmart)
nmap <F5> <Plug>(JavaComplete-Imports-Add)
imap <F5> <Plug>(JavaComplete-Imports-Add)
nmap <F6> <Plug>(JavaComplete-Imports-AddMissing)
imap <F6> <Plug>(JavaComplete-Imports-AddMissing)
nmap <F7> <Plug>(JavaComplete-Imports-RemoveUnused)
imap <F7> <Plug>(JavaComplete-Imports-RemoveUnused)


"let g:ale_java_javac_executable = 'javac -cp ~/Library/ApplicationSupport/IdeaIC2019.3/lombok-plugin/lib/lombok-plugin-0.29-2019.3.jar'
