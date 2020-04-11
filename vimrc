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
"Plug 'ctrlpvim/ctrlp.vim'
Plug 'dense-analysis/ale'
Plug 'arcticicestudio/nord-vim'
Plug 'ervandew/supertab'
Plug 'vim-scripts/AutoComplPop'
Plug 'mileszs/ack.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'vim-scripts/taglist.vim'
Plug 'majutsushi/tagbar'
Plug 'xolox/vim-misc'
Plug 'hdiniz/vim-gradle'
Plug 'hashivim/vim-terraform'
Plug 'vim-syntastic/syntastic'
Plug 'juliosueiras/vim-terraform-completion'
Plug 'emacs-helm/helm'
Plug 'Shougo/deoplete.nvim'
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'
Plug 'roblillack/vim-bufferlist'
Plug 'bounceme/dim-jump'
Plug 'tpope/vim-fugitive'
Plug 'mihaifm/bufstop'
Plug 'ludovicchabant/vim-gutentags'
call plug#end()

let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog  = '/usr/local/bin/python3'
let g:deoplete#enable_at_startup = 1

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

set browsedir=current

autocmd Filetype yaml setlocal ts=2 sw=2 sts=2 expandtab
autocmd Filetype scm setlocal ts=2 sw=2 sts=2 expandtab
autocmd Filetype dart setlocal ts=4 sw=4 sts=4 expandtab

let g:syntastic_python_python_exec = 'python3'


"" Erlang/Elixir setttings
let g:erlang_show_errors = 1
let g:mix_format_on_save = 1
"let g:mix_format_options = '--check-formatted'

"source ~/.flutter.vim
autocmd BufWritePre * %s/\s\+$//e

let g:ackprg = 'ag -G ''Projects|form3'' --vimgrep'

set redrawtime=10000
set ruler
set showmatch

" Go to next buffer
nnoremap <S-Tab> :bn<CR>

set tw=72
set fo+=t

set autowrite


" Go setttings
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

"let g:go_list_type = "quickfix"
let g:go_fmt_command = "goimports"
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1

" Build/Test on save.
augroup auto_go
	autocmd!
	autocmd BufWritePost *.go :GoBuild
	"autocmd BufWritePost *_test.go :GoTest
augroup end


set number
highlight LineNr term=bold cterm=NONE ctermfg=DarkGray ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE

nmap =j :%!python -m json.tool<CR>

colorscheme nord


let g:fzf_action = {
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit'
      \ }
""
"" Configure Ag to always start searching from the project
"" root rather than using cwd
let g:ag_working_path_mode="c"

"" FZF custom search paths
command! SearchFile call fzf#run({'source': 'find ~/go/src/github.com/form3tech/ ~/go/src/github.com/form3tech-oss/  ~/Projects/ -type f -not -path ''*/\.git/*'' -not -path ''*/_build/*''', 'sink':  'edit', 'down': '15%'})

map <C-f> :SearchFile <CR>

""this will cause all splits to happen below (including term)
set splitbelow

"open a terminal, by hitting Shift-T
map <S-T> :term <CR>


"" Terraform configuration
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" (Optional)Remove Info(Preview) window
set completeopt-=preview

" (Optional)Hide Info(Preview) window after completions
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" (Optional) Enable terraform plan to be include in filter
let g:syntastic_terraform_tffilter_plan = 1

" (Optional) Default: 0, enable(1)/disable(0) plugin's keymapping
let g:terraform_completion_keys = 1

" (Optional) Default: 1, enable(1)/disable(0) terraform module registry completion
let g:terraform_registry_module_completion = 0


let g:deoplete#omni_patterns = {}
let g:deoplete#omni_patterns.terraform = '[^ *\t"{=$]\w*'
let g:deoplete#enable_at_startup = 1
call deoplete#initialize()

" Remember open buffers
set viminfo^=%

" easy buffer switch
map <C-B> :BufstopFast<CR>
""map <C-B> :call BufferList()<CR>
""let g:BufferListWidth = 25
""let g:BufferListMaxWidth = 50
""hi BufferSelected term=reverse ctermfg=white ctermbg=NONE cterm=bold
""hi BufferNormal term=NONE ctermfg=gray ctermbg=NONE cterm=NONE

" status line style
hi Search cterm=NONE ctermfg=grey ctermbg=NONE
hi QuickFixLine cterm=NONE ctermfg=grey ctermbg=NONE
hi StatusLine cterm=NONE ctermfg=grey ctermbg=NONE
hi StatusLineNC cterm=NONE ctermfg=grey ctermbg=NONE

set laststatus=0
