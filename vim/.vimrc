set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'FredKSchott/CoVim'
Plugin 'guyzmo/vim-etherpad'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'scrooloose/nerdtree'
Plugin 'tomasr/molokai'
Plugin 'vim-python/python-syntax'
Plugin 'justinmk/vim-syntax-extra'
Plugin 'kmyk/sdl2.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-surround'
Plugin 'honza/vim-snippets'
Plugin 'sirver/ultisnips'
Plugin 'vim-syntastic/syntastic'
Plugin 'jacquesbh/vim-showmarks'
Plugin 'kien/ctrlp.vim'
Plugin 'terryma/vim-smooth-scroll'
Plugin 'adelarsq/vim-matchit'
Plugin 'valloric/youcompleteme'
Plugin 'vim-scripts/ZoomWin'
Plugin 'tomtom/tcomment_vim'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'powerline/powerline'
Plugin 'powerline/fonts' 
Plugin 'joshdick/onedark.vim'
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
syntax enable
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


" working together to make it better !

" To connect to the pad at URI http://localhost:9001/p/test per default:
let g:epad_host = "109.12.211.231" " Hostname to connect to
let g:epad_port = "8555"      " Port to connect to
let g:epad_path = "p/"        " URL Path to the pad
let g:epad_pad = "game"       " Name of the pad to connect to

" GUI feel
let g:epad_updatetime = 250  " lower this for more realtime, higher this for less load

" GUI look
let g:epad_attributes = 0     " set to 1 to display attributes (works only with a font that)
let g:epad_authors = 0        " set to 1 to display authors (works only in gui mode)

" Enable verbosity
let g:epad_verbose = 0        " set to 1 for INFO level, 2 for DEBUG level

" COLORS !!!


" Global Setting
" --------------------------------------------------------------------------------
let python_highlight_all = 1
set updatetime=250 
set number
set relativenumber
map <f7> :DoShowMarks!<cr>
nnoremap <F4> :make<cr>
let g:UltiSnipsExpandTrigger="²"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" allows you to deal with multiple unsaved
" buffers simultaneously without resorting
" to misusing tabs
set hidden

" just hit backspace without this one and
" see for yourself
set backspace=indent,eol,start

" Set to auto read when a file is changed from the outside
set autoread

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file

" Fast saving
nmap ,w  :w!<cr>

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch 

" Show matching brackets when text indicator is over them
set showmatch

" How many tenths of a second to blink when matching brackets
set mat=2


" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

set colorcolumn=110
highlight ColorColumn ctermbg=darkgray

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" MAPPINGS
" ----------------------------------------------------------------------------------------------
no <down> ddp
no <left> <Nop>
no <right> <Nop>
no <up> ddkP
ino <down> <Nop>
ino <left> <Nop>
ino <right> <Nop>
ino <up> <Nop>
vno <down> <Nop>
vno <left> <Nop>
vno <right> <Nop>
vno <up> <Nop>

noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>

nmap di, f,dT,
nmap ci, f,cT,

nmap G Gzz
nmap n nzz
nmap N Nzz
nmap } }zz
nmap { {zz

inoremap {      {}<Left>
inoremap {<CR>   <CR>{<CR>}<Esc>O
inoremap {{     {
inoremap {}     {}

inoremap        (  ()<Esc>i
inoremap <expr> )  strpart(getline('.'), col('.')-1, 1) == ")" ? "\<Esc>la" : ")"

inoremap        [  []<Esc>i
inoremap <expr> ]  strpart(getline('.'), col('.')-1, 1) == "]" ? "\<Esc>la" : "]"

inoremap <expr> ' strpart(getline('.'), col('.')-1, 1) == "\'" ? "\<Esc>la" : "\'\'\<Esc>i"
inoremap <expr> " strpart(getline('.'), col('.')-1, 1) == "\"" ? "\<Esc>la" : "\"\"\<Esc>i"

imap <leader>' '<ESC>i
imap <leader>" "<ESC>i
imap <leader>( (<ESC>i
imap <leader>[ [<ESC>i
imap <leader>{ {<ESC>i

" ----------------------------------------------------------------------------------------------
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
  set termguicolors
endif

autocmd vimenter * NERDTree

set background=dark
let g:onedark_termcolors=256
colorscheme onedark
" let g:molokai_original = 1
" let g:rehash256 = 1
" colorscheme molokai
let g:airline_theme='bubblegum'
let g:airline_powerline_fonts = 1
