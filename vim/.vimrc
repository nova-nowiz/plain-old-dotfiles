set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'



" Others
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'scrooloose/nerdtree'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'mklabs/split-term.vim'
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'tpope/vim-repeat'
Plugin 'ashisha/image.vim'
Plugin 'mtth/scratch.vim'
Plugin 'tpope/vim-unimpaired'
Plugin 'airblade/vim-gitgutter'
Plugin 'scrooloose/nerdcommenter'

Plugin 'easymotion/vim-easymotion'
Plugin 'haya14busa/incsearch.vim'
Plugin 'haya14busa/incsearch-fuzzy.vim'



" Window and tab management
Plugin 't9md/vim-choosewin'
Plugin 'weilbith/nerdtree_choosewin-plugin'


" Linting
Plugin 'w0rp/ale'

" Completion
Plugin 'Shougo/deoplete.nvim'
Plugin 'Shougo/echodoc.vim'

Plugin 'prabirshrestha/async.vim'
Plugin 'prabirshrestha/vim-lsp'
Plugin 'lighttiger2505/deoplete-vim-lsp'

Plugin 'Shougo/neco-syntax'
Plugin 'Shougo/neco-vim'
" Plugin 'carlitux/deoplete-ternjs'

" GDB console in neovim
Plugin 'sakhnik/nvim-gdb'



" For syntax highlighting management -----
Plugin 'vim-python/python-syntax'
Plugin 'justinmk/vim-syntax-extra'
Plugin 'jaxbot/semantic-highlight.vim'
Plugin 'kmyk/sdl2.vim'
Plugin 'xolox/vim-misc'
Plugin 'jdonaldson/vaxe'
Plugin 'rust-lang/rust.vim'
Plugin 'vim-pandoc/vim-pandoc-syntax'



" For tag management -----
Plugin 'majutsushi/tagbar'
Plugin 'ludovicchabant/vim-gutentags'



" For fold management -----
Plugin 'pseewald/vim-anyfold'
Plugin 'konfekt/fastfold'
Plugin 'arecarn/vim-fold-cycle'



" Text management -----
Plugin 'jiangmiao/auto-pairs'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-surround'
Plugin 'godlygeek/tabular'
Plugin 'svermeulen/vim-yoink'
Plugin 'svermeulen/vim-subversive'

Plugin 'michaeljsmith/vim-indent-object'
Plugin 'bkad/CamelCaseMotion'
Plugin 'vim-scripts/argtextobj.vim'



" Snippets -----
Plugin 'honza/vim-snippets'
Plugin 'sirver/ultisnips'
Plugin 'thomasfaingnaert/vim-lsp-ultisnips'

Plugin 'jacquesbh/vim-showmarks'
Plugin 'terryma/vim-smooth-scroll'
Plugin 'adelarsq/vim-matchit'
Plugin 'vim-scripts/ZoomWin'
Plugin 'tomtom/tcomment_vim'



" For aestetics -----
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'powerline/powerline'
Plugin 'powerline/fonts'
Plugin 'tiagofumo/vim-nerdtree-syntax-highlight'
Plugin 'thaerkh/vim-indentguides'

Plugin 'joshdick/onedark.vim'
Plugin 'lifepillar/vim-solarized8'
Plugin 'challenger-deep-theme/vim', {'name': 'challenger-deep-theme'}
Plugin 'Rigellute/rigel'

Plugin 'lervag/vimtex'
Plugin 'KeitaNakamura/tex-conceal.vim'

Plugin 'junegunn/limelight.vim'
Plugin 'junegunn/goyo.vim'

" Always load last
Plugin 'ryanoasis/vim-devicons'


" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
syntax enable


" Global Setting ----------------------------------------------------
set mouse=a
set foldlevel=0
set clipboard+=unnamedplus

" allows you to deal with multiple unsaved
" buffers simultaneously without resorting
" to misusing tabs
set hidden

" just hit backspace without this one and
" see for yourself
set backspace=indent,eol,start

" Set to auto read when a file is changed from the outside
set autoread



" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch



" Show matching brackets when text indicator is over them
set showmatch

" How many tenths of a second to blink when matching brackets
set mat=2



" Indentation
set ai "Auto indent
set si "Smart indent

" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4



" Linebreak on 500 characters
set lbr
set tw=500

set colorcolumn=110

set wrap "Wrap lines
set noshowmode " Disable mode in command line



set updatetime=250



" For the numbers column
set number
set relativenumber


" Split new buffer to the right
set splitright

" Spellchecker config
set spelllang=en_us,fr

" Makes vim take the relative location not from the open buffer, but from the current working directory
set cpo=d

" Hides unnecessary things (used for Latex)
set conceallevel=2

" Leader key assignement
nnoremap <SPACE> <Nop>
let mapleader=" "
let maplocalleader=" "




" PLUGIN CONFIG -----------------------------------------------------

" Nvim specific plugin setup
if (has("nvim"))


    " Ale Config
    let g:ale_set_balloons=1
    let g:ale_cpp_gcc_options = '-std=c++17 -Wall -I/usr/include/entt/ -I/usr/include/rapidjson/'
    let g:ale_cpp_clang_options = '-std=c++17 -Wall -I/usr/include/entt/ -I/usr/include/rapidjson/'
    let g:ale_fixers = {
    \   'cpp': [
    \       'clang-format',
    \       'uncrustify',
    \   ],
    \   'js': [
    \       'eslint',
    \       'prettier',
    \   ],
    \   'html': [
    \       'tidy',
    \       'prettier',
    \   ],
    \}
    let g:ale_cpp_cppcheck_options = '--enable=style -i build -i tests -I/usr/include/entt/ -I/usr/include/rapidjson/'
    let g:ale_cpp_cpplint_options = '--extensions=hpp,cpp,hxx,cxx,cc,h,cu,cuh'



    " Deoplete config
    let g:deoplete#enable_at_startup = 1
    let g:deoplete#max_abbr_width = 0
    let g:deoplete#max_menu_width = 0
    call deoplete#custom#option('sources', {
    \ '_': ['ultisnips', 'lsp', 'file', 'syntax'],
    \ 'js': ['ternjs'],
    \ 'vim': ['vim'],
    \})
    call deoplete#custom#option('keyword_patterns', {
	\ '_': '[a-zA-Z_]\k*',
    \})



    " Ternjs config
    " let g:deoplete#sources#ternjs#types = 1
    " let g:deoplete#sources#ternjs#depths = 1
    " let g:deoplete#sources#ternjs#docs = 1
    " let g:deoplete#sources#ternjs#filter = 1
    " let g:deoplete#sources#ternjs#case_insensitive = 1
    " let g:deoplete#sources#ternjs#guess = 1
    " let g:deoplete#sources#ternjs#sort = 1
    " let g:deoplete#sources#ternjs#expand_word_forward = 1
    " let g:deoplete#sources#ternjs#omit_object_prototype = 0
    " let g:deoplete#sources#ternjs#include_keywords = 0
    " let g:deoplete#sources#ternjs#in_literal = 1

    " Add extra filetypes
    " let g:deoplete#sources#ternjs#filetypes = [
    " \ 'jsx',
    " \ 'javascript.jsx',
    " \ 'vue',
    " \ '...'
    " \ ]



    " Other Options

    let g:split_term_vertical=0
    let g:disable_key_mappings=0

    let g:lsp_diagnostics_enabled = 0

    let g:echodoc#enable_at_startup = 1
endif



" Lsp config
if executable('clangd')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'clangd',
        \ 'cmd': {server_info->['clangd', '-background-index']},
        \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp'],
        \ })
endif

if executable('metals-vim')
   au User lsp_setup call lsp#register_server({
      \ 'name': 'metals',
      \ 'cmd': {server_info->['metals-vim']},
      \ 'initialization_options': { 'rootPatterns': 'build.sbt'},
      \ 'whitelist': [ 'scala', 'sbt' ],
      \ })
endif

if executable('rls')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rustup', 'run', 'stable', 'rls']},
        \ 'workspace_config': {'rust': {'clippy_preference': 'on'}},
        \ 'whitelist': ['rust'],
        \ })
endif

" if executable('tsserver')
"     au User lsp_setup call lsp#register_server({
"       \ 'name': 'javascript support using tsserver',
"       \ 'cmd': { server_info->[&shell, &shellcmdflag, 'tsserver --stdio']},
"       \ 'root_uri': { server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_directory(lsp#utils#get_buffer_path(), '.git/..'))},
"       \ 'whitelist': ['javascript', 'javascript.jsx', 'javascriptreact']
"       \ })
" endif



" Ctrlp Options
let g:ctrlp_extensions = ['mixed', 'tag', 'line', 'quickfix', 'dir', 'changes', 'bookmarkdir']
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_map = '<a-p>'
let g:ctrlp_cmd = 'CtrlPMixed'



" Ultisnips Options
let g:UltiSnipsExpandTrigger="<c-f>"
let g:UltiSnipsJumpForwardTrigger="<c-f>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"



" Gutentags Options
let g:gutentags_ctags_extra_args = ['--fields=+l', '--c-kinds=+p', '--c++-kinds=+p']

if executable('rg')
  let g:gutentags_file_list_command = 'rg --files'
endif



" Vimtex Options
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
let g:tex_conceal='abdmg'



" NERDTree Options
let NERDTreeShowBookmarks=1
let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1



" Fuzzy Incsearch
function! s:config_fuzzyall(...) abort
  return extend(copy({
  \   'converters': [
  \     incsearch#config#fuzzy#converter(),
  \     incsearch#config#fuzzyspell#converter()
  \   ],
  \ }), get(a:, 1, {}))
endfunction

" Incsearch x Easymotion
function! s:config_easyfuzzymotion(...) abort
  return extend(copy({
  \   'converters': [incsearch#config#fuzzyword#converter()],
  \   'modules': [incsearch#config#easymotion#module({'overwin': 1})],
  \   'keymap': {"\<CR>": '<Over>(easymotion)'},
  \   'is_expr': 0,
  \   'is_stay': 1
  \ }), get(a:, 1, {}))
endfunction



" Other Options
let python_highlight_all = 1

let g:AutoPairsShortcutToggle = '<c-p>'

let g:yoinkIncludeDeleteOperations=1

let anyfold_fold_comments=1

let g:camelcasemotion_key = '<leader>'

let g:fold_cycle_default_mapping = 0 "disable default mappings

let g:choosewin_overlay_enable = 0

let g:scratch_insert_autohide = 0
let g:scratch_persistence_file = "~/scratch"
let g:scratch_no_mappings = 1

let g:EasyMotion_startofline = 0 " keep cursor column when JK motion





" MAPPINGS ------------------------------------------------------------

" Fast saving
nmap ,w  :w!<cr>

" Way more convenient [ and ] mappings
nmap < [
nmap > ]
omap < [
omap > ]
xmap < [
xmap > ]
nmap << [[
nmap >> ]]
nmap <> []
nmap >< ][

" Disabling the arrow keys
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

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Choosewin keybindings
nmap  -  <Plug>(choosewin)

" Terminal Stuff
tnoremap <Esc> <C-\><C-n>
tnoremap <C-h> <C-\><C-N><C-w>h
tnoremap <C-l> <C-\><C-N><C-w>l
tnoremap <C-j> <C-\><C-N><C-w>j
tnoremap <C-k> <C-\><C-N><C-w>k

" Smooth scroll for large scroll motions
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>

" Reposition screen when doing large movements
nmap n nzz
nmap N Nzz
nmap } }zz
nmap { {zz
nmap ) )zz
nmap ( (zz

" Visual move up with indentation
xnoremap <silent> K :call mappings#visual#move_up()<CR>
xnoremap <silent> J :call mappings#visual#move_down()<CR>

" deoplete tab-complete
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
inoremap <expr><s-tab> pumvisible() ? "\<c-p>" : "\<tab>"

" transparency
nnoremap <C-t> :call mappings#transparency#Toggle_transparent()<CR>

" CtrlP Keybinds
nmap <a-t> :CtrlPTag<CR>
" nmap <a-l> :CtrlPLine<CR>
nmap <a-b> :CtrlPBuffer<CR>
nmap <a-q> :CtrlPQuickfix<CR>

" Ale movement keybinds
nmap <silent> <a-k> <Plug>(ale_previous_wrap)
nmap <silent> <a-j> <Plug>(ale_next_wrap)

nmap <a-d> :ALEGoToDefinitionInVSplit<CR>
" GoToImplementation is Ctrl+]

" Yoinks keybinds
nmap <a-h> <plug>(YoinkPostPasteSwapBack)
nmap <a-l> <plug>(YoinkPostPasteSwapForward)

nmap p <plug>(YoinkPaste_p)
nmap P <plug>(YoinkPaste_P)

nmap [y <plug>(YoinkRotateBack)
nmap ]y <plug>(YoinkRotateForward)

nmap <c-=> <plug>(YoinkPostPasteToggleFormat)



" Subversive keybinds
" s for substitute
nmap s <plug>(SubversiveSubstitute)
nmap ss <plug>(SubversiveSubstituteLine)
nmap S <plug>(SubversiveSubstituteToEndOfLine)

nmap <leader>s <plug>(SubversiveSubstituteRange)
xmap <leader>s <plug>(SubversiveSubstituteRange)

nmap <leader>ss <plug>(SubversiveSubstituteWordRange)

nmap <leader>cr <plug>(SubversiveSubstituteRangeConfirm)
xmap <leader>cr <plug>(SubversiveSubstituteRangeConfirm)
nmap <leader>crr <plug>(SubversiveSubstituteWordRangeConfirm)

" Yoink Integration
xmap s <plug>(SubversiveSubstitute)
xmap p <plug>(SubversiveSubstitute)
xmap P <plug>(SubversiveSubstitute)



" Very useful function keybindings
map <F1> :LspHover<CR>
map <F2> :NERDTreeToggle<CR>
map <F3> :TagbarToggle<CR>
map <F4> :DoShowMarks!<cr>
map <F5> :make<CR>
map <F6> :setlocal spell!<CR>

" Using Buffers
nnoremap gb :ls<CR>:b<Space>

" Fold cylcle keybindings
nmap <leader><Tab> <Plug>(fold-cycle-open)
nmap <leader><S-Tab> <Plug>(fold-cycle-close)

nmap <leader>gs :Scratch<CR>
nmap <leader>gS :Scratch!<CR>
xmap <leader>gs <plug>(scratch-selection-reuse)
xmap <leader>gS <plug>(scratch-selection-clear)

noremap <silent><expr> z/ incsearch#go(<SID>config_fuzzyall())
noremap <silent><expr> z? incsearch#go(<SID>config_fuzzyall({'command': '?'}))
noremap <silent><expr> zg? incsearch#go(<SID>config_fuzzyall({'is_stay': 1}))
noremap <silent><expr> <Space>/ incsearch#go(<SID>config_easyfuzzymotion())

" Easymotion keybinds
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)



" AUTOCOMMANDS ----------------------------------------------------------

" Startup script
autocmd vimenter * NERDTree | execute "normal \<C-W>l" | AnyFoldActivate

" Terminal autoinsert (I think)
autocmd BufWinEnter,WinEnter term://* startinsert


" Open Quickfix window automatically after running :make
augroup OpenQuickfixWindowAfterMake
    autocmd QuickFixCmdPost [^l]* nested cwindow
    autocmd QuickFixCmdPost    l* nested lwindow
augroup END


" Activate anyfold by default
augroup anyfold
    autocmd!
    autocmd Filetype <filetype> AnyFoldActivate
augroup END

" Disable anyfold for large files
let g:LargeFile = 1000000 " File is large if size greater than 1MB
autocmd BufReadPre,BufRead * let f=getfsize(expand("<afile>")) | if f > g:LargeFile || f == -2 | call LargeFile() | endif
function LargeFile()
    augroup anyfold
        autocmd! " Remove AnyFoldActivate
        autocmd Filetype <filetype> setlocal foldmethod=indent " Fall back to indent folding
    augroup END
endfunction

" Auto enable limelight when entering goyo mode
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!



" COLORSCHEME ----------------------------------------------------------

" True color support
if (has("nvim"))
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
if (has("termguicolors"))
  set termguicolors
endif

" Dark color scheme
set background=dark

" In case the color scheme doesn't have a ColorColumn
highlight ColorColumn ctermbg=darkgray

" autocmd ColorScheme * hi Normal ctermbg=none guibg=none
colorscheme challenger_deep
" let g:onedark_termcolors=256
" colorscheme onedark

" Transparency keybinding variables
let g:is_transparent=0
hi link backup backup
execute "hi backup guibg =" synIDattr(hlID("Normal"), "bg")

" Airline theme
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
