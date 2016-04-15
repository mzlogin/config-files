" Vundle settings {{{
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/vimfiles/bundle/Vundle.vim
let path='~/vimfiles/bundle'
call vundle#begin(path)

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" file explorer
Plugin 'scrooloose/nerdtree'

" tagbar
Plugin 'majutsushi/tagbar'

" cscope autoload and key map
Plugin 'mzlogin/cscope_macros.vim'

" markdown
Plugin 'tpope/vim-markdown'
Plugin 'mzlogin/vim-markdown-toc'

" C++
Plugin 'vim-scripts/OmniCppComplete'
Plugin 'vim-scripts/a.vim'
Plugin 'mzlogin/code_complete'

" Java
Plugin 'vim-scripts/javacomplete'

" comment lines in a program
Plugin 'vim-scripts/EnhCommentify.vim'

" MiniBufExpl
Plugin 'fholgado/minibufexpl.vim'

" characters draw
Plugin 'vim-scripts/DrawIt'

" js formatter
Plugin 'vim-scripts/jsbeautify'

" fuzzy find files, buffers, mrus
Plugin 'Yggdroot/LeaderF'

" brackets auto pair
Plugin 'jiangmiao/auto-pairs'

" web indent
Plugin 'mzlogin/vim-js-indent'
Plugin 'lukaszb/vim-web-indent'

" Qml
Plugin 'peterhoeg/vim-qml'

" h5
Plugin 'mattn/emmet-vim'

" chinese copywriting
Plugin 'hotoo/pangu.vim'

" java decompile
Plugin 'mzlogin/vim-smali'

" python
Plugin 'hynek/vim-python-pep8-indent'

" table mode mostly for markdown
Plugin 'dhruvasagar/vim-table-mode'

" detect file encoding
Plugin 'mbbill/fencview'

" Node.js
Plugin 'myhere/vim-nodejs-complete'

if has("gui_macvim")
    Plugin 'Valloric/YouCompleteMe'
elseif has("win32")
    Plugin 'Valloric/YouCompleteMe', {'pinned': 1}
endif

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
" filetype plugin on
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
" }}}

" Basic settings {{{
" leader
let mapleader="\\"

" tab=4
set shiftwidth=4
set softtabstop=4
set tabstop=4

" tab->white space
set expandtab

" gui options
if has("gui_running")
    if has("win32")
        autocmd GUIEnter * simalt ~x
    elseif has("gui_macvim")
        set guifont=Monaco:h13
"        set fullscreen
        set lines=60 columns=160
    else
        set guifont=Ubuntu\ Mono\ 12
    endif
    set cursorline      " highlight current line
    set guioptions-=m   " menu
    set guioptions-=T   " toolbar
endif

" line number
set number

" search ignore case
set ignorecase

" encoding
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,cp936,big5,latin-1   " ucs-bom, compatible notepad UTF-8 with first three bytes is EFBBBF

" menu and bottom messy code
source $VIMRUNTIME/delmenu.vim   
source $VIMRUNTIME/menu.vim  
if has("win32")
    language messages zh_US.utf-8 
endif

" color scheme
colorscheme desert 

" Ctags
set tags=tags
set tags+=./tags
set tags+=~/tags-files/crttags
set tags+=~/tags-files/stltags
set tags+=~/tags-files/win32tags
set autochdir

" no back up
set nobackup
set nowritebackup

" highlight
syntax on

" search
set hlsearch
set incsearch

" move across lines
set backspace=indent,eol,start
set whichwrap+=b,<,>

" folding
set foldmethod=indent
set foldcolumn=1
set foldlevelstart=99

" no indent for case, default, public, private and protected
set cino+=:0,g0

" autoclose complete window
autocmd CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif 
set completeopt=menuone,menu,longest

" switch buffer
noremap <C-Tab> :bn<CR>
noremap <C-S-Tab> :bp<CR>
" }}}

" FileType specific settings {{{
autocmd FileType html,php,javascript,xml,css setlocal shiftwidth=2
autocmd FileType html,php,javascript,xml,css setlocal tabstop=2
autocmd FileType smali setlocal cindent
autocmd FileType vim setlocal foldmethod=marker
" }}}

" Common mappings {{{
" split window switch key map
noremap <C-l> <C-w>l
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-h> <C-w>h

if has("gui_macvim")
    " auto-complete
    inoremap <D-/> <C-x><C-o>
    " insert mode move settings
    inoremap ¬ <Right>
    inoremap ∆ <Down>
    inoremap ˚ <Up>
    inoremap ˙ <Left>
else
    " auto-complete
    inoremap <A-/> <C-x><C-o>
    " insert mode move settings
    inoremap <A-l> <Right>
    inoremap <A-j> <Down>
    inoremap <A-k> <Up>
    inoremap <A-h> <Left>
    " select auto complete item
    inoremap <A-n> <C-n>
    inoremap <A-p> <C-p>
endif

" }}}

" Plugin settings {{{
" nerdtree
let NERDTreeWinPos=1
let g:NERDTreeIgnore = ['\~$', '\.pyc']
nnoremap <leader>n :NERDTreeToggle<CR>

" OmniCppComplete
nnoremap <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
nnoremap <C-F11> :!cscope -Rb <CR>
let OmniCpp_ShowPrototypeInAbbr = 1 
let OmniCpp_MayCompleteScope = 1 

" javacomplete
autocmd FileType java setlocal omnifunc=javacomplete#Complete
autocmd FileType java setlocal completefunc=javacomplete#CompleteParamsInfo

" tagbar
nnoremap <leader>tb :TagbarToggle<CR>
let g:tagbar_width = 30
let g:tagbar_left = 1
let g:tagbar_type_smali = {
        \ 'ctagstype' : 'smali',
        \ 'kinds' : [
                \ 'f:field',
                \ 'm:method',
        \ ]
\ }
autocmd FileType markdown let g:tagbar_sort = 0
let g:tagbar_type_markdown = {
        \ 'ctagstype' : 'markdown',
        \ 'kinds' : [
                \ 'h:Heading_L1',
                \ 'i:Heading_L2',
                \ 'j:Heading_L3',
                \ 'k:Heading_L4',
                \ 'l:Heading_L5',
                \ 'm:Heading_L6',
        \ ]
\ }

" LeaderF
nnoremap <silent> <leader>m :LeaderfMru<CR>
let g:Lf_DefaultMode = 1
let g:Lf_IndexTimeLimit = 1200

" auto-pairs
let g:AutoPairsShortcutToggle = '<leader>p'
let g:AutoPairsShortcutJump = ''
let g:AutoPairsMapSpace = 0
let g:AutoPairsMultilineClose = 0
autocmd FileType c,cpp let b:AutoPairs = {'[':']', '{':'}',"'":"'",'"':'"', '`':'`'}
autocmd FileType smali let b:AutoPairs = {'(':')', '{':'}',"'":"'",'"':'"', '`':'`'}
if has("gui_macvim")
    let g:AutoPairsShortcutFastWrap = '<D-e>'
endif

" emmet-vim
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

" YouCompleteMe
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 0
nnoremap <leader>g :YcmCompleter GoTo<CR>

" minibufexpl.vim
nnoremap <leader>e :MBEToggle<CR>

" EnhCommentify.vim
if !exists('g:EnhCommentifyCallbackExists')
    function EnhCommentifyCallback(ft)
        if a:ft == 'smali'
            let b:ECcommentOpen = '#'
            let b:ECcommentClose = ''
        endif
    endfunction
    let g:EnhCommentifyCallbackExists = 'Yes'
endif

" vim-table-mode
let g:table_mode_corner = '|'
let g:table_mode_delimiter = ' '
autocmd FileType markdown TableModeEnable

" code_complete
let g:author_for_snippets = 'Zhuang Ma'
let g:email_for_snippets = 'chumpma(at)gmail.com'

" vim-markdown
let g:markdown_fenced_languages = ['html', 'python', 'java', 'cpp', 'c']

" for vim-js-indent debug
"let g:js_indent_logging = 1
" }}}
