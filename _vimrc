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

" taglist
Plugin 'vim-scripts/taglist.vim'

" cscope autoload and key map
Plugin 'mzlogin/cscope_macros.vim'

" markdown
Plugin 'tpope/vim-markdown'

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

" indention levels
"Plugin 'Yggdroot/indentLine'

" brackets auto pair
Plugin 'jiangmiao/auto-pairs'

" web indent
Plugin 'lukaszb/vim-web-indent'

" java decompile
Plugin 'kelwin/vim-smali'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
" filetype plugin on
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" leader
let mapleader="\\"

"tab=4
set shiftwidth=4
set sts=4
set tabstop=4

"tab->white space
set expandtab

"gui options
if has("gui_running")
    if has("win32")
        au GUIEnter * simalt ~x
    elseif has("gui_macvim")
        set guifont=Monaco:h13
"        set fullscreen
        set lines=60 columns=160
    else
        set guifont=Ubuntu\ Mono\ 12
    endif
    set cursorline      " highlight current line
    set guioptions-=m   "menu
    set guioptions-=T   "toolbar
endif

"line number
set nu

" search ignore case
set ignorecase

" encoding
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,cp936,big5,latin-1   "ucs-bom, compatible notepad UTF-8 with first three bytes is EFBBBF

" menu and bottom messy code
source $VIMRUNTIME/delmenu.vim   
source $VIMRUNTIME/menu.vim  
if has("win32")
    language messages zh_US.utf-8 
endif

"color scheme
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

" NERDTree settings
let NERDTreeWinPos=1

" move across lines
set backspace=indent,eol,start
set ww+=b,<,>

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

" folding
set foldmethod=syntax
set foldcolumn=1
set foldlevelstart=99

" no indent for case, default, public, private and protected
set cino+=:0,g0

" C++ auto-complete
nnoremap <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
nnoremap <C-F11> :!cscope -Rb <CR>
let OmniCpp_ShowPrototypeInAbbr = 1 
let OmniCpp_MayCompleteScope = 1 

" Java auto-complete
if has("autocmd")
    autocmd Filetype java setlocal omnifunc=javacomplete#Complete
    autocmd Filetype java setlocal completefunc=javacomplete#CompleteParamsInfo
endif

" autoclose complete window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif 
set completeopt=menuone,menu,longest

au Filetype html,php,javascript,xml setl shiftwidth=2
au Filetype html,php,javascript,xml setl tabstop=2

" taglist settings
let Tlist_Show_One_File = 1
let Tlist_Sort_Type = "name"

" LeaderF settings
nnoremap <silent> <leader>m :LeaderfMru<CR>

" auto-pairs seetings
let g:AutoPairsShortcutToggle = ''
let g:AutoPairsShortcutJump = ''

" smali language
let g:tlist_smali_settings = "smali;f:field;m:method"
