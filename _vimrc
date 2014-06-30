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

" Python
Plugin 'vim-scripts/Pydiction'

" C++
Plugin 'vim-scripts/OmniCppComplete'

" comment lines in a program
Plugin 'vim-scripts/EnhCommentify.vim'

"MiniBufExpl
Plugin 'fholgado/minibufexpl.vim'

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
    au GUIEnter * simalt ~x
    set guioptions-=m   "menu
    set guioptions-=T   "toolbar
endif

"line number
set nu

" encoding
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,cp936,big5,latin-1   "ucs-bom, compatible notepad UTF-8 with first three bytes is EFBBBF

" menu and bottom messy code
source $VIMRUNTIME/delmenu.vim   
source $VIMRUNTIME/menu.vim  
language messages zh_US.utf-8 

"color scheme
colorscheme desert 

" Ctags
set tags=tags;
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

" folding
set foldmethod=syntax
set foldcolumn=1
set foldlevelstart=99

" no indent for case, default, public, private and protected
set cino+=:0,g0

" Python auto-complete
let g:pydiction_location = '~/vimfiles/bundle/Pydiction/complete-dict'
let g:pydiction_menu_height = 10

" C++ auto-complete
nnoremap <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
let OmniCpp_ShowPrototypeInAbbr = 1 
let OmniCpp_MayCompleteScope = 1 
" autoclose complete window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif 
set completeopt=menuone,menu,longest

" taglist settings
let Tlist_Show_One_File = 1
let Tlist_Sort_Type = "name"
