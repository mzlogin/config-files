let s:darwin = has('mac')
let s:windows = has('win32') || has('win64')
let s:gui = has('gui_running')

" vim-plug settings {{{
set rtp+=~/vimfiles
let path='~/vimfiles/plugged'
let g:plug_url_format = 'git@github.com:%s.git'

call plug#begin(path)

" file explorer
Plug 'scrooloose/nerdtree'

" tagbar
Plug 'majutsushi/tagbar'

" cscope autoload and key map
Plug 'mzlogin/cscope_macros.vim'

" markdown
Plug 'mzlogin/vim-markdown-toc'
Plug 'mzlogin/vim-kramdown-tab'
if s:windows
    Plug 'tpope/vim-markdown'
endif

Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }

" C++
Plug 'vim-scripts/OmniCppComplete'
Plug 'vim-scripts/a.vim'
Plug 'mbbill/code_complete'

" Java
Plug 'vim-scripts/javacomplete'

" comment lines in a program
Plug 'tomtom/tcomment_vim'

" MiniBufExpl
Plug 'fholgado/minibufexpl.vim'

" characters draw
Plug 'vim-scripts/DrawIt'

" js formatter
Plug 'pangloss/vim-javascript'

" fuzzy find files, buffers, mrus
if has('python') || has('python3')
    if s:windows
        Plug 'Yggdroot/LeaderF', { 'do': '.\install.bat' }
    else
        Plug 'Yggdroot/LeaderF', { 'do': '.\install.sh' }
    endif
endif

" brackets auto pair
Plug 'jiangmiao/auto-pairs'

" web indent
Plug 'jason0x43/vim-js-indent'
Plug 'lukaszb/vim-web-indent'

" Qml
" Plug 'peterhoeg/vim-qml'

" h5
Plug 'mattn/emmet-vim'

" chinese copywriting
Plug 'hotoo/pangu.vim'

" java decompile
Plug 'mzlogin/vim-smali'

" python
Plug 'hynek/vim-python-pep8-indent'

" table mode mostly for markdown
Plug 'dhruvasagar/vim-table-mode'

" detect file encoding
Plug 'mbbill/fencview'

" CoffeeScript
Plug 'kchmck/vim-coffee-script'

" php
Plug 'StanAngeloff/php.vim'

" vue
Plug 'posva/vim-vue'

" God's presence, but build it is to death.
" In Windows, config ycm manually and use :packadd to lazy load
"if has('python')
"    if s:gui && s:darwin
"        Plug 'Valloric/YouCompleteMe', {'pinned': 1}
"    endif
"endif

" devdocs
Plug 'rhysd/devdocs.vim'

" json pretty prints
Plug 'tpope/vim-jdaddy'

" compile and run
Plug 'xuhdev/SingleCompile'

" miniprogram
Plug 'chemzqm/wxapp.vim'

" search
Plug 'mileszs/ack.vim'

" asciidoc
Plug 'habamax/vim-asciidoctor'

" graphviz
"Plug 'wannesm/wmgraphviz.vim'

if s:darwin
    Plug 'ybian/smartim'
endif

if s:windows
    Plug 'lyokha/vim-xkbswitch'
endif

" paste image in markdown
Plug 'ferrine/md-img-paste.vim'

" base64 encode / decode
Plug 'equal-l2/vim-base64'

" vim terminal
Plug 'skywind3000/vim-terminal-help'

" All of your Plugins must be added before the following line
call plug#end()            " required
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
if s:gui
    if s:windows
        autocmd GUIEnter * simalt ~x
    elseif s:darwin
        set guifont=Monaco:h13
"        set fullscreen
        set lines=60 columns=160
"        set noimdisable
"        set iminsert=2
    else
        set guifont=Ubuntu\ Mono\ 12
    endif
    set imsearch=1
    set guioptions-=m   " menu
    set guioptions-=T   " toolbar
endif

" highlight current line
set cursorline

" ruler in status bar
set ruler

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
if s:windows
    language messages en_US.utf-8
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

" fix windows path separator
" set shellslash
set completeslash=slash

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

" jump to the last position when reopening a file
if has("autocmd")
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    " jump to top while git commit
    autocmd BufReadPost COMMIT_EDITMSG exe "normal! gg"
endif

" }}}

" FileType specific settings {{{
autocmd BufRead,BufNewFile *.{mmd} set filetype=mermaid
autocmd BufRead,BufNewFile *.{uixml} set filetype=html
autocmd BufRead,BufNewFile *.{gv} set filetype=dot
autocmd FileType html,javascript,css,less,vue,wxml,wxss setlocal shiftwidth=2 tabstop=2
autocmd FileType proto setlocal shiftwidth=2 tabstop=2
autocmd FileType smali setlocal cindent
autocmd FileType conf,markdown,proto,mermaid,dot setlocal smartindent
autocmd FileType vim setlocal foldmethod=marker
autocmd BufRead,BufNewFile *.{log} set filetype=log
autocmd FileType log set autoread
autocmd FileType log nnoremap <silent> <F5> :checktime<CR>
autocmd FocusGained,FocusLost,CursorHold,CursorHoldI,CursorMoved,CursorMovedI *.{log} checktime
" }}}

" Common mappings {{{
" switch buffer
nnoremap <C-Tab> :bn<CR>
nnoremap <C-S-Tab> :bp<CR>

" split window switch key map
nnoremap <C-l> <C-w>l
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h

" adjust split window size
nnoremap <leader>w= <C-w><C-=>
nnoremap <leader>wm <C-w>\|<C-w>_

if s:gui && s:darwin
    " auto-complete
    inoremap <D-/> <C-x><C-o>
    " insert mode move settings
    inoremap ¬ <Right>
    inoremap ∆ <Down>
    inoremap ˚ <Up>
    inoremap ˙ <Left>
    tnoremap œ <c-\><c-n>
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

" open/create file under cursor
nnoremap <leader>gf :e <cfile><cr>

" split fast
nnoremap <leader>\ :vs<cr>
nnoremap <leader>- :sp<cr>

" move on "screen lines"
nnoremap j gj
nnoremap k gk

" list search results in location window
nnoremap <leader>lv :lv /<c-r>=expand("<cword>")<cr>/ %<cr>:lw<cr>

" center the screen when jumping through the changelist
nnoremap g; g;zz
nnoremap g, g,zz

" scroll through the command history
cnoremap <c-j> <down>
cnoremap <c-k> <up>

" trailing whitespaces
nnoremap <leader><space> :%s/ \+$//e<CR>


" ref https://github.com/Valloric/dotfiles/blob/master/vim/vimrc.vim
" fast saving
nnoremap <leader>w :w!<cr>

" <leader>v brings up .vimrc
" <leader>V reloads it and makes all changes active (file has to be saved first)
noremap <leader>v :e! $MYVIMRC<CR>
noremap <silent> <leader>V :source $MYVIMRC<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

" }}}

" Plug settings {{{
" nerdtree
let NERDTreeWinPos=1
let g:NERDTreeIgnore = ['\~$', '\.pyc$', '\.class$']
nnoremap <leader>n :NERDTreeToggle<CR>

" OmniCppComplete
nnoremap <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extras=+q .<CR>
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
let g:tagbar_type_markdown = {
        \ 'ctagstype' : 'markdown',
        \ 'kinds' : [
                \ 'h:headings',
        \ ],
    \ 'sort' : 0
\ }

" LeaderF
nnoremap <silent> <leader>m :LeaderfMru<CR>
let g:Lf_DefaultMode = 'FullPath'
let g:Lf_IndexTimeLimit = 1200
let g:Lf_MruWildIgnore = {
            \ 'dir': ['.git'],
            \ 'file': []
            \ }
let g:Lf_ShowDevIcons = 0

" auto-pairs
let g:AutoPairsShortcutToggle = '<leader>p'
let g:AutoPairsShortcutJump = ''
let g:AutoPairsMapSpace = 0
let g:AutoPairsMultilineClose = 0
let g:AutoPairsMapBS = 0
autocmd FileType c,cpp let b:AutoPairs = {'[':']', '{':'}',"'":"'",'"':'"', '`':'`'}
autocmd FileType smali let b:AutoPairs = {'(':')', '{':'}',"'":"'",'"':'"', '`':'`'}
if s:gui && s:darwin
    let g:AutoPairsShortcutFastWrap = '<D-r>'
else
    let g:AutoPairsShortcutFastWrap = '<M-r>'
endif

" emmet-vim
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

" YouCompleteMe
"let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
"let g:ycm_confirm_extra_conf = 0
"nnoremap <leader>g :YcmCompleter GoTo<CR>
"nnoremap <leader>r :YcmCompleter GoToReferences<CR>

" minibufexpl.vim
nnoremap <leader>e :MBEToggle<CR>

" tcomment_vim
nmap <leader>c gcc
vmap <leader>c gc

" vim-table-mode
let g:table_mode_corner = '|'
let g:table_mode_delimiter = ' '
let g:table_mode_verbose = 0
let g:table_mode_auto_align = 0
autocmd FileType markdown TableModeEnable

" code_complete
let g:user_defined_snippets = '~/snippets.vim'
let g:author_for_snippets = 'Zhuang Ma'
let g:user_for_snippets = '马壮'
let g:email_for_snippets = 'chumpma(at)gmail.com'

" vim-markdown
let g:markdown_fenced_languages = ['html', 'python', 'java', 'cpp', 'c', 'xml', 'sql', 'json']

" devdocs.vim
augroup plugin-devdocs
  autocmd!
  autocmd FileType c,cpp,python,java,javascript nmap <buffer>K <Plug>(devdocs-under-cursor)
augroup END

" vim-jdaddy
nmap <leader>j gqaj

" for vim-js-indent debug
"let g:js_indent_logging = 1

" SingleCompile
nmap <F9> :SCCompile<cr>
nmap <F10> :SCCompileRun<cr>
autocmd FileType mermaid call SingleCompile#SetCompilerTemplate('mermaid', 'mmdc', 'mermaid.cli',
            \'mmdc', '-i $(FILE_NAME)$ -o $(FILE_TITLE)$.svg', '$(FILE_TITLE)$.svg')
autocmd FileType mermaid call SingleCompile#SetOutfile('mermaid', 'mmdc', '$(FILE_TITLE)$.svg')
autocmd FileType python call SingleCompile#ChooseCompiler('python', 'python3')
autocmd FileType java call SingleCompile#SetCompilerTemplate('java', 'sunjdk', 'Sun Java Development Kit',
            \'javac', '-encoding utf-8 $(FILE_NAME)$', 'java $(FILE_TITLE)$')
autocmd FileType cpp call SingleCompile#SetCompilerTemplate('cpp', 'g++', 'GNU C++ Compiler',
            \'g++', '-std=c++17 -g -o a.out $(FILE_NAME)$', 'a.out')

" markdown-preview
autocmd FileType markdown nnoremap <silent> <F5> :MarkdownPreview<CR>
autocmd FileType markdown nnoremap <silent> <F6> :MarkdownPreviewStop<CR>
let g:mkdp_markdown_css = expand('~/custom-markdown.css')

" ack.vim , required ripgrep
if executable('rg')
  let g:ackprg = 'rg --vimgrep'
endif
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ack!<Space>
let g:ack_autofold_results=1

" vim-asciidoctor
" Function to create buffer local mappings
fun! AsciidoctorMappings()
	nnoremap <buffer> <leader>oo :AsciidoctorOpenRAW<CR>
	nnoremap <buffer> <leader>op :AsciidoctorOpenPDF<CR>
	nnoremap <buffer> <leader>oh :AsciidoctorOpenHTML<CR>
	nnoremap <buffer> <leader>ox :AsciidoctorOpenDOCX<CR>
	nnoremap <buffer> <leader>ch :Asciidoctor2HTML<CR>
	nnoremap <buffer> <leader>cp :Asciidoctor2PDF<CR>
	nnoremap <buffer> <leader>cx :Asciidoctor2DOCX<CR>
endfun
" Call AsciidoctorMappings for all `*.adoc` and `*.asciidoc` files
augroup asciidoctor
	au!
	au BufEnter *.adoc,*.asciidoc call AsciidoctorMappings()
augroup END
autocmd FileType asciidoc nnoremap <silent> <F5> :Asciidoctor2HTML<CR>:AsciidoctorOpenHTML<CR>

" wmgraphviz.vim
"autocmd FileType dot nnoremap <silent> <F5> :GraphvizCompile<CR>:GraphvizShow<CR>

" md-img-paste.vim
autocmd FileType markdown nmap <buffer><silent> <leader>i :call mdip#MarkdownClipboardImage()<CR>
let g:mdip_imgdir = '.'

" vim-xkbswitch
if s:windows
    let g:XkbSwitchLib = expand('~/vim-ext-bin/libxkbswitch32.dll')
endif

" vim-base64
vnoremap <silent> <leader>ab :<c-u>call base64#encode_and_substitute()<cr>
vnoremap <silent> <leader>ba :<c-u>call base64#decode_and_substitute()<cr>

" vim-terminal-help
let g:terminal_key = '≠'
let g:terminal_close = 1

" }}}
