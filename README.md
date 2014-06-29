vimconf
=======

My gvim for win7 config via Vundle.

The Quick Start below is just for self use, I don't promise it will work well in your environment.

###Quick Start

* Set up Vundle:

```
git clone https://github.com/gmarik/Vundle.vim.git %userprofile%/vimfiles/bundle/Vundle.vim
```

* Put ctags.exe  
The plugin taglist required ctags.exe, so download it, and put it into YourVimInstallPath/vim74/ directory.

* Put cscope.exe (*optional*)  
If you want to view code, you may need cscope support, put it into YourVimInstallPath/vim74/ directory.(It may depend on regex2.dll and curses2.dll)

* Config CLisp (*optional*)  
If you use vim to write clisp, just install clisp and make your path to "start-swank.lisp" right.

* Get my _vimrc:

```
cd %userprofile%
git init
git remote add origin git@github.com:mzlogin/vimconf.git
git pull origin master
```

* Install plugins  
Start vim, and exec `:PluginInstall`

* Done!
