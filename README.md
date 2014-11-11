config-files
=======

Contains config files below.

1. vim config for Win7/Ubuntu via Vundle.  
2. stl views support for gdb.  
3. stl tags file generate from <http://www.vim.org/scripts/script.php?script_id=2358>.

The Quick Start below is just for self use, I don't promise it will work well in your environment.

###Quick Start

####Windows
* Set up Vundle:

```
git clone https://github.com/gmarik/Vundle.vim.git %userprofile%/vimfiles/bundle/Vundle.vim
```

* place ctags.exe  
The plugin taglist required ctags.exe, so download it, and put it into YourVimInstallPath/vim74/ directory.

* place cscope.exe (*optional*)  
If you want to view code, you may need cscope support, put it into YourVimInstallPath/vim74/ directory.(It may depend on regex2.dll and curses2.dll)

* generate tags files (*optional*)  
For better c++ coding experience, generate your crttags and win32tags into ~/tags-files dir.

* pull config files:

```
cd %userprofile%
git init
git remote add origin git@github.com:mzlogin/config-files.git
git pull origin master
```

* Install plugins  
Start vim, and exec `:PluginInstall`

* Done!

####Ubuntu
* Set up Vundle:

```
git clone https://github.com/gmarik/Vundle.vim.git ~/vimfiles/bundle/Vundle.vim
```

* install ctags
The plugin taglist required ctags, so install it.

```
sudo apt-get install ctags-exuberant
```

* install cscope
If you want to view code, you may need cscope support, so install it.

```
sudo apt-get install cscope
```

* generate tags files (*optional*)  
For better c++ coding experience, generate your crttags and win32tags into ~/tags-files dir.

* pull config files:

```
cd ~
git init
git remote add origin git@github.com:mzlogin/config-files.git
git pull origin master
```

* Install plugins  
Start vim, and exec `:PluginInstall`

* Done!
