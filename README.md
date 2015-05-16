config-files
=======

Contains config files below.

1. vim config for Win7/Ubuntu via Vundle.

2. stl views support for gdb.

3. stl tags file generate from <http://www.vim.org/scripts/script.php?script_id=2358>.

4. emacs config for Win7.

The Quick Start below is just for self use, I don't promise it will work well in your environment.

###Quick Start

####Windows

**Vim**

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

**Emacs**

* get GNU Emacs in <http://www.gnu.org/software/emacs/>.

* add Emacs bin path to PATH environment varible, set ALTERNATE_EDITOR to runemacs.exe.

	Notice: If you want to use Vim plugins like taglist, add Emacs bin path after Vim bin path, or ctags.exe in Emacs may effect Vim plugins' performance.

* add "Edit with Emacs" to context menu with .reg file (replace emacsclientw.exe file path to yours):

	```
	Windows Registry Editor Version 5.00

	[HKEY_CLASSES_ROOT\*\shell\Edit with Emacs]

	[HKEY_CLASSES_ROOT\*\shell\Edit with Emacs\command]
	@="\"D:\\emacs\\bin\\emacsclientw.exe\" \"%1\""
	```

* download GNU Global from <http://www.gnu.org/software/global/>, and copy files to Emacs directory.

* download sbcl from <http://www.sbcl.org/> and install it to default path.

* copy c++ headers into ~/.emacs.d/cpp-headers

* use elpa in Emacs to install plugins below:
    
    * company

    * ggtags

	* helm-gtags

    * markdown-mode

	* projectile

	* sr-speedbar

	* smex

	* evil

	* jedi
		* pip install virtualenv
	    * M-x package-install <RETURN> jedi <RETURN>
		* M-x package-install <RETURN> exec-path-from-shell <RETURN>
		* restart emacs
		* M-x exec-path-from-shell-initialize
		* M-x jedi:install-server

	* evil-nerd-commenter

	* function-args

	* slime

	* slime-company

    For example to install ggtags:

    ```
    M-x package-install <RETURN>
    ggtags <RETURN>
    ```

####Ubuntu

* Set up Vundle:

    ```
    git clone https://github.com/gmarik/Vundle.vim.git ~/vimfiles/bundle/Vundle.vim
    ```

* install ctags

	The plugin taglist required ctags, so install it.

    ```
    sudo apt-get install exuberant-ctags
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
