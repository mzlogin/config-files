# config-files

Contains config files below.

1. Vim config for Mac OS X/Windows via vim-plug.

2. STL views support for gdb.

3. STL tags file generate from <http://www.vim.org/scripts/script.php?script_id=2358>.

4. Emacs config for Windows.

The Quick Start below is just for self use, I don't promise it will work well in your environment.

## Table of Contents

<!-- vim-markdown-toc GFM -->

* [Quick Start](#quick-start)
    * [Mac OS X](#mac-os-x)
        * [Vim](#vim)
        * [Emacs](#emacs)
    * [Windows](#windows)
        * [Vim](#vim-1)
        * [Emacs](#emacs-1)
* [Other common requirements](#other-common-requirements)
    * [asciidoc](#asciidoc)

<!-- vim-markdown-toc -->

## Quick Start

### Mac OS X

#### Vim

* Set up vim-plug:

    ```
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    ```

* install cscope, universal-ctags and ripgrep

    If you want to view code, you may need cscope and ctags support, so install it.

    ```
    brew install cscope
    brew install --HEAD universal-ctags/universal-ctags/universal-ctags
    brew install ripgrep
    brew install yarn
    ```

* generate tags files (*optional*)

    For better c++ coding experience, generate your crttags and win32tags into ~/tags-files dir.

* pull config files:

    ```
    cd ~
    git init
    git remote add origin https://github.com/mzlogin/config-files.git
    git pull origin master
    ```

* Install plugins

    Start vim, and exec `:PlugInstall`

* Done!

#### Emacs

* install the newest version of GNU Emacs, global and sbcl

    ```
    brew install emacs
    brew install global
    brew install sbcl
    ```
* create file /usr/bin/sbcl and change its content to

    ```
    #!/bin/sh
    exec /usr/local/bin/sbcl "$@"
    ```

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

    * smooth-scrolling

    For example to install ggtags:

    ```
    M-x package-install <RETURN>
    ggtags <RETURN>
    ```

### Windows

#### Vim

* Set up vim-plug:

    ```
    md ~\vimfiles\autoload
    $uri = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    (New-Object Net.WebClient).DownloadFile(
      $uri,
      $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath(
        "~\vimfiles\autoload\plug.vim"
      )
    )
    ```

* place ctags

    The plugin tagbar required universal-ctags, so download it, and put it into YourVimInstallPath/vim80/ directory.

    Download link: <https://github.com/universal-ctags/ctags>

* install ripgrep

    Download link: https://github.com/BurntSushi/ripgrep

* place cscope (*optional*)

    If you want to view code, you may need cscope support, put it into YourVimInstallPath/vim80/ directory.

    Download link: <https://sourceforge.net/projects/mslk/files/Cscope/>

* generate tags files (*optional*)

    For better c++ coding experience, generate your crttags and win32tags into ~/tags-files dir.

* pull config files:

    ```
    cd %userprofile%
    git init
    git remote add origin https://github.com/mzlogin/config-files.git
    git pull origin master
    ```

* install yarn

* Install plugins

    Start vim, and exec `:PlugInstall`

* place iconv.dll

    The plugin fencview, for file encoding detect and switch, required this dll, so download one and add its path to PATH environment variable.

    (You can download iconv.dll I am used [here](http://pan.baidu.com/s/1pJGOpCJ).)

* Done!

#### Emacs

* get GNU Emacs in <http://www.gnu.org/software/emacs/>.

* add "Edit with Emacs" to context menu with .reg file (replace exe file path to yours first):

    ```
    Windows Registry Editor Version 5.00

    [HKEY_CLASSES_ROOT\*\shell\Edit with Emacs]

    [HKEY_CLASSES_ROOT\*\shell\Edit with Emacs\command]
    @="\"D:\\emacs\\bin\\emacsclientw.exe\" -a \"D:\\emacs\\bin\\runemacs.exe\" \"%1\""
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

    * smooth-scrolling

    For example to install ggtags:

    ```
    M-x package-install <RETURN>
    ggtags <RETURN>
    ```

## Other common requirements

### asciidoc

```
gem install asciidoctor
gem install coderay
gem install asciidoctor-pdf --pre
```

Install Chrome Extension below, and enable file address access.

<https://asciidoctor.org/docs/editing-asciidoc-with-live-preview/>
