## directory
#
function Enter-Sources {
    cd d:\sources\
}

Set-Alias src Enter-Sources

function Open-Current-Directory {
    explorer .
}

Set-Alias e. Open-Current-Directory

function Enter-Nginx {
    cd D:\Programs\nginx-1.14.0
}

Set-Alias ng Enter-Nginx

## Git

function Git-Status {
    git status
}

Set-Alias gs Git-Status

function Git-Add-All {
    git add .
}

Set-Alias ga Git-Add-All

function Git-Gui {
    gitk
}

Set-Alias gg Git-Gui

function Git-Pull-Current-Branch {
    $currentBranch = git symbolic-ref --short -q HEAD
    git pull origin $currentBranch
}

Set-Alias gpull Git-Pull-Current-Branch

function Git-Push-Current-Branch {
    $currentBranch = git symbolic-ref --short -q HEAD
    git push origin $currentBranch
}

Set-Alias gpush Git-Push-Current-Branch

function Git-Commit-And-Push {
    git add .
    git commit -m $args[0]
    Git-Push-Current-Branch
}

Set-Alias g1 Git-Commit-And-Push

## objdump

function Obj-Dump {
    D:\Android\sdk\ndk-bundle\toolchains\x86_64-4.9\prebuilt\windows-x86_64\bin\x86_64-linux-android-objdump.exe $args
}

Set-Alias objdump Obj-Dump

## AutoJump

function Auto-Jump {
    $dir = mzj $args[0]
    cd $dir
}

Set-Alias j Auto-Jump

function Auto-Jump-Child {
    $dir = mzjc $args[0]
    cd $dir
}

Set-Alias jc Auto-Jump-Child

function Auto-Jump-Open {
    mzjo $args[0]
}

Set-Alias jo Auto-Jump-Open

function Auto-Jump-Show {
    mzj -s
}

Set-Alias js Auto-Jump-Show

Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs

Import-Module posh-git
$GitPromptSettings.DefaultPromptSuffix = '`n$ '
