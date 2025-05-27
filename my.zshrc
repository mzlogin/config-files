# path
export PATH=/usr/local/bin:${PATH}
export PATH=.:$PATH
export PATH="/usr/local/sbin:$PATH"
export PATH="${HOME}/vim-ext-bin:$PATH"

export CLICOLOR=1

# completion case-insensitive
autoload -U compinit
compinit
# zstyle ':completion:*' completer _complete _prefix _correct _prefix _match _approximate
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'

# see https://gist.github.com/mislav/1712320
autoload colors; colors;
export LSCOLORS="Gxfxcxdxbxegedabagacad"
setopt prompt_subst
# prompt
ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}["
ZSH_THEME_GIT_PROMPT_SUFFIX="]%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
# show git branch/tag, or name-rev if on detached head
parse_git_branch() {
  (command git symbolic-ref -q HEAD || command git name-rev --name-only --no-undefined --always HEAD) 2>/dev/null
}
# show red star if there are uncommitted changes
parse_git_dirty() {
  if command git diff-index --quiet HEAD 2> /dev/null; then
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  else
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  fi
}
# if in a git repo, show dirty indicator + git branch
git_custom_status() {
  local git_where="$(parse_git_branch)"
  [ -n "$git_where" ] && echo "$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_PREFIX${git_where#(refs/heads/|tags/)}$ZSH_THEME_GIT_PROMPT_SUFFIX"
}
# show current rbenv version if different from rbenv global
rbenv_version_status() {
  local ver=$(rbenv version-name)
  [ "$(rbenv global)" != "$ver" ] && echo "[$ver]"
}
# put fancy stuff on the right
if which rbenv &> /dev/null; then
  RPS1='$(git_custom_status)%{$fg[red]%}$(rbenv_version_status)%{$reset_color%} $EPS1'
else
  RPS1='$(git_custom_status) $EPS1'
fi
# basic prompt on the left
PROMPT='%{$fg[cyan]%}%~% %(?.%{$fg[green]%}.%{$fg[red]%})%B$%b '

fpath=(~/.zsh $fpath)

get_current_branch() {
    # git symbolic-ref --short -q HEAD
    git branch --show-current
}

get_main_branch_name() {
    if git rev-parse --quiet --verify main > /dev/null
    then
        echo "main"
    else
        echo "master"
    fi
}

git_merge_to() {
    print merge $1 to $2
    git checkout $2
    git pull origin $(get_current_branch)
    vared -p 'Would you like to merge? (y/n) ' -c tmp
    if [[ "${tmp}" == "y" ]] then
        git merge $1
    fi
}

mvn_set_version() {
    mvn versions:set -DnewVersion=$1
}

mvn_package() {
    mvn clean package -DskipTests
}

mvn_deploy() {
    mvn clean deploy -DskipTests
}

zip_clean() {
    zip -d "$1" "__MACOSX*"
    zip -d "$1" "*.DS_Store"
}

7z_clean() {
    7zz a -tzip "$1.zip" "$1"
    zip_clean "$1.zip"
}

# alias
alias gs='git status'
alias gpull='git pull origin $(get_current_branch)'
alias gpush='git push origin $(get_current_branch)'
alias gmt='git_merge_to $(get_current_branch)'
alias gc='git checkout'
alias gcm='git checkout $(get_main_branch_name)'
alias gcd='git checkout develop'
alias gm='git merge'
alias gb='git branch'
alias gr='git remote -v'
# 清除本地已经合并的 git 分支
alias gclean='git branch --merged | ggrep -E -v "(^\*|master|main|dev|develop|support/fat)" | xargs git branch -d'

alias ll='ls -l'
alias tailf='tail -f'

alias mvnv='mvn_set_version'
alias mvnd='mvn_deploy'
alias mvnp='mvn_package'

alias zipclean='zip_clean'

alias rgc='rg 10.0.0-SNAPSHOT'

alias get_idf='. $HOME/github/esp-idf/export.sh'

export TK_SILENCE_DEPRECATION=1
export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles

# tmux
function tmux_attach() {
    if [[ -z "$TMUX" ]] ;then
        ID="`tmux ls | grep -vm1 attached | cut -d: -f1`" # get the id of a deattached session
        if [[ -z "$ID" ]] ;then # if not available create a new one
            tmux new-session
        else
            tmux attach-session -t "$ID" # if available attach to it
        fi
    fi
}
alias ta=tmux_attach

# proxy
function proxy_on() {
    export http_proxy="http://127.0.0.1:54107"
    export https_proxy=$http_proxy
    export no_proxy="localhost,127.0.0.1,0.0.0.0"
    echo -e "proxy on"
}
function proxy_off() {
    unset http_proxy
    unset https_proxy
    echo -e "proxy off"
}

test -e "${HOME}/.zsh/_iterm2/iterm2-shell-integration.zsh" && source "${HOME}/.zsh/_iterm2/iterm2-shell-integration.zsh"
test -e "${HOME}/.zsh/_git-flow/git-flow-completion.zsh" && source "${HOME}/.zsh/_git-flow/git-flow-completion.zsh"
test -e "${HOME}/.zsh/_git_merge_to/git_merge_to-completion.zsh" && source "${HOME}/.zsh/_git_merge_to/git_merge_to-completion.zsh"

# forbidden brew to auto update softwares
export HOMEBREW_NO_AUTO_UPDATE=1
export HOMEBREW_NO_INSTALL_CLEANUP=1
