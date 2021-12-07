# path
export PATH=/usr/local/bin:${PATH}
export PATH=.:$PATH
export PATH="/usr/local/sbin:$PATH"

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
    git symbolic-ref --short -q HEAD
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

# alias
alias gs='git status'
alias gpull='git pull origin $(get_current_branch)'
alias gpush='git push origin $(get_current_branch)'
alias gmt='git_merge_to $(get_current_branch)'
alias gc='git checkout'
alias gcm='git checkout master'
alias gcd='git checkout develop'
alias gm='git merge'

alias ll='ls -l'

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

test -e "${HOME}/.zsh/_iterm2/iterm2-shell-integration.zsh" && source "${HOME}/.zsh/_iterm2/iterm2-shell-integration.zsh"
test -e "${HOME}/.zsh/_git-flow/git-flow-completion.zsh" && source "${HOME}/.zsh/_git-flow/git-flow-completion.zsh"
