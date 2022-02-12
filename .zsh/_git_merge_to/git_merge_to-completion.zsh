#compdef git_merge_to
compdef _git_merge_to_comp git_merge_to

_git_merge_to_comp()
{
    local -a git_branches
    git_branches=("${(@f)$(git branch --format='%(refname:short)')}")
    _describe 'command' git_branches
}
