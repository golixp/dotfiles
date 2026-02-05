#!/usr/bin/env zsh

### 别名相关配置 ###

# --- git aliases ---
# 基本命令
alias g='git'
alias ga='git add'
alias gaa='git add --all'
alias gst='git status'

alias gss='git stash'
alias gf='git fetch'
alias grs='git reset'

alias gb='git branch'
alias gm='git merge'
alias grb='git rebase'
alias gcp='git cherry-pick'

# clone 相关
alias gcl='git clone --recurse-submodules'
alias gcld1='git clone --depth=1 --recurse-submodules'

# checkout 相关
alias gco='git checkout'
alias gcb='git checkout -b'

# diff 相关
alias gd='git diff'
alias gds='git diff --staged'
alias gdw='git diff --word-diff'
alias gdsw='git diff --staged --word-diff'

# push/pull 相关
alias gp='git push'
alias gl='git pull'
alias glr='git pull --rebase'
alias glra='git pull --rebase --autostash'

# log 相关
alias glog='git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset" --date=iso'
alias gloga='git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset" --date=iso --all'

# commit 相关
alias gc='git commit --verbose'
alias gcm='git commit --message'
alias 'gc!'='git commit --verbose --amend'
alias 'gcn!'='git commit --verbose --no-edit --amend'

# --- eza aliases ---
if (( $+commands[eza] )); then
    typeset -ag eza_params
    eza_params=(
        '--git'
        '--smart-group'
        '--icons'
        '--group-directories-first'
        '--time-style=iso'
        '--color-scale=all'
    )

    alias ll='eza --all --header --long $eza_params'
    alias llm='eza --all --header --long --sort=modified $eza_params'
    alias lls='eza --all --header --long --total-size $eza_params'
    alias lt='eza --tree $eza_params'
    alias lg='eza --git-ignore $eza_params'
fi
