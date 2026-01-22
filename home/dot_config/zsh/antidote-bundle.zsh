#!/usr/bin/env zsh

set -euo pipefail

ZSH_DIR="$HOME/.config/zsh"

# 下载或更新 Antidote
ANTIDOTE_DIR="$ZSH_DIR/antidote"
ANTIDOTE_REPO="https://github.com/mattmc3/antidote.git"

if [[ -d "$ANTIDOTE_DIR/.git" ]]; then
  echo "Updating Antidote..."
  git -C "$ANTIDOTE_DIR" pull --ff-only
elif [[ ! -d "$ANTIDOTE_DIR" ]]; then
  echo "Cloning Antidote..."
  git clone --depth=1 "$ANTIDOTE_REPO" "$ANTIDOTE_DIR"
else
  echo "Error: $ANTIDOTE_DIR exists but is not a git repository" >&2
  exit 1
fi

# 配置 Antidote
ANTIDOTE_HOME="$ZSH_DIR/plugins"
zstyle ':antidote:bundle' use-friendly-names 'yes'

# 生成插件加载文件
fpath=($ANTIDOTE_DIR/functions $fpath)
autoload -Uz antidote
antidote bundle < $ZSH_DIR/zsh_plugins.txt > $ZSH_DIR/zsh_plugins.zsh