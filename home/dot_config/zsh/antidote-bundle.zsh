#!/usr/bin/env zsh

set -euo pipefail

ZSH_DIR="$HOME/.config/zsh"

# 下载 Antidote
ANTIDOTE_DIR="$ZSH_DIR/antidote"
ANTIDOTE_REPO="https://github.com/mattmc3/antidote.git"

if [[ ! -d "$ANTIDOTE_DIR" ]]; then
  echo "Cloning Antidote..."
  git clone --depth=1 "$ANTIDOTE_REPO" "$ANTIDOTE_DIR"
fi

# 配置 Antidote
ANTIDOTE_HOME="$ZSH_DIR/plugins"
zstyle ':antidote:bundle' use-friendly-names 'yes'

# 生成插件加载文件
fpath=($ANTIDOTE_DIR/functions $fpath)
autoload -Uz antidote
antidote bundle < $ZSH_DIR/zsh_plugins.txt > $ZSH_DIR/zsh_plugins.zsh

# antidote 更新插件以及自身
(
  # 防止 __antidote_update_tmpdir 变量未定义报错
  set +u
  antidote update
)
