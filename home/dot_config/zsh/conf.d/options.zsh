#!/usr/bin/env zsh

### 配置选项 ###

# 配置 less 分页参数
export LESS='-g -i -M -R -S -w'

# 配置 zsh 粘贴内容时不做高亮显示
zle_highlight+=(paste:none)
