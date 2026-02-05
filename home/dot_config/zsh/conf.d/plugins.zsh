#!/usr/bin/env zsh

### 插件加载 ###

# ez-compinit配置: 开启补全系统缓存
# 路径优先级 $ZSH_COMPDUMP > $XDG_CACHE_HOME/zsh/zcompdump > ~/.cache/zsh/zcompdump
zstyle ':plugin:ez-compinit' 'use-cache' 'yes'

# ez-compinit配置: 使用oh-my-zsh 风格的补全
# 官方提供 styles: gremlin, ohmy, prez, zshzoo
# 只有 ohmy 预设与 fzf-tab 兼容, 其它模式 group 标题会有无法渲染的占位符
zstyle ':plugin:ez-compinit' 'compstyle' 'ohmy'

# zephyr配置: 关闭 magic-enter 预设功能
zstyle ':zephyr:plugin:editor' 'magic-enter' no   # 回车自动执行 ls 或 git 命令

# 配合 fzf-tab 相关补全选项
zstyle ':completion:*:git-checkout:*' sort false   # git-checkout 关闭按字母排序
zstyle ':completion:*:descriptions' format '[%d]'  # 窗口增加 group 标题

# fzf-tab 内部配置
zstyle ':fzf-tab:*' fzf-flags --bind=tab:accept                               # 使用 Tab 键确认 fzf-tab 中的选项
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath' # 在 cd 界面使用 eza 显示目录内容

# 加载插件
source ${ZDOTDIR:-~}/zsh_plugins.zsh

# 配置 zsh-history-substring-search 快捷键
bindkey '^[[A'   history-substring-search-up    2>/dev/null # '^[[A' 序列的上方向键
bindkey '^[OA'   history-substring-search-up    2>/dev/null # '^[[A' 序列的上方向键

bindkey '^[[B'   history-substring-search-down  2>/dev/null # '^[[B' 序列的下方向键
bindkey '^[OB'   history-substring-search-down  2>/dev/null # '^[OB' 序列的下方向键

# 启动fzf-tab
enable-fzf-tab

# 初始化 zoxide
if (( $+commands[zoxide] )); then
  eval "$(zoxide init zsh)"
fi
