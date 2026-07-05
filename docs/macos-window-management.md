# macOS 窗口管理改造(AeroSpace)— 交接文档

记录日期:2026-07-05
执行者:macOS 工作机上的 Claude Code session
状态:**待执行**(Linux 端已完成仓库内准备工作)

## 背景与已定决策

三端快捷键统一计划(原始资料在 Linux 机 `~/Documents/keyboard-unification-plan.md` /
`keyboard-unification-deep-research.md`,本文档自包含、无需读取它们)经 review 后,
用户于 2026-07-05 做出如下决策:

1. **Linux 侧所有改键方案全部取消**(xremap 应用白名单、GTK Emacs key theme、
   Ghostty/Emacs 的 Alt 系快捷键)。原因:兼容性问题多、维护复杂度超预期。
   不要再向用户提议 Linux 改键。
2. **Windows 不动**(游戏机)。
3. **macOS 只做一件事:用 AeroSpace 补窗口管理**。macOS 默认的 Command/Ctrl
   分层用户很满意,不改;`Ctrl+Space` 输入源切换不动。
4. **不装 AltTab.app**:用户不常用窗口级切换,Cmd+Tab / Cmd+` 保持系统默认。
5. **不做 Command↔Ctrl 互换**(System Settings → 键盘 → 修饰键):它能把 GUI
   快捷键搬回物理 Ctrl 位置对齐 Linux,但会把终端控制键和输入法切换一起搬到
   物理 Alt 位置,破坏终端肌肉记忆和 macOS 原生分层,得不偿失。已评估,除非
   用户日后主动要求,否则不做。

## 前提事实

- 外接键盘为美式布局,使用 **Mac 模式**:物理 Ctrl = Control,**物理 Win = Option**,
  物理 Alt = Command。
- Linux 上物理 Win = Super,被 niri 独占为窗口管理键,应用永远收不到。
  因此 mac 端把 Option(同一物理位置)整体交给 AeroSpace 是同构取舍。
- 目标是**物理手感一致**,不是修饰键名称一致。
- 用户环境:终端 Ghostty(配置已由 chezmoi 共享)、编辑器 helix / Doom Emacs /
  VS Code / Cursor、中文输入法(macOS 默认输入法,`Ctrl+Space` 切换)。
- 工作机可能受公司 MDM 管控,Accessibility 授权可能被策略限制。

## 仓库内已就位的内容(Linux 端已完成)

- `home/dot_config/aerospace/aerospace.toml`:AeroSpace 配置草稿,
  chezmoi apply 后落到 `~/.config/aerospace/aerospace.toml`(AeroSpace 支持
  XDG 路径,无需用 `~/.aerospace.toml`)。
- `home/.chezmoiignore`:
  - 非 darwin 平台忽略 `.config/aerospace/`;
  - 非 linux 平台忽略 fcitx5 / systemd / niri / DankMaterialShell。

## mac 端执行步骤

1. **首次 apply 前先看 diff**:`chezmoi diff`。检查是否还有 Linux 专属文件
   会泄漏到 mac(例如 `.config/containers/` 等本文档未覆盖的目录),发现就补进
   `home/.chezmoiignore` 的非 linux 块,不要直接 apply 了事。
2. **安装 AeroSpace**:`brew install --cask nikitabobko/tap/aerospace`
   (以官方 README 当前说明为准)。
3. **校对配置草稿**:`aerospace.toml` 未在真机验证过,逐条对照当前版本 guide:
   - 命令名(`focus` / `move` / `workspace` / `move-node-to-workspace` /
     `resize smart` / `layout` / `close` / `fullscreen` / `join-with` /
     `focus-monitor` / `move-node-to-monitor`);
   - 键名 `pageUp` / `pageDown` 的写法;
   - `move-node-to-workspace` 是否支持 `--focus-follows-window`
     (niri 移动窗口到工作区时焦点跟随,支持的话加上以对齐手感);
   - `workspace next/prev` 是否需要 `--wrap-around`。
   配置错误会导致 AeroSpace 启动/重载失败,修到 `aerospace reload-config` 干净通过。
4. **首启与授权**:启动 AeroSpace,授予 Accessibility 权限。若 MDM 策略禁止授权,
   整个方案止步,回报用户,不要找绕过手段。
5. **跑下方测试清单**,按需在 `on-window-detected` 里给弹窗类应用(系统设置等)
   加 floating 规则。
6. **改动回写 chezmoi**:真机上对 `~/.config/aerospace/aerospace.toml` 的所有修正
   用 `chezmoi add` 回写仓库并提交,保持源与目标一致。
7. 与用户确认下方两个**可选项**后再决定是否落地。

## 键位对照表(niri ↔ AeroSpace)

| 功能 | Linux(niri,Mod=物理 Win) | macOS(AeroSpace,alt=Option=物理 Win) |
|---|---|---|
| 焦点 左/下/上/右 | `Mod+H/J/K/L` | `alt-h/j/k/l` |
| 移动窗口 | `Mod+Shift+H/J/K/L` | `alt-shift-h/j/k/l` |
| 工作区 1..9 | `Mod+1..9` | `alt-1..9` |
| 移窗到工作区 | `Mod+Shift+1..9` | `alt-shift-1..9` |
| 相邻工作区 | `Mod+Page_Down/Page_Up`(及 `Mod+U/I`) | `alt-pageDown/pageUp` |
| 关闭窗口 | `Mod+Q` | `alt-q` |
| 全屏 | `Mod+Shift+F` | `alt-shift-f` |
| 浮动切换 | `Mod+Shift+T` | `alt-shift-t` |
| tabbed/accordion | `Mod+W` | `alt-w` |
| 窗口尺寸 | `Mod+Equal/Minus` | `alt-equal/alt-minus` |
| 开终端 | `Mod+T`(ghostty) | `alt-t`(open -a Ghostty) |
| 显示器焦点 | `Mod+Ctrl+H/L` | `ctrl-alt-h/l` |
| 移窗到显示器 | `Mod+Shift+Ctrl+H/L` | `ctrl-alt-shift-h/l` |
| 窗口/应用切换 | `Alt+Tab` / `Alt+grave` | `Cmd+Tab` / ``Cmd+` ``(系统默认,物理位置相同,不改) |

## 设计约束:哪些 alt-* 刻意没绑,为什么

AeroSpace 的绑定是全局抢占的,被绑走的 Option 组合键任何应用都收不到。
mac 端终端和 GUI Emacs 的 Meta 默认都落在 Option(= 物理 Win)上,所以绑定集合
是精心挑过的,**扩充绑定前必须过一遍这个约束**:

- **不绑 `alt-i` / `alt-o` / `alt-u` / `alt-b` / `alt-d` / `alt-n` / `alt-m` /
  `alt-comma` / `alt-period` / `alt-semicolon`**:留给终端里的 helix
  (`Alt-o/i` 语法树选择伸缩、`Alt-;`、`Alt-,`、`Alt-u/U` 历史)和 readline
  (`M-b`、`M-d`、`M-.`)。相邻工作区因此用 pageDown/pageUp 而不是 u/i。
- **`fullscreen` 放 `alt-shift-f` 而不是 `alt-f`**:保住 readline 的 `M-f`。
- **`alt-t` 会遮蔽 `M-t`(transpose-words)**:低频,接受;若用户在意可改绑。
- **不绑 `alt-space`**:留给系统/输入法。
- **不绑 `alt-tab`**:物理 Alt+Tab 在 Mac 模式下是 Cmd+Tab,保持系统默认(决策 4)。

## 可选项(需用户逐个确认,默认不做)

### A. Ghostty `macos-option-as-alt = true`

Ghostty 配置由 chezmoi 跨平台共享(`home/dot_config/ghostty/config`),
`macos-option-as-alt` 是 mac 专属键,加进共享配置对 Linux 无副作用。
开启后 mac 终端里 Option = Meta,readline `M-f/M-b/M-d` 和 helix 的 Alt 系列键
可用(仅限未被 AeroSpace 绑走的组合,见上节)。代价:Option 无法再输入特殊字符
(用户主要中英文输入,影响小)。**建议开,但先问。**

### B. mac 端 Emacs 把 Meta 放到 Command

GUI Emacs 在 mac 上默认 Option = Meta,会和 AeroSpace 抢键,且与 Linux
(物理 Alt = Meta)物理位置不一致。若设:

```elisp
;; GNU Emacs NS build 用 ns-command-modifier;emacs-mac port 用 mac-command-modifier
(setq ns-command-modifier 'meta)
```

则物理 Alt = Meta,与 Linux 完全同位,Option 完整让给 AeroSpace。
代价:Emacs 内失去 Cmd 系 GUI 快捷键(s-c/s-v 等;用户是 Doom/Evil 用户,
主力是 y/p + leader,影响小,而且顺便避免误触 Cmd+Q 退出)。
**需要读 Doom 配置(`home/dot_config/doom/`)确认无冲突后落地,并问过用户。**

## 测试清单

窗口管理(AeroSpace):

- `alt-h/j/k/l` 焦点移动;`alt-shift-h/j/k/l` 移动窗口。
- `alt-1..9` 切工作区;`alt-shift-1..9` 移窗到工作区(确认焦点是否跟随)。
- `alt-pageDown/pageUp` 相邻工作区。
- `alt-q` 关窗口(不是退出应用);`alt-shift-f` 全屏;`alt-shift-t` 浮动切换;
  `alt-w` 布局切换;`alt-equal/minus` 调尺寸;`alt-t` 开 Ghostty。
- 外接显示器接入时 `ctrl-alt-h/l` 跨屏。

原生行为不能退化:

- `Cmd+Tab`、``Cmd+` ``、`Cmd+C/V/X/A/S/T/W` 全部正常。
- `Ctrl+Space` 输入源切换正常;中文输入、候选不受影响。
- Ghostty 里 `Ctrl+C` 中断、`Ctrl+A/E/F/B/K` readline 行编辑正常。
- 若开了可选项 A:验证 `M-f/M-b/M-d` 与 helix 的 `Alt-o/i/;/,`。
- 若做了可选项 B:验证 Doom leader、Evil、minibuffer、`M-x`。
- 重启后 AeroSpace 自启(`start-at-login`)且 Accessibility 授权仍在
  (MDM 环境重点验证)。

## 回滚

`brew uninstall --cask aerospace`,配置文件由 chezmoi 管理可随时移除;
除 Accessibility 授权记录外无系统残留。Linux / Windows 完全不受影响。
