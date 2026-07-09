# AGENTS.md — 给 AI 助手的仓库与系统说明

## 这个仓库是什么

chezmoi 源目录(dotfiles 仓库),管理用户 lixp 的多平台配置。**GitHub public 仓库,任何人可见。**

- **`.chezmoiroot` = `home`**:chezmoi 的源状态根是 `home/` 子目录。所有源状态特殊文件(`.chezmoiignore` 等)必须放在 `home/` 下才生效,放在仓库顶层会被 chezmoi 静默忽略(踩过坑:2026-07 发现顶层 `.chezmoiignore` 从未生效)。
- 仓库顶层(`README.md`、本文件、`docs/`)不会被 apply 到目标机器。
- **源文件名前缀编码目标文件的名字和权限**,可叠加:`dot_foo` → `.foo`;`private_foo` → 权限 0600(机密/含个人信息);`executable_foo` → 加可执行位(如 `dot_claude/executable_statusline.py` → `~/.claude/statusline.py`);`private_dot_ssh/` → `~/.ssh/`(0600)。
- 跨平台:Linux(主力)/ Windows / Android / macOS 共用此仓库,平台差异通过 `home/.chezmoiignore` 的 `{{ if ne .chezmoi.os "linux" }}` 之类模板块处理(某平台不需要的路径在该平台被忽略、不 apply)。改跨平台配置时同步维护此文件。
- **文件名约束**:因为要在 Windows 上 checkout,严禁把含 `\` `:` 等 Windows 非法字符的文件名提交入库(例如 systemd 转义单元名里的 `\x2d`——改用 dash 截断 drop-in 目录 `app-.service.d/` 这类无特殊字符的等效方案)。

## chezmoi 工作流

**最关键的事实:这是"源状态",不是活动配置。** 编辑本仓库的文件**不会**改变实际系统,直到执行 `chezmoi apply`。本仓库(`~/.local/share/chezmoi`)是 chezmoi 源目录,目标是用户 home 下的真实 dotfiles,两者通过 apply/add 同步:

```sh
chezmoi diff              # 预览源目录相对当前系统的改动(改配置后先看这个)
chezmoi apply             # 把源目录应用到系统(部署改动)
chezmoi apply --dry-run --verbose
chezmoi add ~/.zshrc      # 把系统里已有文件纳入/更新到源目录
chezmoi edit --apply ~/.zshrc   # 编辑源文件并立即 apply
chezmoi cd                # 进入本源目录
chezmoi update            # git pull 远程再 apply
```

## 本机私有覆盖模式(机密/机器特定配置的去向)

public 仓库不放任何机密或机器特定内容。这些放到**本机不入库**的 include 文件,由入库的主配置引用:

- `~/.gitconfig.local` ← `home/dot_gitconfig` 的 `[include] path`
- `~/.ssh/config.local` ← `private_dot_ssh/private_config` 的 `Include`
- `~/.config/zsh/local.zsh` ← `zsh/dot_zshrc` 里 source(且在 `.chezmoiignore` 中对所有平台排除)
- 网络栈等敏感细节:本机 Claude 记忆 `proxy-stack-details`

改配置需要引入机密/内网地址/网络软件细节时,走这个机制,不要写进仓库。

## 主力系统环境(Linux)

- Arch Linux + **niri**(Wayland 合成器)+ **DankMaterialShell / DMS**(quickshell 实现,基于 danklinux.com 安装),终端 Ghostty,输入法 fcitx5。
- 系统托盘(StatusNotifierWatcher)由 DMS 的 quickshell 提供,对应 systemd 用户单元 `dms.service`(Type=dbus)。
- XDG autostart 由 systemd 的 `xdg-autostart-generator` 处理,生成 `app-<name>@autostart.service`,与 `dms.service` 并行启动、无顺序保证。
- 本机有用户高度依赖的**常驻网络服务进程**;具体软件、端口等细节刻意不写入公开仓库(见"重要约束"),本机的 Claude 会话可从记忆 `proxy-stack-details` 获取。

## macOS 工作机

- **待执行改造**:AeroSpace 窗口管理(物理 Win 位 = Option 承担 niri 式窗口管理)。
  交接文档:`docs/macos-window-management.md`;配置草稿:`home/dot_config/aerospace/aerospace.toml`。
- 已定决策(2026-07):macOS 默认 Command/Ctrl 分层不动;不装 AltTab;不做 Command↔Ctrl 互换;
  Linux 侧改键方案(xremap/GTK key theme/Emacs Alt 层)已评估后**取消**,不要再提议。

## 管理的配置一览(`home/`)

- 桌面/输入法(Linux):`dot_config/` 下 `niri`(Wayland 合成器)、`DankMaterialShell`、`private_fcitx5`(fcitx5 + rime)、`systemd/user`、`fontconfig`、`ghostty`、`containers`
- Shell/编辑器:`zsh`(antidote 插件 + `conf.d/` 分模块 + p10k)、`doom`(Doom Emacs)、`helix`、`tmux`
- macOS:`dot_config/aerospace`(窗口管理草稿,见 `docs/macos-window-management.md`)
- AI 工具:`dot_claude`(Claude Code 全局配置 + statusline)、`dot_codex`(Codex CLI)

## 重要约束

- **本仓库是 public 仓库**:任何提交前必须自查改动内容(包括新增文档、代码注释和提交信息)——密钥/令牌、订阅或内网地址、公司与工作信息、能标识用户网络环境的软件细节等,一律不得入库。此类细节放本机不入库的位置(见"本机私有覆盖模式")。
- **不要中断正在运行的常驻网络服务**:对网络类常驻进程 kill/restart/改配置/占端口前必须先征得用户同意。
- 需要 sudo 的命令交给用户执行。
- **提交信息约定**:`<范围>: <中文描述>`,范围通常是被改配置的应用名(如 `niri:`、`doom:`、`claude:`、`rime:`)。

## 已修复问题存档(2026-07)

### Electron 自启应用开机后无托盘图标

原因:启动竞态。XDG autostart 的 Electron 应用比 dms.service 先启动,Electron/Chromium 创建托盘时 DBus 上还没有 org.kde.StatusNotifierWatcher,就永久放弃(不监听、不重试)。手动晚启动的 Electron 应用托盘正常,即为对照。

修复:`home/dot_config/systemd/user/app-.service.d/10-wait-tray.conf`(dash 截断 drop-in,对所有 `app-*.service` 生效)—— `After=dms.service` + ExecStartPre 轮询等待 watcher 注册,无 DMS 环境零延迟跳过。

对照知识:Tauri 应用无此问题,因为其托盘走 libayatana-appindicator,该库会监听 DBus NameOwnerChanged,watcher 后出现时会自动补注册。

### 调试技巧:Tauri/WebKitGTK 应用远程调试

Tauri/WebKitGTK 应用可用 `WEBKIT_INSPECTOR_HTTP_SERVER=127.0.0.1:9222` 启动,再通过 WebSocket(WebKit inspector 协议,命令需经 `Target.sendMessageToTarget` 包装)远程执行 JS / 抓 console。曾用于排查某 Tauri 应用"后端正常但 GUI 前端数据全空"的问题(具体软件与修复细节在本机 Claude 记忆 `proxy-stack-details`)。

## Agent skills

### Issue tracker

issue 以 markdown 文件形式存放在本仓库 `.scratch/<feature>/` 下(单人 dotfiles 仓,不走远程)。See `docs/agents/issue-tracker.md`.

### Triage labels

五个规范三态标签使用默认字符串(needs-triage / needs-info / ready-for-agent / ready-for-human / wontfix)。See `docs/agents/triage-labels.md`.

### Domain docs

单上下文:仓库根一个 CONTEXT.md + docs/adr/(均按需惰性创建)。See `docs/agents/domain.md`.
