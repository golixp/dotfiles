# AGENTS.md — 给 AI 助手的仓库与系统说明

## 这个仓库是什么

chezmoi 源目录(dotfiles 仓库),管理用户 lixp 的多平台配置。

- **`.chezmoiroot` = `home`**:chezmoi 的源状态根是 `home/` 子目录。所有源状态特殊文件(`.chezmoiignore` 等)必须放在 `home/` 下才生效,放在仓库顶层会被 chezmoi 静默忽略(踩过坑:2026-07 发现顶层 `.chezmoiignore` 从未生效)。
- 仓库顶层(`README.md`、本文件)不会被 apply 到目标机器。
- 跨平台:Linux(主力)/ Windows / Android 共用此仓库,平台差异通过 `home/.chezmoiignore` 的 `.chezmoi.os` 条件块处理。
- **文件名约束**:因为要在 Windows 上 checkout,严禁把含 `\` `:` 等 Windows 非法字符的文件名提交入库(例如 systemd 转义名 `app-mihomo\x2dparty@...`——改用 dash 截断 drop-in 目录 `app-.service.d/` 这类无特殊字符的等效方案)。

## 主力系统环境(Linux)

- Arch Linux + **niri**(Wayland 合成器)+ **DankMaterialShell / DMS**(quickshell 实现,基于 danklinux.com 安装),终端 Ghostty,输入法 fcitx5。
- 系统托盘(StatusNotifierWatcher)由 DMS 的 quickshell 提供,对应 systemd 用户单元 `dms.service`(Type=dbus)。
- XDG autostart 由 systemd 的 `xdg-autostart-generator` 处理,生成 `app-<name>@autostart.service`,与 `dms.service` 并行启动、无顺序保证。
- 代理客户端:**mihomo-party**(AUR 包 clash-party-bin,Electron)承担日常代理,监听 7897;clash-verge-rev(官方仓库,Tauri)也安装但系统代理未启用。

## macOS 工作机

- **待执行改造**:AeroSpace 窗口管理(物理 Win 位 = Option 承担 niri 式窗口管理)。
  交接文档:`docs/macos-window-management.md`;配置草稿:`home/dot_config/aerospace/aerospace.toml`。
- 已定决策(2026-07):macOS 默认 Command/Ctrl 分层不动;不装 AltTab;不做 Command↔Ctrl 互换;
  Linux 侧改键方案(xremap/GTK key theme/Emacs Alt 层)已评估后**取消**,不要再提议。

## 重要约束

- **不要中断正在运行的代理**(mihomo-party 及其内核):kill/restart/占端口前必须先征得用户同意。用户全部流量依赖它。
- 需要 sudo 的命令交给用户执行。

## 已修复问题存档(2026-07)

### mihomo-party 开机自启后无托盘图标

原因:启动竞态。autostart 的 mihomo-party 比 dms.service 先启动,Electron/Chromium 创建托盘时 DBus 上还没有 org.kde.StatusNotifierWatcher,就永久放弃(不监听、不重试)。手动晚启动的 Electron 应用(yesplaymusic)托盘正常,即为对照。

修复:`home/dot_config/systemd/user/app-.service.d/10-wait-tray.conf`(dash 截断 drop-in,对所有 `app-*.service` 生效)—— `After=dms.service` + ExecStartPre 轮询等待 watcher 注册,无 DMS 环境零延迟跳过。

对照知识:Tauri 应用(clash-verge-rev)无此问题,因为其托盘走 libayatana-appindicator,该库会监听 DBus NameOwnerChanged,watcher 后出现时会自动补注册。

### clash-verge-rev 2.5.x GUI 数据全空(订阅/代理页空白、流量为 0)

原因:2.5.x 新增 `enable_external_controller`,默认 false,内核只监听 Unix socket;但 GUI 前端(WebView 里的 JS)获取内核数据依赖 TCP external controller(127.0.0.1:9097),无法访问 Unix socket → 前端数据全部请求失败。Rust 后端自身走 Unix socket 一切正常,故订阅后台自动更新一直成功,仅界面显示为空。

修复:设置 `enable_external_controller: true`(verge.yaml,可在 GUI 设置中开)。

调试技巧存档:Tauri/WebKitGTK 应用可用 `WEBKIT_INSPECTOR_HTTP_SERVER=127.0.0.1:9222` 启动,再通过 WebSocket(WebKit inspector 协议,命令需经 `Target.sendMessageToTarget` 包装)远程执行 JS / 抓 console。
