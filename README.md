# Chezmoi 管理的个人配置

Arch Linux 系列发行版安装:

```sh
sudo pacman -S chezmoi
```

## 初始化与同步

初始化空仓库

```sh
chezmoi init
```

拉取远程 dotfiles 并将其应用到当前系统中:

```sh
chezmoi init --apply https://github.com/golixp/dotfiles
```

拉取远程更新:

```sh
chezmoi update
```

## 管理文件

添加文件到 chezmoi 仓库

```sh
chezmoi add ~/.zshrc
```

编辑配置文件

```sh
chezmoi edit ~/.zshrc          # 编辑文件
chezmoi edit --apply ~/.zshrc  # 编辑完成后应用文件
chezmoi edit --watch ~/.zshrc  # 保存文件时立即应用
```

查看差异:

```sh
chezmoi diff
```

应用更改:

```sh
chezmoi apply
```

切换到仓库目录

```sh
chezmoi cd
```

## Fcitx5 配置

```sh
~/.config/fcitx5/profile               # 启用的输入法
~/.config/fcitx5/config                # 全局快捷键设置
~/.config/fcitx5/conf/classicui.conf   # 皮肤和字体
```