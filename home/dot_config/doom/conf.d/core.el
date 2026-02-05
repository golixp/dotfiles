;;; $DOOMDIR/conf.d/core.el --- 核心配置 -*- lexical-binding: t; -*-

;; 主题配置, M-x `load-theme' 可以载入主题
(setq! doom-theme 'doom-moonlight)

;; 字体配置
(setq! doom-font (font-spec :family "Sarasa Mono SC" :size 16 :weight 'Regular)
       ;; (setq! doom-font (font-spec :family "Maple Mono Normal NF CN" :size 16 :weight 'Regular)
       doom-big-font (font-spec :family "Sarasa Mono SC" :size 24)
       ;; doom-big-font (font-spec :family "Maple Mono Normal NF CN" :size 24)
       doom-variable-pitch-font (font-spec :family "LXGW WenKai" :size 16)
       doom-serif-font (font-spec :family "Noto Serif CJK SC" :weight 'light)
       doom-symbol-font (font-spec :family "Noto Color Emoji")
       )

;; 默认打开路径
(setq! default-directory "~/project/")

;; 使用英文时间戳
(setq! system-time-locale "C")

;; 显示行号
(setq! display-line-numbers-type t)

;; 配置编辑时底部空行数量
(setq! scroll-margin 8
       scroll-conservatively 101
       scroll-preserve-screen-position t)

;; 拆分窗口相关配置
(setq window-combination-resize t  ; 让新窗口平分空间
      evil-vsplit-window-right t   ; 垂直拆分在右侧
      evil-split-window-below t)   ; 水平拆分在下方

;; --- 插件配置 ---
;; 状态栏显示项目和路径
(after! doom-modeline
  (setq! doom-modeline-buffer-file-name-style 'truncate-nil)
  (setq! doom-modeline-persp-name t)
  (setq! doom-modeline-display-default-persp-name t)
  )

;; 项目列表忽略的目录
(after! projectile
  (add-to-list 'projectile-ignored-projects "~/.config/emacs/"))

;; diff-hl Git 更改提示条使用实心宽条
(after! diff-hl
  (define-fringe-bitmap 'my-solid-block [255] nil nil '(center repeated))
  (setq! diff-hl-fringe-bmp-function (lambda (type pos) 'my-solid-block))
  )

;; 输入法自动切换
(after! fcitx
  (setq! fcitx-remote-command "fcitx5-remote"))

;; 开启顶部 lsp 导航栏
(after! lsp-mode
  (setq! lsp-headerline-breadcrumb-enable t))

;; 快捷键提示栏打开速度(秒)
(after! which-key
  (setq! which-key-idle-delay 0.2))
