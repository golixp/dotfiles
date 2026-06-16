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

;; 输入法自动切换：mac 用 sis，其它系统用 fcitx
;; 注：不启用 sis-global-inline-mode，避免中文输入时空格触发临时英文模式
(if (featurep :system 'macos)
    (use-package! sis
      :hook ((doom-first-input . sis-global-cursor-color-mode)
             (doom-first-input . sis-global-respect-mode)
             (doom-first-input . sis-global-context-mode))
      :config
      (sis-ism-lazyman-config
       "com.apple.keylayout.ABC"                  ; 英文
       "com.apple.inputmethod.SCIM.Shuangpin"))   ; 系统拼音双拼
  (after! fcitx
    (setq! fcitx-remote-command "fcitx5-remote")))

;; 禁用 Doom :input chinese 模块塞进来的内置 pyim 输入法
;; 系统输入法由 sis/fcitx 接管，避免误按 C-\ 切到 pyim
(after! pyim
  (setq default-input-method nil))

;; 关闭 evil 操作（y/d/c/p 等）后的高亮反馈
(after! evil-goggles
  (evil-goggles-mode -1))

;; 开启顶部 lsp 导航栏
(after! lsp-mode
  (setq! lsp-headerline-breadcrumb-enable t))

;; 快捷键提示栏打开速度(秒)
(after! which-key
  (setq! which-key-idle-delay 0.2))

;; 关闭 vterm 下的 evil 模式
(after! vterm
  (set-evil-initial-state! 'vterm-mode 'emacs)
  (evil-make-intercept-map vterm-mode-map)
  )

