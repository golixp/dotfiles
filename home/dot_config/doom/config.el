;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(after! nerd-icons
  (setq nerd-icons-font-family "Symbols Nerd Font"))

;; --- 基础配置 ---
;; 显示行号
(setq display-line-numbers-type t)

;; 主题配置, M-x `load-theme' 可以载入主题
(setq doom-theme 'doom-one)

;; 配置默认打开路径
(setq default-directory "~/project/")

;; org-mode 目录
(setq org-directory "~/project/notes/org/")

;; 强制使用英文时间戳
(setq system-time-locale "C")

;; 字体配置
(setq doom-font (font-spec :family "Sarasa Mono SC" :size 16 :weight 'Regular)
      ;; (setq doom-font (font-spec :family "Maple Mono Normal NF CN" :size 16 :weight 'Regular)
      doom-big-font (font-spec :family "Sarasa Mono SC" :size 24)
      ;; doom-big-font (font-spec :family "Maple Mono Normal NF CN" :size 24)
      doom-variable-pitch-font (font-spec :family "LXGW WenKai" :size 16)
      doom-serif-font (font-spec :family "Noto Serif CJK SC" :weight 'light)
      doom-symbol-font (font-spec :family "Noto Color Emoji"))


;; 插件配置 ---
;; 输入法自动切换
(after! fcitx
  (setq! fcitx-remote-command "fcitx5-remote"))

;; 状态栏显示项目和路径
(after! doom-modeline
  (setq! doom-modeline-buffer-file-name-style 'file-name-with-project)
  (setq! doom-modeline-persp-name t)
  (setq! doom-modeline-display-default-persp-name t)
  )

;; 项目列表忽略的目录
(after! projectile
  (add-to-list 'projectile-ignored-projects "~/.config/emacs/"))

;; 开启顶部 lsp 导航栏
(after! lsp-mode
  (setq! lsp-headerline-breadcrumb-enable t))

;; 快捷键提示栏打开速度(秒)
(after! which-key
  (setq! which-key-idle-delay 0.2))

;; evil 相关快捷键配置
(after! evil
  ;; 定义宏, 修改删除操作默认寄存器
  (defmacro my/without-yanking (command)
    `(lambda (&optional count)
       (interactive "P")
       (let ((evil-this-register ?_))
         (call-interactively ,command))))

  ;; 删除操作默认保存到黑洞寄存器
  (map! :nv "d" (my/without-yanking #'evil-delete)
        :nv "D" (my/without-yanking #'evil-delete-line)
        :nv "c" (my/without-yanking #'evil-change)
        :nv "C" (my/without-yanking #'evil-change-line)
        :nv "x" (my/without-yanking #'evil-delete-char))

  ;; 定义 X 为剪切
  (map! :nv "X" #'evil-delete)

  ;; 覆盖 evil 默认无用的 C-y 快捷键
  ;; 增加 C-v 粘贴
  (map! :i "C-y" #'yank
        :i "C-v" #'yank)

  ;; 覆盖无用的切换 Emacs 模式快捷键 C-z 为撤销
  (map! :nvi "C-z" #'undo)

  ;; Visual 模式粘贴不覆盖寄存器
  (setq evil-kill-on-visual-paste nil))

;; vterm 终端配置
(after! vterm
  (map! :map vterm-mode-map
        :i "C-y" #'vterm-yank
        :i "C-v" #'vterm-yank))

;; 配置 org-roam 使用和 logseq 相同的路径和配置
(after! org-roam
  ;; 核心路径设置
  (setq! org-roam-directory (file-truename "~/project/notes/")
         org-roam-dailies-directory "journals/")

  ;; 排除 Logseq 内部目录, 防止索引垃圾文件
  (setq! org-roam-file-exclude-regexp (rx (or ".git/" "logseq/" "org/")))

  ;; 配置普通文件模板
  (setq! org-roam-capture-templates
         '(("d" "default" plain "%?"
            :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
            :unnarrowed t)))

  ;; 配置日记文件模板
  (setq! org-roam-dailies-capture-templates
         '(("d" "default" entry "* %?"
            :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

  ;; 强制使用 ID 链接
  ;; (setq! org-id-link-to-org-use-id t)
  )

;; 禁止 Emacs 系统剪切板同步
;; (setq select-enable-clipboard nil)
;; (setq select-enable-primary nil)

;; (map! :leader
;;       "y y" #'clipboard-kill-ring-save   ; 复制到系统剪切板
;;       "y p" #'clipboard-yank)            ; 从系统剪切板粘贴


;; (map! :map vterm-mode-map
;;       "C-S-v" #'+vterm/paste
;;       "C-S-c" #'my/vterm-copy)

;; (after! vterm
;;   ;; 粘贴系统剪贴板到 vterm
;;   (defun my/vterm-paste ()
;;     (interactive)
;;     (when-let ((text (gui-backend-get-selection 'CLIPBOARD 'STRING)))
;;       (vterm-send-string text)))

;; ;; 复制 vterm 选区到系统剪贴板
;; (defun my/vterm-copy ()
;;   (interactive)
;;   (when (use-region-p)
;;     (gui-set-selection 'CLIPBOARD (buffer-substring (region-beginning) (region-end)))))

;;   ;; 快捷键绑定
;;   (map! :map vterm-mode-map
;;         "C-S-v" #'my/vterm-paste
;;         "C-S-c" #'my/vterm-copy)
;;   )
