;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; (after! nerd-icons
;;   (setq nerd-icons-font-family "Symbols Nerd Font"))

;; --- 基础配置 ---
;; 显示行号
(setq display-line-numbers-type t)

;; 配置编辑时底部空行数量
(setq! scroll-margin 8
       scroll-conservatively 101
       scroll-preserve-screen-position t)

;; 主题配置, M-x `load-theme' 可以载入主题
(setq doom-theme 'doom-one)

;; 配置默认打开路径
(setq default-directory "~/project/")

;; org-mode 目录
(setq org-directory "~/project/notes/org/")

;; 强制使用英文时间戳
(setq system-time-locale "C")

;; 配置博客 org 笔记模板
(set-file-template! "/posts/.*\\.org$" :trigger "__hugo-post.org" :mode 'org-mode)

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

  ;; 复制 vterm 选区到系统剪贴板
  (defun my/vterm-copy ()
    (interactive)
    (when (use-region-p)
      (gui-set-selection 'CLIPBOARD (buffer-substring (region-beginning) (region-end)))))

  ;; 配置快捷键
  (map! :map vterm-mode-map
        :i "C-y" #'vterm-yank
        :i "C-S-v" #'vterm-yank
        :i "C-S-c" #'my/vterm-copy))

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
  )

;; 配置 org capture 模板
(after! org
  (setq! org-capture-templates
         '(("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox")
            "* [ ] %?\n%U" :prepend t)
           ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox")
            "* %U %?" :prepend t)
           ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
            "[%<%H:%M>] %?")
           ("p" "Templates for projects")
           ("pt" "Project-local todo" entry
            (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n%a\n%U"
            :prepend t)
           ("pn" "Project-local notes" entry
            (file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n%a\n%U"
            :prepend t)
           ("pc" "Project-local changelog" entry
            (file+headline +org-capture-project-changelog-file "Unreleased")
            "* %U %?\n%a" :prepend t)
           ("o" "Centralized templates for projects")
           ("ot" "Project todo" entry #'+org-capture-central-project-todo-file
            "* TODO %?\n%a\n%U" :heading "Tasks" :prepend nil)
           ("on" "Project notes" entry #'+org-capture-central-project-notes-file
            "* %U %?\n%a\n%U" :heading "Notes" :prepend t)
           ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file
            "* %U %?\n%a" :heading "Changelog" :prepend t))))

;; hugo 导出配置
(after! ox-hugo
  ;; hugo 根目录
  (setq! org-hugo-base-dir "~/project/blog")
  ;; 自动写入最后修改时间
  (setq! org-hugo-auto-set-lastmod t)
  )


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
  )

