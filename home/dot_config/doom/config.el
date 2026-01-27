;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; 项目列表忽略的目录
(after! projectile
  (add-to-list 'projectile-ignored-projects "~/.config/emacs/"))

;; 配置默认打开路径
(setq default-directory "~/project/")

;; 输入法自动切换
(after! fcitx
  (setq fcitx-remote-command "fcitx5-remote"))

;; 状态栏显示项目和路径
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'file-name-with-project))

;; 开启顶部 lsp 导航栏
(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t))

;; 快捷键提示栏打开速度(秒)
(after! which-key
  (setq which-key-idle-delay 0.2))

(after! evil
  ;; 定义宏，修改删除操作默认寄存器
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

(after! vterm
  (map! :map vterm-mode-map
        :i "C-y" #'vterm-yank
        :i "C-v" #'vterm-yank))

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


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept.

;; 字体配置
(after! mixed-pitch
  (set-face-attribute 'variable-pitch nil :weight 'regular))
(setq doom-font (font-spec :family "Maple Mono Normal NF CN" :size 16 :weight 'Regular)
      doom-variable-pitch-font (font-spec :family "LXGW WenKai" :size 16 :weight 'Regular))
;; doom-variable-pitch-font (font-spec :family "更纱黑体 SC" :size 16 :weight 'Regular))
;; doom-variable-pitch-font (font-spec :family "等距更纱黑体 SC" :size 16 :weight 'Regular))
;; 非等宽字体配置特定文档格式使用
(use-package! mixed-pitch :hook
  (org-mode . mixed-pitch-mode)
  (markdown-mode . mixed-pitch-mode))

;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; (setq doom-theme 'catppuccin)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
