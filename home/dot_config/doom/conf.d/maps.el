;;; $DOOMDIR/conf.d/maps.el --- map 绑定相关配置 -*- lexical-binding: t; -*-


;; 定义编译期宏: 快捷配置指定寄存器
(eval-when-compile
  (defmacro my/without-yanking (command)
    `(lambda (&optional count)
       (interactive "P")
       (let ((evil-this-register ?_))
         (call-interactively ,command)))))

;; evil 相关快捷键配置
(after! evil
  (map! :nv "d" (my/without-yanking #'evil-delete)
        :nv "D" (my/without-yanking #'evil-delete-line)
        :nv "c" (my/without-yanking #'evil-change)
        :nv "C" (my/without-yanking #'evil-change-line)
        :nv "s" (my/without-yanking #'evil-substitute)
        :nv "S" (my/without-yanking #'evil-change-line)
        :nv "x" (my/without-yanking #'evil-delete-char)
        :nv [delete] (my/without-yanking #'evil-delete-char)
        :nvi  [C-backspace] (my/without-yanking #'backward-kill-word)
        :nvi  "C-k" (my/without-yanking #'kill-line)
        ;; 保留 X 为剪切键
        :nv "X" #'evil-delete
        ;; 配置编辑模式常用按键
        :i  "C-y" #'yank
        :i  "C-v" #'yank
        :i "C-z" #'undo)
  (setq! evil-kill-on-visual-paste nil)
  )

;; 修复 org 下 evil 的默认覆盖
(map! :after evil-org
      :map evil-org-mode-map
      :map evil-org-mode-map
      :nv "d" (my/without-yanking #'evil-org-delete)
      :nv "x" (my/without-yanking #'evil-org-delete-char)
      )

;; vterm 终端快捷键
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
        :i "C-S-c" #'my/vterm-copy)
  )


;; 增加在当前模式和 fundamental-mode 模式切换的快捷键
(defun my/toggle-fundamental-mode ()
  "在当前模式和 fundamental-mode 之间切换。"
  (interactive)
  (if (eq major-mode 'fundamental-mode)
      ;; 如果是 fundamental-mode，尝试恢复到之前的模式
      ;; 如果没有记录，默认回到 text-mode
      (funcall (or (get 'my/prior-mode 'state) #'text-mode))
    ;; 否则，记录当前模式并切到 fundamental-mode
    (put 'my/prior-mode 'state major-mode)
    (fundamental-mode)
    (message "Switched to fundamental-mode")
    )
  )

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Fundamental mode" "t" #'my/toggle-fundamental-mode)
      )
