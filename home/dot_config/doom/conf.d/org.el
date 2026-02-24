;;; $DOOMDIR/conf.d/org.el --- org-mode 相关配置 -*- lexical-binding: t; -*-

;; org-mode 目录
(setq! org-directory "~/project/notes/org/")

;; 配置 org hugo blog 模板
(set-file-template! "/posts/.*\\.org$" :trigger "__hugo.org" :mode 'org-mode)

;; hugo 导出配置
(after! ox-hugo
  ;; hugo 根目录
  (setq! org-hugo-base-dir "~/project/blog")
  ;; 自动写入最后修改时间
  (setq! org-hugo-auto-set-lastmod t)
  ;; 禁止导出作者防止和 DoIt 主题冲突
  (setq! org-export-with-author nil)
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
         '(("d" "default" entry "%?"
            :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  )

(after! org

  (require 'ox-gfm nil t)

  ;; 配置 Capture 模板
  (setq! org-capture-templates
         '(
           ("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox")
            "* TODO %?\n%U" :prepend t)
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
            "* %U %?\n%a" :heading "Changelog" :prepend t)
           )
         )
  )
