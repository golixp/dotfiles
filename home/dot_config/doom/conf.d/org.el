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
            "* TODO %U %?" :prepend t)
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

(use-package! org-transclusion
  :after org
  :config
  (map! :map org-mode-map
        :localleader
         (:prefix ("v" . "transclusion")
          :desc "Activate mode"          "t" #'org-transclusion-mode
          :desc "Add at point"           "a" #'org-transclusion-add
          :desc "Add all"                "A" #'org-transclusion-add-all
          :desc "Remove at point"        "r" #'org-transclusion-remove
          :desc "Remove all"             "R" #'org-transclusion-remove-all))
)

(defun my/org-dblock-write:range-dailies (start-date end-date &optional level)
  "从 START-DATE 到 END-DATE 抓取日记并插入 transclusion 标签。
日期格式为 (month day year) 列表。"
  (let* ((daily-path "journals/")
         (current-day-num (calendar-absolute-from-gregorian start-date))
         (end-day-num (calendar-absolute-from-gregorian end-date))
         (ins-level (or level 2)))

    (while (<= current-day-num end-day-num)
      (let* ((date (calendar-gregorian-from-absolute current-day-num))
             (year (nth 2 date))
             (month (nth 0 date))
             (day (nth 1 date))
             (date-str (format "%04d-%02d-%02d" year month day))
             (file-name (concat date-str ".org"))
             (abs-path (file-truename
                        (expand-file-name file-name
                                          (expand-file-name daily-path org-roam-directory)))))

        (when (file-exists-p abs-path)
          (insert (format "#+transclude: [[file:%s]] :level %d\n" abs-path ins-level)))
        (setq current-day-num (1+ current-day-num))))
    (org-align-all-tags)))

(defun org-dblock-write:month-dailies (params)
  "按月份生成日记列表"
  (let* ((year (plist-get params :year))
         (month (plist-get params :month))
         (start (list month 1 year))
         (end (list month (calendar-last-day-of-month month year) year)))
    (insert (format "#+TITLE: %d年%02d月 日记汇总\n\n" year month))
    (my/org-dblock-write:range-dailies start end (plist-get params :level))))

(defun org-dblock-write:range-dailies (params)
  "按自定义日期范围生成日记列表
   参数示例: :from \"2023-10-01\" :to \"2023-10-15\""
  (let* ((from (org-date-to-gregorian (plist-get params :from)))
         (to (org-date-to-gregorian (plist-get params :to))))
    (my/org-dblock-write:range-dailies from to (plist-get params :level))))
