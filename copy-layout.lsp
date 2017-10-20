(defun c:copy-layout (/ layout-name n)
  ;命令copy-layout，快速复制同一布局多次
  (setvar "cmdecho" 0)
  (setq layout-name (getstring "输入要复制的布局名称<a4y>:")
        n (getint "输入要复制的布局数量:"))
  (if (= layout-name "")
    (setq layout-name "a4y"))
  (repeat n
    (command "layout" "c" layout-name ""))
  (prompt (strcat "复制了" layout-name "布局" (rtos n) "次！"))
  (prin1))
(prompt "命令copy-layout，快速复制同一布局多次。")
 (prin1)
        