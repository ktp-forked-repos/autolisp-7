(defun c:copy-layout (/ layout-name n)
  ;����copy-layout�����ٸ���ͬһ���ֶ��
  (setvar "cmdecho" 0)
  (setq layout-name (getstring "����Ҫ���ƵĲ�������<a4y>:")
        n (getint "����Ҫ���ƵĲ�������:"))
  (if (= layout-name "")
    (setq layout-name "a4y"))
  (repeat n
    (command "layout" "c" layout-name ""))
  (prompt (strcat "������" layout-name "����" (rtos n) "�Σ�"))
  (prin1))
(prompt "����copy-layout�����ٸ���ͬһ���ֶ�Ρ�")
 (prin1)
        