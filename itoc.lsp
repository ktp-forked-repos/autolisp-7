(defun itoc (n / n-str str jump)
					;������nת���ɺ��֡���һ����...�š�����ʽ
  (setq	n-str (itoa n) ;������ת�����ַ���
	jump  1 ;ѭ������
	str   "" ;����ת������ַ����������óɿմ�
  )
  (repeat (strlen n-str) ;��������λ������ѭ������

    (setq
      str (strcat
	    str
	    (nth (atoi (substr n-str jump 1))
		 '("��" "һ" "��" "��" "��" "��" "��" "��" "��" "��")
	    )
	  )
    )
    (setq jump (1+ jump))
  )
  str
)
