(defun c:skck  (/ osmode ffd ff n point point-list det-xy)
					;���س��(skck) ������븨������
					;����ѡ���ĵ㣬�������س�׳���������
					;ֻ������һ���г����ȫ���������
					;���ɵĸ������벢���ܱ�֤������ȷ�������˹���������ʵ����
					;�汾V1.0 ʱ�䣺2015-10-23 ���ߣ�������
  (setq osmode (getvar "osmode"))
  (setvar "osmode" 4)			;�趨Բ�Ĳ���
  (setq ffd (getfiled "д���׳����ļ�" "" "txt" 1))
  (setq ff (open ffd "w"))		;�ļ�ָ��
  (write-line "M10" ff)			;�������뿪ͷ-��ʼ
  (write-line "G84 X0 Y0" ff)		;�������뿪ͷ-����
  (setq point-list nil)			;Բ������Ĵ洢�б�
  (setq	point	   (getpoint "������Ҫ��׵ĵ�һ��Բ��λ��")
	point-list (cons point nil)
	n	   2)
  (while (setq point
		(getpoint point
			  (strcat "\n�������"
				  (rtos n 2)
				  "��Ҫ��׵�Բ��λ�ã������밴ENT��:")))
    (setq point-list (cons point point-list)
	  n	     (1+ n)))
  (princ (strcat "һ��Ҫ��" (rtos (1- n) 2) "���ס�"))
  (setq	point-list (reverse point-list)
	n	   0)
  (repeat (1- (length point-list))
    (setq det-xy
	   (mapcar '- (nth (1+ n) point-list) (nth n point-list))
					;����ȡ�������㣬����������Լ���
	  )
    (write-line
      (strcat "G84 X"
	      (rtos (car det-xy) 2)
	      " Y"
	      (rtos (cadr det-xy) 2))
      ff)				;����д���ļ�
    (setq n (1+ n)))
  (write-line "M05 " ff)		;���������β-��ʼ
  (write-line "M11" ff)
  (write-line "G04 K4" ff)
  (write-line "G29" ff)
  (write-line "G04 K9" ff)
  (write-line "M30" ff) 		;���������β-����
  (close ff)
  (setvar "osmode" osmode)		;�ָ���ʼ��������
  )