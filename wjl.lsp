;;���ָ��ݼ������ȼ��㣬�����23��
;;�ݼ����Ӽ������ľ�l1=63.53mm l2=118.7mm
;;�����֧�ż��װ��е�Ϊԭ�㣬������¿�����Ϊ(120.8  118.1)
;;�������֧�ż��е����Ϊd


(defun wjl (d)
  ;;�������֧�ż����ľ�d�������ݼ������ȼ������ľ���
  (setq l1 63.53
	; l2 117
	dx 120.8
	; dy 247.51
	a  0.4014);23��Ļ���

  ;|(setq l (- (/ (/ (- d (* 2 dx)) 2)
		(cos a))
	     (/ (/ l1 2) (cos a))
	     )
	rzl-l (+ l 20 48)
	)|;
  (setq ll (/ (/ (- (- d (* 2 dx))
                 l1)
                2)
              (cos a))
        rzl-l (+ ll 20 50)
        )
              
  (princ (strcat "���ָ��ݼ������߳�" (rtos rzl-l 2) ", �н�60.5��,"))
  (princ (strcat "��Զ���׾�" (rtos ll 2)))
  (princ "\n���Ͽ�����20Ϊ�ߣ�����30Ϊһ����\73 ���¿�����40Ϊһ����")
  (princ)
  )

  
 

(wjl (getreal "������С�ݼ���ȣ�"))