(defun asin (gou xuan / gu)
;�����Һ���
(setq gu (sqrt (- (expt xuan 2)
                  (expt gou 2))))
(atan (/ gou gu)))

(defun c:cyk (/ ang-0 ang l1 l2 r1 r2 p0 p1 p2 p3 p4 p5 p6)
  ;����Բ��
  (setvar "cmdecho" 0)
  (setq os (getvar "osmode"))
  (setvar "osmode" 0)
  (setq p0 (getpoint "ʰȡ��Բ��Բ��:\n"))
  (setq ang-0 (getangle p0 "��Բ�׳��᷽��:\n"));�����׼�Ƕ�
  (setq l1 (getreal "���볤�����:\n")
        l2 (getreal "����������:\n"))
  (setq r1 (/ l1 2) ;����뾶
        r2 (/ l2 2) ;����뾶
        ang (asin r2 r1)
        p1 (polar p0 (- ang-0 ang) r1)
        p2 (polar p0 ang-0 r1)
        p3 (polar p0 (+ ang-0 ang) r1)
        p4 (polar p0 (+ pi (- ang-0 ang))
                  r1)
        p5 (polar p0 (+ pi ang-0)
                  r1)
        p6 (polar p0 (+ pi (+ ang-0 ang))
                  r1))
  (command "pline" p1 "a" "s" p2 p3 "l" p4 "a" "s" p5 p6 "l" "cl")
  (setvar "osmode" os)
  (prin1))
                  
                        
        