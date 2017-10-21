(defun c:sum-length (/ obj-s obj-name obj n sum-l)
  (prin1 "��ѡ���߶ε��ܳ�")
  (setq obj-s (ssget) ;ѡ��
        n 0  ;ѡ���ͼԪ����
        sum-l 0)  ;�ܳ�
  (repeat (sslength obj-s)
    (setq obj (ssname obj-s n)  ;ͼԪ��
          obj-name (cdr (assoc 0 (entget obj) ;ͼԪ����
                               )
                        )
          )
    (if (or (= obj-name "LINE")
            (= obj-name "CIRCLE")
            (= obj-name "ARC")
            (= obj-name "SPLINE")
            (= obj-name "POLYLINE")
            )
      (setq sum-l (+ sum-l (get-length obj)))) ;ͼԪ�ĳ��ȼ���
    (setq n (1+ n))
    )
 (prin1 (strcat "ѡ���"
	  (itoa n)
                "��������ܳ��ȣ�"
                (rtos sum-l)))
  (princ)
  )

(defun get-length (curve)
  ;��ֱ�ߡ�Բ����Բ������ߡ��Ż�����ߡ��������ߵȳ���
   (vl-load-com)
   (setq curve (vlax-ename->vla-object curve))
   (vlax-curve-getDistAtParam curve
     (vlax-curve-getEndParam curve)
   )
)