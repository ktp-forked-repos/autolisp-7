(defun c:sum-length (/ obj-s obj-name obj n sum-l)
  (prin1 "求选定线段的总长")
  (setq obj-s (ssget) ;选择集
        n 0  ;选择的图元数量
        sum-l 0)  ;总长
  (repeat (sslength obj-s)
    (setq obj (ssname obj-s n)  ;图元名
          obj-name (cdr (assoc 0 (entget obj) ;图元类型
                               )
                        )
          )
    (if (or (= obj-name "LINE")
            (= obj-name "CIRCLE")
            (= obj-name "ARC")
            (= obj-name "SPLINE")
            (= obj-name "POLYLINE")
            )
      (setq sum-l (+ sum-l (get-length obj)))) ;图元的长度加总
    (setq n (1+ n))
    )
 (prin1 (strcat "选择的"
	  (itoa n)
                "个对象的总长度："
                (rtos sum-l)))
  (princ)
  )

(defun get-length (curve)
  ;求直线、圆弧、圆、多段线、优化多段线、样条曲线等长度
   (vl-load-com)
   (setq curve (vlax-ename->vla-object curve))
   (vlax-curve-getDistAtParam curve
     (vlax-curve-getEndParam curve)
   )
)