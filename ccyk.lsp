(defun c:ccyk (/ po dx dy dx/2 dy/2)
;;多段线画长圆孔
;;长圆孔-多段线（类似solidworks中的槽口）
(setq p0 (getpoint "输入圆心点")
      dx (getreal "输入X轴距离")
      dy (getreal "输入Y轴距离"))
  (setvar "osmode" 0)
(if (> dy dx)
  (progn (setq dy/2 (/ (- dy dx) 2)
	dx/2 (/ dx 2))
    (command "pline"
	     (mapcar '+ p0 (list dx/2 (- dy/2)))
	     (mapcar '+ p0 (list dx/2  dy/2))
	     "a"
	     (mapcar '+ p0 (list (- dx/2)  dy/2))
	     "l"
	     (mapcar '+ p0 (list (- dx/2) (- dy/2)))
	     "a"
	     "cl"))
  (progn (setq dx/2 (/ (- dx dy) 2)
	dy/2 (/ dy 2))
    (command "pline"
	     (mapcar '+ p0 (list (- dx/2) (- dy/2)))
	     (mapcar '+ p0 (list dx/2 (- dy/2)))
	     "a"
	     (mapcar '+ p0 (list dx/2  dy/2))
	     "l"
	     (mapcar '+ p0 (list (- dx/2)  dy/2))
	     "a"
	     "cl")
    
    ))
  (setvar "osmode" 167)
  )