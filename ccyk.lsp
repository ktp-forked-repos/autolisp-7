(defun c:ccyk (/ po dx dy dx/2 dy/2)
;;����߻���Բ��
;;��Բ��-����ߣ�����solidworks�еĲۿڣ�
(setq p0 (getpoint "����Բ�ĵ�")
      dx (getreal "����X�����")
      dy (getreal "����Y�����"))
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