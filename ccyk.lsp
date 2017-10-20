(defun c:ccyk (/ po dx dy dx/2 dy/2)
  ;¶à¶ÎÏß»­³¤Ô²¿×
(setq p0 (getpoint "ÊäÈëÔ²ÐÄµã")
      dx (getreal "ÊäÈëXÖá¾àÀë")
      dy (getreal "ÊäÈëYÖá¾àÀë"))
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