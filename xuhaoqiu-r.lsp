(defun c:xuhaoqiu  ( / os dd num dd2 pt1 pt2 en1 en2 pp1 pp2 pp3 pp4
		    pp_0 pp_90 pp_270 pp_180
		    ang 
		    scale)
;自动连续或者重复编号
;可以自定义编号起始号码
;可输入放大比例
  (setvar "cmdecho" 0)
  (setq os (getvar "osmode"))
  (setvar "osmode" 0)
  (setq scale (getreal "\n输入放大比例<1>:"))
  (if (null scale) (setq scale 1.0))
  (setq dd (getdist "\n正方形边长 <5>: "))
  (if (null dd)
    (setq dd 5))
  (setq dd (* scale dd)) ;边长放大倍数
  (setq num (getint "\n起始编号 <1>: "))
  (if (null num)
    (setq num 1))
  (setq dd2 (/ dd 5))
  (setq pt1 (getpoint "\n第一点: "))
  (while (/= pt1 nil)
    (command "donut" 0 dd2 pt1 "")
    (setq pt2 (getpoint pt1 "\n第二点: "))
    (command "line" pt1 pt2 "")
    (command "polygon" 4 "e" pt2 (polar pt2 0 dd))
    (setq en1 (entlast))
    (command "text"
             "m"
             (polar (polar pt2 0 (/ dd 2)) (/ pi 2) (/ dd 2))
             0
             (itoa num))
    (setq en2 (entlast))
    (setq pp1 pt2)
    (setq pp2 (polar pp1 0 dd))
    (setq pp3 (polar pp2 (/ pi 2) dd))
    (setq pp4 (polar pp1 (/ pi 2) dd))
    (setq pp_0 (polar pp1 (/ pi 2) (/ dd 2)))
    (setq pp_90 (polar pp1 0 (/ dd 2)))
    (setq pp_180 (polar pp2 (/ pi 2) (/ dd 2)))
    (setq pp_270 (polar pp3 pi (/ dd 2)))
    (setq pp pt2)
    (setq ang (angle pt1 pt2))          ;---------------------------?判断选取点pt1与pt2的角度方向，决定矩形位置
    (cond ((and (> ang (* pi 0.5)) (< ang pi)) (setq pp pp2))
          ((and (> ang pi) (< ang (* pi 1.5))) (setq pp pp3))
          ((and (> ang (* pi 1.5)) (< ang (* pi 2))) (setq pp pp4)))
    (cond ((= ang 0) (setq pp pp_0))
          ((= ang (/ pi 2)) (setq pp pp_90))
          ((= ang pi) (setq pp pp_180))
          ((= ang (* pi 1.5)) (setq pp pp_270)))
    (command "move" en1 en2 "" pp pt2)
    (initget 128 "r")
    (setq pt1 (getpoint "\n下一点或者[r(重复序号)；按回车退出程序]:"))
    (cond ((= pt1 "r") (setq pt1 (getpoint "\n下一点: ")))
	  (t (setq num (1+ num))))
    )
  (setvar "osmode" os)
  (prin1))
