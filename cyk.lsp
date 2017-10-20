(defun asin (gou xuan / gu)
;反正弦函数
(setq gu (sqrt (- (expt xuan 2)
                  (expt gou 2))))
(atan (/ gou gu)))

(defun c:cyk (/ ang-0 ang l1 l2 r1 r2 p0 p1 p2 p3 p4 p5 p6)
  ;画长圆孔
  (setvar "cmdecho" 0)
  (setq os (getvar "osmode"))
  (setvar "osmode" 0)
  (setq p0 (getpoint "拾取长圆孔圆心:\n"))
  (setq ang-0 (getangle p0 "长圆孔长轴方向:\n"));长轴基准角度
  (setq l1 (getreal "输入长轴距离:\n")
        l2 (getreal "输入短轴距离:\n"))
  (setq r1 (/ l1 2) ;长轴半径
        r2 (/ l2 2) ;短轴半径
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
                  
                        
        