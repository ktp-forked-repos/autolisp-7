
(defun c:fugan	(/	       p0 p1	     dist-x
		 jiejian-no    dx dy	     fugan-min-dist
		 dx-46	       fugan-jiange-46
		 dx-41	       fugan-jiange-41
		 set-no)
					;画桁架腹杆函数，优先角度 56度 51度
  (setvar "osmode" 33)
  (setvar "cmdecho" 0)
  (setq	p0	   (getpoint "指定腹杆群左下角点")
  	p1	   (getpoint "\n指定腹杆群右下角点：")
	dy	   (getdist "输入腹杆竖直投影距离：")
      jiejian-no (getint "\n输入腹杆节间数量<4>："))
  (if (not jiejian-no) (setq jiejian-no 4))
  (setq dist-x (distance p0 p1)) ;dist-x 腹杆群水平投影总距离
  
  (setq fugan-min-dist (getreal "输入腹杆对尖最小距离<60>:"))
  (if (not fugan-min-dist) (setq fugan-min-dist 60))

  (setq dx-46 (/ dy (tan (radian 46.35))))

  (setq	fugan-jiange-46
	 (/ (- dist-x
	       (* 2 jiejian-no dx-46)
	       (* jiejian-no fugan-min-dist)
	       )
	    (1- (* 2 jiejian-no))
	    )
	)

  (princ
    (strcat "\n<1>46.35度角时，角铁间隔：" (rtos fugan-jiange-46 2 2) "\n")
    )

  (setq dx-41 (/ dy (tan (radian 41.13))))

  (setq	fugan-jiange-41
	 (/ (- dist-x
	       (* 2 jiejian-no dx-41)
	       (* jiejian-no fugan-min-dist)
	       )
	    (1- (* 2 jiejian-no))
	    )
	)

  (princ (strcat "<2>41.13度角时，角铁间隔：" (rtos fugan-jiange-41 2 2) "\n")
	 )

  (initget 1 "1 2 3")
  (setq
    set-no
     (getkword
       "请选择<1>：46.35度腹杆；<2>：41.13度腹杆；<3>：角度不受限制"
       )
    )
  (setvar "osmode" 0)
  (cond	((= "1" set-no)
	 (fugan-array
	   p0
	   dx-46
	   dy
	   (+ fugan-min-dist fugan-jiange-46)
	   fugan-jiange-46
	   jiejian-no
	   )
	 )
	((= "2" set-no)
	 (fugan-array
	   p0
	   dx-41
	   dy
	   (+ fugan-min-dist fugan-jiange-41)
	   fugan-jiange-41
	   jiejian-no
	   )
	 )
	((= "3" set-no)
	 (setq fugan-jiange (getreal "输入腹杆间隔距离<15>:"))
	 (if (not fugan-jiange) (setq fugan-jiange 15))
	  (setq     dx	    (/ (- dist-x
				  (* (1- (* 2 jiejian-no)) fugan-jiange)
				  (* jiejian-no fugan-min-dist)
				  )
			       (* 2 jiejian-no)
			       )
	       )
	 
	 (fugan-array
	   p0
	   dx
	   dy
	   (+ fugan-min-dist fugan-jiange)
	   fugan-jiange
	   jiejian-no
	   )
	 )
	)
  (setvar "osmode" 167)
  )







(defun tan  (n)
					;正切函数
  (/ (sin n) (cos n))
  )
(defun RADIAN  (n)
					;角度n转换成弧度
  (/ (* n pi) 180)
  )

					

(defun fugan-array  (p0 dx dy ds-top ds-down jiejian-no)
					;根据单腹杆的水平和垂直投影距离dx dy，上下间隔 ds-top ds-down 节间数量 jiejian-no
					;画出腹杆群
					; p0 左下角点 dx dy 水平和竖直投影距离
					; ds-top ds-down 腹杆间距 jiejian-no 腹杆三角形数量
  (command "line" p0 (strcat "@" (rtos dx 2) "," (rtos dy 2)) "")
  (command "array"
	   (entlast)
	   ""
	   "r"
	   "1"
	   (rtos jiejian-no 2)
	   (rtos (+ (* 2 dx) ds-top ds-down) 2)
	   )
  (command "line"
	   (mapcar '+ p0 (list (+ ds-top (* 2 dx)) 0))
	   (strcat "@-" (rtos dx 2) "," (rtos dy 2))
	   ""
	   )
  (command "array"
	   (entlast)
	   ""
	   "r"
	   "1"
	   (rtos jiejian-no 2)
	   (rtos (+ (* 2 dx) ds-top ds-down) 2)
	   )
  )



