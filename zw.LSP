(defun xy-move (X0Y0 x-det y-det) (mapcar '+ x0y0 (list x-det y-det)))

(defun itoc (n / n-str str jump)	;将整数n转换成汉字“〇一二三...九”的形式
  (setq	n-str (itoa n)			;将数字转换成字符串
	jump  1				;循环增量
	str   ""			;保存转换后的字符串，先设置成空串
  )
  (repeat (strlen n-str)		;根据数字位数设置循环次数
    (setq
      str (strcat
	    str
	    (nth (atoi (substr n-str jump 1))
		 '("零" "一" "二" "三" "四" "五" "六" "七" "八" "九")
	    )
	  )
    )
    (setq jump (1+ jump))
  )
  str
)

(defun numer (p r n c)
  ;;p圆心 r半径 n编号 c是转换开关0数字样式，1或其他汉字样式
  (command "circle" p r)
  (if (= c 0)
    (command "text" "m" p "" (itoa n))	;数字样式
    (command "text" "m" p "" (itoc n))	;汉字样式
  )
)

(defun dim-style-set (scal)
  ;;设置标注样式
  ;;scal 打印比例
  (command "style" "dim-scal-text" "gbcbig.shx" 0 "" "" "" "" "")
					;设置字体样式
  (setvar "dimtxsty" "dim-scal-text")	;设置标注字体               
  (setvar "dimscale" scal)		;设置放大比例
  (command "dimstyle"
	   "s"
	   (strcat "s-"
		   (rtos scal 2 1)
		   "-t"
		   (substr (rtos (getvar "cdate") 2 6) 10 4)
	   )
  )					;根据当前时间设置标注样式名称
)



(defun c:zw (/		   shang-kai	 shang-kai-dist
	     shang-kai-num zw-l1	 zw-l2	       x0y0
	     text-size	   you-jin	 dim-length    num
	     osmode	   dim-dist-scale
	    )				;绘制正交轴网
					;输入上开规格表(轴距 开间数 轴距 开间数 ...)，如：(4000 10 ...)
					;输入右开规格表(轴距 开间数 轴距 开间数 ...)，如：(10800 10 ...)
					;输入轴线至尺寸线距离
					;版本 时间 作者
  (while
    (progn
      (setq shang-kai  (read (strcat "(" (getstring t "\n输入上开规格：") ")")
		       )
	    you-jin    (read (strcat "(" (getstring t "\n输入右进规格：") ")"))
	    dim-length (read (getstring "\n输入轴网延出线距离<250>:"))
	    x0y0       (getpoint "\n输入轴网左下角点:")
	    bianhaoqiu-zhijing  (getint "输入编号球直径<1500>:")
      )
      (if (not dim-length)
	(setq dim-length 250)
      )
      (if (not bianhaoqiu-zhijing)
	(setq bianhaoqiu-zhijing 1500)
      )
      (setq text-size (- (/ bianhaoqiu-zhijing 2) 50)) ;编号球内字体高度
      (setq dim-dist-scale 4)		;轴网尺寸线距离比轴网延出线距离的倍数
      (princ "上开规格表：")
      (princ shang-kai)
      (princ ";右进规格表：")
      (princ you-jin)
      (princ (strcat ";轴网延出线长度"
		     (rtos dim-length 2)
		     "; 编号球字体高度"
		     (rtos text-size 2)
	     )
      )
      (princ ";轴网左下角点：")
      (princ x0y0)
      (initget "r")
      (=
	(getkword
	  "\n查看参数是否正确，如果执行画图，直接回车；如果重新输入参数，输入r："
	)
	"r"
      )
    )
  )
  (setq osmode (getvar "osmode"))
  (setvar "osmode" 0)



  
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;求上开规格系列表
  (setq	shang-kai-dist nil
	shang-kai-num nil
  )
  (repeat (/ (length shang-kai) 2)
    (setq shang-kai-dist (cons (car shang-kai) shang-kai-dist)
	  shang-kai-num	 (cons (car (cdr shang-kai)) shang-kai-num)
	  shang-kai	 (cddr shang-kai)
    )
  )
  (setq	shang-kai-dist (reverse shang-kai-dist)
	shang-kai-num  (reverse shang-kai-num)
  )
  (setq	zw-y (+	(* 2 dim-length)
		(apply '+ (mapcar '* shang-kai-dist shang-kai-num))
	     )				;轴网Y轴线长度
  )
;;;;;;;;;;;;;;;;;;;;;;求右进系列规格表
  (setq	you-jin-dist nil
	you-jin-num nil
  )
  (repeat (/ (length you-jin) 2)
    (setq you-jin-dist (cons (car you-jin) you-jin-dist)
	  you-jin-num  (cons (car (cdr you-jin)) you-jin-num)
	  you-jin      (cddr you-jin)
    )
  )
  (setq	you-jin-dist (reverse you-jin-dist)
	you-jin-num  (reverse you-jin-num)
  )
  (setq	zw-x (+	(* 2 dim-length)
		(apply '+ (mapcar '* you-jin-dist you-jin-num))
	     )
  )					;轴网X轴线长度
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;设置标注比例和编号球字体大小
  (dim-style-set (/ (car shang-kai-dist) 40)) ;设置标注样式比例
  (command "style"
	   "number"
	   "gbcbig"
	   (rtos text-size 2 0)
	   ""
	   ""
	   ""
	   ""
	   ""
  )					;设置编号球字体
;;;;;;;;;;;;;;;;;;;;;;;;;画X轴线
  (setq zw-x0 (xy-move x0y0 (- dim-length) 0)) ;X轴线左下角点
  (setq zw-x-list (cons (list zw-x0 (xy-move zw-x0 zw-x 0)) nil))
					;X轴线点对表
  (while (car shang-kai-num)
    (repeat (car shang-kai-num)
      (setq zw-x-list
	     (cons
	       (list
		 (xy-move (caar zw-x-list) 0 (car shang-kai-dist))
		 (xy-move (caar zw-x-list)
			  zw-x
			  (car shang-kai-dist)
		 )
	       )
	       zw-x-list
	     )
      )
    )
    (setq shang-kai-num	 (cdr shang-kai-num)
	  shang-kai-dist (cdr shang-kai-dist)
    )
  )
  (setq zw-x-list (reverse zw-x-list))
  (command "layer" "M" "zhouwang" "C" "red" "" "Lw" 0.13 "" "")
					;生成轴网层
  (foreach point zw-x-list (command "line" (car point) (cadr point) ""))
  (command "layer" "M" "zhouwang-num" "C" "green" "" "Lw" 0.13 "" "")
					;生成轴网序号及尺寸线层
;;;;;;;;;;;;;;轴网x轴序号标注
  (setq	num 1
	zw-x-end-list nil
  )
  (foreach point zw-x-list
    (numer (xy-move (car point) -2000 0) (+ 50 text-size) num 1)
					;汉字样式编序号
    (setq zw-x-end-list (cons (cadr point) zw-x-end-list))
					;轴网X轴各终点列表
    (setq num (1+ num))
  )
  (setq zw-x-end-list (reverse zw-x-end-list))
  (setq num 0)
  (repeat (1- (length zw-x-end-list))
    (command "dimlinear"
	     (nth num zw-x-end-list)
	     (nth (1+ num) zw-x-end-list)
	     "v"
	     (xy-move (nth (1+ num) zw-x-end-list)
		      (* dim-dist-scale dim-length)
		      -1000
	     )
    )
    (setq num (1+ num))
  )
  (command "dimlinear"
	   (nth 0 zw-x-end-list)
	   (nth num zw-x-end-list)
	   "v"
	   (xy-move (nth num zw-x-end-list)
		    (* 2 dim-dist-scale dim-length)
		    -1000
	   )
  )
;;;;;;;;;;;;;;;画Y轴线
  (setq zw-y0 (xy-move x0y0 0 (- dim-length))) ;y轴线左下角点
  (setq zw-y-list (cons (list zw-y0 (xy-move zw-y0 0 zw-y)) nil))
					;y轴线点对表
  (while (car you-jin-num)
    (repeat (car you-jin-num)
      (setq zw-y-list
	     (cons
	       (list
		 (xy-move (caar zw-y-list) (car you-jin-dist) 0)
		 (xy-move (caar zw-y-list) (car you-jin-dist) zw-y)
	       )
	       zw-y-list
	     )
      )
    )
    (setq you-jin-num  (cdr you-jin-num)
	  you-jin-dist (cdr you-jin-dist)
    )
  )
  (setq zw-y-list (reverse zw-y-list))
  (command "layer" "S" "zhouwang" "")
  (foreach point zw-y-list (command "line" (car point) (cadr point) ""))
;;;;;;;;;;;;;;轴网y轴序号标注
  (setq	num 1
	zw-y-end-list nil
  )
  (command "layer" "S" "zhouwang-num" "")
  (foreach point zw-y-list
    (numer (xy-move (car point) 0 -2000) (+ 50 text-size) num 0)
					;汉字样式编序号
    (setq zw-y-end-list (cons (cadr point) zw-y-end-list))
					;轴网y轴各终点列表
    (setq num (1+ num))
  )
  (setq zw-y-end-list (reverse zw-y-end-list))
  (setq num 0)
  (repeat (1- (length zw-y-end-list))
    (command "dimlinear"
	     (nth num zw-y-end-list)
	     (nth (1+ num) zw-y-end-list)
	     "h"
	     (xy-move (nth (1+ num) zw-y-end-list)
		      -1000
		      (* dim-dist-scale dim-length)
	     )
    )					;各轴线标注
    (setq num (1+ num))
  )
  (command "dimlinear"
	   (nth 0 zw-y-end-list)
	   (nth num zw-y-end-list)
	   "h"
	   (xy-move (nth num zw-y-end-list)
		    -1000
		    (* 2 dim-dist-scale dim-length)
	   )
  )					;总轴线标注
;;;;;;;;;;;;;;;;;
  (command "zoom" "a")
  (setvar "osmode" osmode)
)