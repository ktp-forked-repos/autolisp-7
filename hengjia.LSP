
(setq osmode (getvar "osmode"))
(setvar "osmode" 0)

;;;(command "layer" "s" "0" "") ;回到0图层
;;;


(defun jiaochengban (point num)		;画角撑板 point是两立边交点  num是象限点 1、2、3、4四个象限
  (cond	((= num 1)
	 (command "line" (xy-move point 150 0) "@-150,150" "")
	 (command "line" (xy-move point 79 0) "@-79,79" "")
	)
	((= num 2)
	 (command "line" (xy-move point -150 0) "@150,150" "")
	 (command "line" (xy-move point -79 0) "@79,79" "")
	)
	((= num 3)
	 (command "line" (xy-move point -150 0) "@150,-150" "")
	 (command "line" (xy-move point -79 0) "@79,-79" "")
	)
	((= num 4)
	 (command "line" (xy-move point 150 0) "@-150,-150" "")
	 (command "line" (xy-move point 79 0) "@-79,-79" "")
	)
  )
)




(defun xy-move (X0Y0 x-det y-det) (mapcar '+ x0y0 (list x-det y-det)))


(defun hengjia-3wuji ()			;3屋脊桁架孔和竖撑管画法
  (command "line"
	   (xy-move pt-ld (- (/ kuaju 3) 25 (/ lizhu-kuan 2)) guan-gao)
	   (strcat "@0," (rtos (- hengjia-gao guan-gao guan-gao) 2))
	   ""
  )
  (command "copy" (entlast) "" "d" "50,0")
  (command
    "line"
    (xy-move pt-rd (- (- (/ kuaju 3) 25 (/ lizhu-kuan 2))) guan-gao)
    (strcat "@0," (rtos (- hengjia-gao guan-gao guan-gao) 2))
    ""
  )
  (command "copy" (entlast) "" "d" "-50,0")
  (command
    "circle"
    (xy-move pt-lt
	     (- (/ kuaju 3) dist-of-top-front-view (/ lizhu-kuan 2))
	     (+ (/ guan-kuan 2) dist-of-top-front-view)
    )
    "5"
  )
  (command "copy" (entlast) "" "d" "160,0")
  (command
    "circle"
    (xy-move pt-rt
	     (- (- (/ kuaju 3) dist-of-top-front-view (/ lizhu-kuan 2)))
	     (+ (/ guan-kuan 2) dist-of-top-front-view)
    )
    "5"
  )
  (command "copy" (entlast) "" "d" "-160,0")
)

(defun hengjia-2wuji ()			;2屋脊桁架孔和竖撑管画法
  (command "line"
	   (xy-move pt-ld (- (/ kuaju 2) 25 (/ lizhu-kuan 2)) guan-gao)
	   (strcat "@0," (rtos (- hengjia-gao guan-gao guan-gao) 2))
	   ""
  )
  (command "copy" (entlast) "" "d" "50,0")
  (command
    "circle"
    (xy-move pt-lt
	     (- (/ kuaju 2) dist-of-top-front-view (/ lizhu-kuan 2))
	     (+ (/ guan-kuan 2) dist-of-top-front-view)
    )
    "5"
  )
  (command "copy" (entlast) "" "d" "160,0")
)

(defun hengjia-kaijian () (setvar "osmode" 167) (exit))

(defun c:hengjia (/		kuaju	      lizhu-kuan
		  hengjia-gao	guan-gao      guan-kuan
		  hengjia-length	      pt-ld
		  pt-lt		pt-rt	      pt-rd
		  hengjia-setting
		 )
  (setq	dist-of-left-front-view
	 150				; 左视图和主视图距离
	dist-of-top-front-view 80
  )					;俯视图和主视图距离
  (setq	kuaju	    (getreal "\n请输入跨距<12000>：")
	lizhu-kuan  (getreal "\n请输入立柱宽度<120>：")
	hengjia-gao (getreal "\n请输入桁架高度<600>：")
	guan-gao    (getreal "\n请输入上下弦管高度<50>：")
	guan-kuan   (getreal "\n请输入下弦管宽度<50>：")
  )
  (if (not kuaju)
    (setq kuaju 12000)
  )
  (if (not lizhu-kuan)
    (setq lizhu-kuan 120)
  )
  (if (not hengjia-gao)
    (setq hengjia-gao 600)
  )
  (if (not guan-gao)
    (setq guan-gao 50)
  )
  (if (not guan-kuan)
    (setq guan-kuan 50)
  )
  (initget "1 2 3 4 ")
  (setq	hengjia-setting
	 (getkword
	   "选择桁架类型：<1>三屋脊<2>两屋脊<3>山墙线梁<4>8米开间桁架"
	 )
  )
  (setq hengjia-length (- kuaju lizhu-kuan)) ;桁架长度
  (setq pt-ld (getpoint "请输入桁架左下角点："))
  (setq	pt-lt (xy-move pt-ld 0 hengjia-gao)
	pt-rt (xy-move pt-lt hengjia-length 0)
	pt-rd (xy-move pt-ld hengjia-length 0)
  )
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (command "rectang" pt-ld pt-rt)
  (command "rectang"			;左视图
	   (xy-move pt-ld (- dist-of-left-front-view) 0)
	   (xy-move pt-ld
		    (- (+ guan-kuan dist-of-left-front-view))
		    hengjia-gao
	   )
  )
  (command
    "rectang"				;排锌孔
    (xy-move pt-ld
	     (- (+ (/ (- guan-kuan 25) 2) dist-of-left-front-view))
	     5
    )
    "@-25,25"
  )
  (command
    "rectang"				;排锌孔
    (xy-move pt-lt
	     (- (+ (/ (- guan-kuan 25) 2) dist-of-left-front-view))
	     (- 5 guan-gao)
    )
    "@-25,25"
  )
  (command "rectang"			;俯视图
	   (xy-move pt-lt 0 dist-of-top-front-view)
	   (xy-move pt-rt 0 (+ guan-kuan dist-of-top-front-view))
  )
  (command "line"
	   (xy-move pt-lt 10 dist-of-top-front-view)
	   (xy-move pt-lt 10 (+ guan-kuan dist-of-top-front-view))
	   ""
  )
  (command "line"
	   (xy-move pt-rt -10 dist-of-top-front-view)
	   (xy-move pt-rt -10 (+ guan-kuan dist-of-top-front-view))
	   ""
  )
  (command "line" (xy-move pt-ld 10 0) (xy-move pt-lt 10 0) "")
  (command "line" (xy-move pt-rd -10 0) (xy-move pt-rt -10 0) "")
  (command "line"
	   (xy-move pt-ld 10 guan-gao)
	   (xy-move pt-rd -10 guan-gao)
	   ""
  )
  (command "line"
	   (xy-move pt-lt 10 (- guan-gao))
	   (xy-move pt-rt -10 (- guan-gao))
	   ""
  )
  (jiaochengban (xy-move pt-ld 10 guan-gao) 1)
  (jiaochengban (xy-move pt-lt 10 (- guan-gao)) 4)
  (jiaochengban (xy-move pt-rd -10 guan-gao) 2)
  (jiaochengban (xy-move pt-rt -10 (- guan-gao)) 3)
  (cond	((= "1" hengjia-setting) (hengjia-3wuji))
	((= "2" hengjia-setting) (hengjia-2wuji))
	((= "3" hengjia-setting) (setvar "osmode" 167) (exit))
	((= "4" hengjia-setting) (hengjia-kaijian))
  )
  (setvar "osmode" 167)
)













