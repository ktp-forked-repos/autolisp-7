(defun c:venlo-p ()
;;;绘制文洛式联栋温室平面轴网图
  (defun getdata ()
    (setq kuadu (get_tile "kuadu")
	  kaijian (get_tile "kaijian")
	  jiangao (get_tile "jiangao")
	  duan_lizhu (get_tile "duan_lizhu")
	  bian_lizhu (get_tile "bian_lizhu")
	  p0_x (atof (get_tile "p0_x"))
	  p0_y (atof (get_tile "p0_y"))
    ))
  
  (setq id (load_dialog "venlo-p.dcl"))
  (if (< id 0) (exit))
  (setq std 2
	kuadu "(10.8 5)"
	kaijian "(4 10)"
	jiangao "4"
	duan_lizhu "1"
	bian_lizhu "0"
	p0_x 0 p0_y 0)
  (while (> std 1)
    (if (not (new_dialog "venlo_plan" id)) (exit))
    (set_tile "kuadu" kuadu)
    (set_tile "kaijian" kaijian)
    (set_tile "jiangao" jiangao)
    (set_tile "duan_lizhu" duan_lizhu)
    (set_tile "bian_lizhu" bian_lizhu)
    (set_tile "p0_x" (rtos p0_x 2 2))
    (set_tile "p0_y" (rtos p0_y 2 2))
    (action_tile "pick"  "(getdata) (done_dialog 2)")
    (action_tile "accept" "(getdata) (done_dialog 1)")
    (action_tile "cancel" "(done_dialog 0)")
    (setq std (start_dialog))
    (if (= std 2)
      (progn
	(initget 1)
	(setq p (getpoint "输入左下定位点："))
	(setq p0_x (car p) p0_y (cadr p))))
    )
  (if  (= std 1)
    (progn
      (venlo-plan kuadu kaijian duan_lizhu
	bian_lizhu p0_x p0_y))
    )
  (unload_dialog id)
  (command "zoom" "e")
  (princ)
  )
 
  

(defun venlo-plan (kuadu kaijian duan_lizhu bian_lizhu
		    p0_x p0_y / osmode celtscale)
  ;;文洛式联栋温室基础平面图
  ;;适合9.6m（3.2m、6.4m）10.8m（3.6m 7.2m）12m（4m 8m）16m(4m 8m)跨度,4m\8m开间
  ;;矩形布局
  ;;跨度规格表格式(跨度 跨数 [跨度 跨数])如(9.6 3 6.4 1 9.6 3)
  ;;开间规格表格是(开间 开间数 [开间 开间数])如(4 10)或(4 4 8 1 4 4)
  (setq osmode (getvar "osmode"))
  (setvar "osmode" 0)
  (setvar "pdmode" 66)
  (setvar "pdsize" 1200)
  (setvar "cmdecho" 0)
  (command "layer" "Make" "基础" "")	;设置基础点所在图层
  (command "layer" "Make" "天沟" "Color" 1 "" "Ltype" "Center" "" "") ;设置天沟线所在图层
  (command "layer" "Make" "桁架" "Color" 3 "" "Ltype" "Center" "" "") ;设置桁架线所在图层
  (setq	
	kuadu	    (read kuadu)	;跨度规格表
	kaijian	    (read kaijian)	;开间规格表
	kuadubiao   kuadu
	kaijianbiao kaijian
	p0 (list p0_x p0_y)
	text-size  700                   ;设置编号球内文字大小及编号球大小
  )
  (setvar "clayer" "基础")
  
  (command "point" p0 "")
  (setq p-set (ssadd (entlast)))	;创建端点选择集
  (setq	vl 0
	vw 0
  )					;vl温室跨度方向长度,vw温室开间方向长度
  (setq	tj (cond ((= (car kuadu) 16) 4000)
		 ((= (rem (* 10 (car kuadu)) 32) 0) 3200)
		 ((= (rem (* 10 (car kuadu)) 36) 0) 3600)
		 ((= (rem (* 10 (car kuadu)) 40) 0) 4000)
	   )
  )					; tj 两排天沟直接距离,即温室屋脊跨度
;;;;;;;;跨度方向第一排布置
  (setq pp p0)
  (while (/= (car kuadu) nil)
    (setq kd (* 1000 (car kuadu))	;跨度
	  ks (cadr kuadu)		;跨度数量
    )
    (setq vl (+ vl (* kd ks)))
    (repeat ks
      (setq pp (polar pp 0 kd))
      (command "point" pp "")
      (ssadd (entlast) p-set)
    )
    (setq kuadu (cddr kuadu))		;下一个跨度规格
  )
;;;;;;;;;开间方向阵列
  (while (/= (car kaijian) nil)
    (setq kj  (* 1000 (car kaijian))	;kj开间距离(mm)
	  kjs (cadr kaijian)		;kjs开间数量
    )
    (repeat kjs
      (setq vw (+ vw kj))
      (command "copy" p-set ""
	       "d" (list 0 vw))
      )    
    (setq kaijian (cddr kaijian))	;下一个开间规格
  )
;;;;;;;;桁架线和天沟线
  (setq	tjs   (fix (1+ (/ vl tj)))	;tjs 天沟列数
	kjs+1 (1+ (/ vw 4000))
  )					;kjs+1 开间方向桁架行数

  
  (setq celtscale (getvar "celtscale")) ;设置线型比例
  (setvar "celtscale" 1) ;设置线型比例因子
  
  (setvar "clayer" "桁架")
  (command "line" p0 (polar p0 0 vl) "") ;桁架线
  (command "array"
	   (entlast)
	   ""
	   "r"
	   kjs+1
	   1
	   4000
  )

  (setvar "clayer" "天沟")
  (command "line" p0 (polar p0 (/ pi 2) vw) "") ;天沟线
  (command "array"
	   (entlast)
	   ""
	   "r"
	   1
	   tjs
	   tj
  )
  (setvar "celtscale" celtscale) ;恢复线型比例

  (setvar "clayer" "0")
  (command "style" "number" "gbcbig" (rtos text-size 2 0) "" "" "" "" "")
					;设置编号用字体
  (setq	p0- (polar p0 (/ pi -2) 4000)	;p0- 列编号起始点
	p0+ (polar p0 pi tj)
  )					;p0+ 行编号起始点
  (setq n 1)
  (repeat tjs				;X轴列编号
    (numer p0- (+ 50 text-size) n)			;编号函数
    (setq p0- (polar p0- 0 tj)
	  n   (1+ n)
    )
  )
  (setq n 1)
  (repeat kjs+1				;Y轴行编号,转换成汉字
    (numer-to-c p0+ (+ 50 text-size) n)
    (setq p0+ (polar p0+ (/ pi 2) 4000)
	  n   (1+ n)
    )
  )
;;;;;;;;;;;是否添加端立柱和边立柱
 
  (if (= duan_lizhu "1")
    (progn
      (setq pp	   p0
	    pp-set (ssadd)
      )
      (while (/= (car kuadubiao) nil)
	(setq kd (* 1000 (car kuadubiao)) ;跨度
	      ks (cadr kuadubiao)	;跨度数量
	)
	(repeat	ks
	  (repeat (fix (1- (/ kd tj)))
	    (setq pp (polar pp 0 tj))
	    (command "point" pp "")
	    (ssadd (entlast) pp-set)
	  )
	  (setq pp (polar pp 0 tj))
	)
	(setq kuadubiao (cddr kuadubiao)) ;下一个跨度规格
      )
      (command "copy"
	       pp-set
	       ""
	       "d"
	       (list 0 vw)
      )
    )
  )
;;;;;;;;;;

  (if (= bian_lizhu "1")
    (progn
      (setq pp	   p0
	    pp-set (ssadd)
	    vw	   0
      )

      (while (/= (car kaijianbiao) nil)
	(setq kj  (* 1000 (car kaijianbiao)) ;kj开间距离(mm)
	      kjs (cadr kaijianbiao)
	)				;kjs开间数量
	(if (= kj 8000)
	  (progn
	    (repeat kjs
	      (setq pp (polar pp (/ pi 2) 4000))
	      (command "point" pp "")
	      (ssadd (entlast) pp-set)
	      (setq pp (polar pp (/ pi 2) 4000))
	    )
	  )

	  (setq	vw (+ vw (* kj kjs))
		pp (polar pp (/ pi 2) vw)
	  )
	)
	(setq kaijianbiao (cddr kaijianbiao)) ;下一个开间规格
      )
      (command "copy" pp-set ""
	       "d" (list vl 0))
    )
  )
  (setvar "osmode" osmode)
)

(defun itoc (n / n-str str jump)
					;将整数n转换成汉字“〇一二三...九”的形式
  (setq	n-str (itoa n) ;将数字转换成字符串
	jump  1 ;循环增量
	str   "" ;保存转换后的字符串，先设置成空串
  )
  (repeat (strlen n-str) ;根据数字位数设置循环次数

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

(defun numer (p r n)
  ;;p圆心 r半径 n编号
  (command "circle" p r)
  (command "text" "m" p "" (itoa n))
)

(defun numer-to-c (p r n)
   ;;p圆心 r半径 n编号
  (command "circle" p r)
  (command "text" "m" p "" (itoc n));转换成汉字数码样式
)

  (print "文洛式联栋温室基础平面图")
  (print "适合9.6m（3.2m、6.4m）10.8m（3.6m 7.2m）12m（4m 8m）16m(4m 8m)跨度,4m\8m开间")
  (print "矩形布局")
  (print "跨度规格表格式(跨度 跨数 [跨度 跨数])如(9.6 3 6.4 1 9.6 3)")
  (print "开间规格表格是(开间 开间数 [开间 开间数])如(4 10)或(4 4 8 1 4 4)")