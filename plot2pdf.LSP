;;;批量将图框paper-a4x paper-a4y paper-a3 范围内打印成pdf图纸
;;;2017.11.09 ok!
;;;2017.11.10 refactor
(defun c:p2p  (/	    osmode	 ss	      ss-name
	       scale	    paper-min-max	      paper-min
	       paper-max    num		 insert-name  draw-paper-name)
  ;; osmode 存储捕获模式
  ;; ss 图框选择集
  (setq osmode (getvar "osmode"))	;保存捕获设置
  (setvar "osmode" 0)			;取消捕获
  (setvar "PLOTOFFSET" 1)		; 设置将打印偏移设定为相对于图纸边
  (princ "请选择要打印的图纸框，建议以栏选方式（F）")
  (setq	ss (ssget '((0 . "insert")	;过滤列表 只选择特定名称的图快
		    (-4 . "<or")
		    (2 . "paper-a4y")
		    (2 . "a4y")
		    (2 . "paper-a4x")
		    (2 . "a4x")
		    (2 . "paper-a3x")
		    (2 . "a3x")
		    (-4 . "or>"))))
  (command "ucs" "w")			;恢复世界坐标系,为定义get_minmax函数获取正确坐标点做好准备
  (setq num 0)				; 使用num 循环遍历 图框选择集
  (repeat (sslength ss)
    (setq ss-name     (ssname ss num)	;获取第n个图元名
	  scale	      (cdr (assoc 41 (entget ss-name)))
					;scale 图框的放大比例
	  insert-name (cdr (assoc 2 (entget ss-name))) ;图框块的名称
	  )
    (if	(= insert-name "a4y")		;兼容历史遗留的a4x a4y a3x 图框名字 
      (setq insert-name "paper-a4y"))
    (if	(= insert-name "a4x")
      (setq insert-name "paper-a4x"))
    (if	(= insert-name "a3x")
      (setq insert-name "paper-a3x"))
    (setq paper-min-max	  (get_minmax ss-name)
					;自定义get_minmax函数,获取图框对角点坐标
	  paper-min	  (car paper-min-max)
					;图块左下角坐标点                            
	  paper-max	  (cadr paper-min-max) ;图框右上角坐标点
	  draw-paper-name (get-biaotilan-attr-papername
			    paper-min
			    paper-max))	;自定义函数,获取标题栏中图号和图纸名
    (plot insert-name scale paper-min paper-max draw-paper-name)
					;自定义plot函数,打印成pdf文件
    (command "_text"			;图框下单行文字,标注打印日期时间
	     "s"
	     (strcat "s-" (rtos scale))	; 设置文字样式,画图框时已设置相应样式
	     (mapcar '+ paper-min (list 0 (* -4 scale))) ;文字位置
	     "0"
	     (strcat "已于 "
		     (substr (rtos (getvar "cdate") 2 7) 1 15)
					;打印日期时间
		     " 打印为 PDF 文件"))
    (setq num (1+ num)))
  (princ (strcat "共打印了" (rtos num) "张图纸"))
  (setvar "osmode" osmode)		;恢复捕获设置
  (princ))

(defun Get_MinMax  (en / vl-en minext maxext)
					;得到矩形最大角点 最小角点 ------别人写的
					;坐标是世界坐标系下
  (vl-load-com)
  (setq	*acadobject*   (vlax-get-acad-object)
	*acaddocument* (vla-get-activedocument *acadobject*)
	*mspace*       (vla-get-modelspace *acaddocument*)) ;_ Endsetq
;;;  (setq en (entsel "Please select a object: "))
  (setq vl-en (vlax-ename->vla-object en))
  (setq minext (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (setq maxext (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (vla-getboundingbox vl-en 'minext 'maxext)
  (list (vlax-safearray->list minext) (vlax-safearray->list maxext)))

(defun plot  (paper	   scale	paper-min    paper-max
	      pdf-filename /		p-l	     paper-z
	      transpose	   plot-offset	plot-min     plot-max)
  ;;接受主程序c:p2p生成参各个数，然后调用cad自身的plot命令打印图纸
  ;;根据图纸类型 paper(a4x a4y a3x) 打印比例 scale 打印范围 打印图纸 打印偏移 plot-offset
  ;;打印范围 plot-min plot-max
  ;;pdf-filename 打印的文件名
  ;;paper-z 纸张类型 paper-min paper-max 窗口 左下角点 右上角点
  ;;transpose 是否上下颠倒打印
  (cond	((= paper "paper-a4y")
	 (setq p-l	   "P"		; 竖幅打印
	       paper-z	   "ISO full bleed A4 (210.00 x 297.00 毫米)"
					;纸张名字
	       transpose   "y"
	       plot-offset "25,5"	; 打印偏移
	       plot-min	   (mapcar '+ paper-min (list (* scale 25) (* scale 5)))
					;获取打印范围左下框点
	       plot-max	   (mapcar '+ paper-max (list (* scale -5) (* scale -5)))
					;获取打印范围右上框点
	       ))
	((= paper "paper-a4x")
	 (setq p-l	   "l"		;横幅打印
	       paper-z	   "ISO full bleed A4 (297.00 x 210.00 毫米)"
	       transpose   "n"
	       plot-offset "5,5"
	       plot-min	   (mapcar '+ paper-min (list (* scale 5) (* scale 5)))
					;获取打印范围左下框点
	       plot-max	   (mapcar '+
				   paper-max
				   (list (* scale -5) (* scale -25)))
					;获取打印范围右上框点
	       ))
	((= paper "paper-a3x")
	 (setq p-l	   "l" ;横幅打印
	       paper-z	   "ISO full bleed A3 (420.00 x 297.00 毫米)"
	       transpose   "n"
	       plot-offset "25,5"
	       plot-min	   (mapcar '+ paper-min (list (* scale 25) (* scale 5)))
					;获取打印范围左下框点
	       plot-max	   (mapcar '+
				   paper-max
				   (list (* scale -5) (* scale -5)))
					;获取打印范围右上框点
	       )))			;确定图纸纵向还是横向打印
  (setvar "cmdecho" 0)
  (command "plot"
	   "y"				;详细配置
	   ""				;默认模型空间
	   "dwg to pdf"			;打印机名称
	   paper-z			;纸张类型
	   "m"				;毫米单位
	   p-l				;打印方向
	   transpose			;上下颠倒打印
	   "W"				;窗口模式
	   plot-min
	   plot-max
	   (strcat "1:" (rtos scale 2))	;打印比例
	   plot-offset			;打印偏移
	   ""				;默认是否按样式打印
	   "."				;默认打印样式表 .表示无
	   ""				;默认是否打印线宽
	   ""				;默认着色打印设置
	   (strcat (getvar "DWGPREFIX")	;当前文档路径
		   (strcat pdf-filename	;图名
			   "_"
			   ;(substr (getvar "DWGNAME") 1 3)
					;CAD文档名前3个字符，如果是中文会乱码
			   (car (regex-extract "^.{3}" (getvar "DWGNAME") "ig"))
;使用了正则表达式提取文档名前3个字符，可以处理中文
			   "_"
			   (substr (rtos (getvar "cdate") 2 6) 5 4)))
					;输出的pdf文件名:图名-CAD文件名前3位-生成日期时间
	   "n"				;默认是否保存对页面设置的修改
	   "y"				;是否继续打印
	   ))


(defun get-biaotilan-attr-papername
       (paper-min paper-max / ss block-attr-list paper-name paper-num)
  ;;给定图幅框范围paper-min paper-max
  ;;返回图幅框范围内标题栏块的属性"图号"-"图名"的值
  ;;图名为 PDF文件的文件名组成部分
  (setq ss (ssget "_W" paper-min paper-max '((2 . "标题栏"))))
					;ss是标题栏图快
					;取得标题栏图快属性列表，赋值给biaotilan-attr
  (setq	block-attr-list	(vlax-safeArray->list
			  (vlax-variant-value
			    (vla-getattributes
			      (vlax-ename->vla-object (ssname ss 0)))))
					;block-attr-list 标题栏的属性列表
	paper-name	(vlax-get-property ;paper-name 图名
			  (nth 0	;图名是标题栏的第1个属性
			       block-attr-list)
			  'TextString)
	paper-num	(vlax-get-property ;paper-num 图号
			  (nth 3	;图号是标题栏的第4个属性
			       block-attr-list)
			  'TextString))
  (if (/= paper-num "图号")		;如果有特定图号,返回 图号-图名,否则单独返回 图名
    (strcat paper-num "#" paper-name)   ;使用#分割图号 图名
    paper-name))

(defun regex-extract (pat str key / regex S tmp str1)
  ;; 提取正则表达式匹配到的内容
  ;; pat 正则表达式 str 字符串 
  ;; pat 中 \ 使用 \\
  ;; key "igm" i(Ignorecase)忽略大小写 g (Global)全局匹配 m (Multili) 多行模式
  ;; 注意：一般使用全局匹配 g
  ;; 可组合使用或单独使用 或置空 ""
  (vl-load-com)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;引用正则表达式控件
  (if (wcmatch key "*i*,*I*")
    (vlax-put-property regex "IgnoreCase" 1) ;忽略大小写
    (vlax-put-property regex "IgnoreCase" 0) ;不忽略大小写
    )
  (if (wcmatch key "*g*,*G*")
    (vlax-put-property regex "Global" 1) ;匹配方式，全文字匹配
    (vlax-put-property regex "Global" 0)
    )
  (if (wcmatch key "*m*,*M*")
    (vlax-put-property regex "Multiline" 1) ;多行模式
    (vlax-put-property regex "Multiline" 0)
    )
  (vlax-put-property regex "Pattern" pat)
  (setq s (vlax-invoke-method regex "Execute" str))
  ;;将规则运用到STR字符，得到提取出的文字内容
  (VLAX-FOR tmp s                       ;遍历集合对象
    (setq str1 (cons (vlax-get-property tmp "value") str1))
    )
  ;;将内容转换为LISP语言就可以直接观察了
  (vlax-release-object regex)
  (REVERSE str1)
  )





