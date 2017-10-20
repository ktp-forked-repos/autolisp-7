(defun Get_MinMax (en / vl-en minext maxext)
					;得到矩形最大角点 最小角点 ------别人写的
					;坐标是世界坐标系下
  (vl-load-com)
  (setq	*acadobject*   (vlax-get-acad-object)
	*acaddocument* (vla-get-activedocument *acadobject*)
	*mspace*       (vla-get-modelspace *acaddocument*)
  ) ;_ Endsetq
;;;  (setq en (entsel "Please select a object: "))
  (setq vl-en (vlax-ename->vla-object en))
  (setq minext (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (setq maxext (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (vla-getboundingbox vl-en 'minext 'maxext)
  (list (vlax-safearray->list minext) (vlax-safearray->list maxext) (vla-get-WindowTitle *acaddocument*))
  
)

(defun plot (paper scale paper-min paper-max pdf-name / p-l paper-z transpose)
  ;;根据图纸类型 paper 打印比例 scale 打印范围 打印图纸
  ;;paper-z 纸张类型 paper-min paper-max 窗口 左下角点 右上角点
  ;;transpose 是否上下颠倒打印
  (cond	((= paper "paper-a4y")
	 (setq p-l	 "P"
	       paper-z	 "ISO expand A4 (210.00 x 297.00 毫米)"
	       transpose "n"
	 )
	)
	((= paper "paper-a4x")
	 (setq p-l	 "l"
	       paper-z	 "ISO expand A4 (210.00 x 297.00 毫米)"
	       transpose "n"
	 )
	)
	((= paper "paper-a3x")
	 (setq p-l	 "l"
	       paper-z	 "ISO expand A3 (297.00 x 420.00 毫米)"
	       transpose "n"
	 )
	)
  )					;确定图纸纵向还是横向打印
  (setvar "cmdecho" 0)
  (command "plot"
	   "y"				;详细配置
	   ""				;默认模型空间
	   "DWG To PDF.pc3"	;打印机名称
	   paper-z			;纸张类型
	   "m"				;毫米单位
	   p-l				;打印方向
	   transpose			;上下颠倒打印
	   "W"				;窗口模式
	   paper-min
	   paper-max
	   (strcat "1=" (rtos (* 1.05 scale) 2))	;打印比例
	   "5,7"			;打印偏移
	   ""				;默认是否按样式打印
	   "."				;默认打印样式表 .表示无
	   ""				;默认是否打印线宽
	   ""				;默认着色打印设置
	   pdf-name				;pdf文件名
	   ""				;默认是否保存对页面设置的修改
	   "y"				;是否继续打印
	
  )
 
)






(defun c:bpdf
       (/ osmode ss ss-name scale paper-min paper-max num insert-name)
  ;;批量打印图纸框
  (setq osmode (getvar "osmode"))	;保存捕获设置
  (setvar "osmode" 0)			;取消捕获
  (princ "请选择要打印的图纸框，建议以栏选方式（F）")
  (setq	ss (ssget '((0 . "insert")	;过滤列表 只选择特定名称的图快
		    (-4 . "<or")
		    (2 . "paper-a4y")
		    (2 . "a4y")
		    (2 . "paper-a4x")
		    (2 . "a4x")
		    (2 . "paper-a3x")
		    (2 . "a3x")
		    (-4 . "or>")
		   )
	   )
  )
  (command "ucs" "w")			;恢复世界坐标系,为打印获取正确坐标点做好准备。
  (setq num 0)
  (repeat (sslength ss)
    (setq ss-name     (ssname ss num)	;图元名
	  scale	      (cdr (assoc 41 (entget ss-name))) ;放大比例
	  insert-name (cdr (assoc 2 (entget ss-name))) ;块名称
    )
    (cond ((or (= insert-name "paper-a4y") (= insert-name "a4y"))
	   (progn (setq	paper-min (car (get_minmax ss-name))
			paper-max (cadr (get_minmax ss-name))
			
		  )
		  (plot "paper-a4y" scale paper-min paper-max (rtos num) )
	   )
	  )
	  ((or (= insert-name "paper-a4x") (= insert-name "a4x"))
	   (progn (setq	paper-min (car (get_minmax ss-name))
			paper-max (cadr (get_minmax ss-name))
		  )
		  (plot "paper-a4x" scale paper-min paper-max (rtos num))
	   )
	  )
	  ((or (= insert-name "paper-a3x") (= insert-name "a3x"))
	   (progn (setq	paper-min (car (get_minmax ss-name))
			paper-max (cadr (get_minmax ss-name))
		  )
		  (plot "paper-a3x" scale paper-min paper-max (rtos num))
	   )
	  )
    )
    
    (setq num (1+ num))
    (command "delay" 600) ;延迟0.6秒钟
  )
  (princ (strcat "共打印了" (rtos num) "张图纸"))
  (setvar "osmode" osmode)		;恢复捕获设置
  (princ)
)