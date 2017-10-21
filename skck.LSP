(defun c:skck  (/ osmode ffd ff n point point-list det-xy)
					;数控冲孔(skck) 程序代码辅助生成
					;依据选定的点，生成数控冲孔程序辅助代码
					;只适用于一次行程完成全部冲孔任务
					;生成的辅助代码并不能保证运行正确，还需人工检查和生产实践！
					;版本V1.0 时间：2015-10-23 作者 kangdalei
  (setq osmode (getvar "osmode"))
  (setvar "osmode" 4)			;设定圆心捕获
  (setq ffd (getfiled "写入冲孔程序文件" "" "txt" 1))
  (setq ff (open ffd "w"))		;文件指针
  (write-line "M10" ff)			;辅助代码开头-开始
  (write-line "G84 X0 Y0" ff)		;辅助代码开头-结束
  (setq point-list nil)			;圆心坐标的存储列表
  (setq	point	   (getpoint "请输入要冲孔的第一个圆心位置")
	point-list (cons point nil)
	n	   2)
  (while (setq point
		(getpoint point
			  (strcat "\n请输入第"
				  (rtos n 2)
				  "个要冲孔的圆心位置（结束请按ENT）:")))
    (setq point-list (cons point point-list)
	  n	     (1+ n)))
  (princ (strcat "一共要冲" (rtos (1- n) 2) "个孔。"))
  (setq	point-list (reverse point-list)
	n	   0)
  (repeat (1- (length point-list))
    (setq det-xy
	   (mapcar '- (nth (1+ n) point-list) (nth n point-list))
					;依次取出两个点，进行坐标相对计算
	  )
    (write-line
      (strcat "G84 X"
	      (rtos (car det-xy) 2)
	      " Y"
	      (rtos (cadr det-xy) 2))
      ff)				;代码写入文件
    (setq n (1+ n)))
  (write-line "M05 " ff)		;辅助代码结尾-开始
  (write-line "M11" ff)
  (write-line "G04 K4" ff)
  (write-line "G29" ff)
  (write-line "G04 K9" ff)
  (write-line "M30" ff) 		;辅助代码结尾-结束
  (close ff)
  (setvar "osmode" osmode)		;恢复开始捕获设置
  )