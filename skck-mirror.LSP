(defun c:skck-mirror  (/ osmode ffd ff n point point-list det-xy)
					;数控冲孔(skck) 程序代码辅助生成
					;依据选定的点，生成数控冲孔程序辅助代码
					;适用于孔距离边小于58mm的板上冲孔。
					;板上孔对称。选择一半孔，冲完后，板反面，冲另一半镜像冲孔。
					;生成的辅助代码并不能保证运行正确，还需人工检查和生产实践！
					;版本V1.0 时间：2016-05-14 作者 kangdalei
  (setq osmode (getvar "osmode"))
  (setvar "osmode" 4)			;设定圆心捕获
  (setq ffd (getfiled "写入冲孔程序文件" "" "txt" 1))
  (setq ff (open ffd "w"))		;文件指针
  (write-line "G84 X0 Y0" ff)		;辅助代码开头-开始冲孔
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
  (princ (strcat "一共要冲" (rtos (* 2 (1- n)) 2) "个孔。"))
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
  (write-line "M05 " ff)		;辅助代码中间返回-开始 M05 停冲头
  (write-line "G04 K1" ff)		;暂停1秒
  (write-line "G29" ff)			;返回原点
  (write-line "M11" ff)			;后夹松开
  (write-line "G04 K14" ff)		;暂停14秒，板翻面
  (write-line "M10" ff)			;后夹夹紧
  (write-line "G84 X0 Y0" ff)		;开始冲另一半
  (setq n 0)
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
  (write-line "M05" ff)
  (write-line "G04 K1" ff)
  (write-line "M11" ff)			;后夹松开
  (write-line "G04 K6" ff)		;暂停6秒，卸下板材
  (write-line "G29" ff)			;返回原点
  (write-line "G04 K8" ff)             ;暂停8秒，上板材
  (write-line "M10" ff)			;后夹夹紧
  (write-line "M30" ff)			;辅助代码结尾-结束
  (close ff)
  (setvar "osmode" osmode)		;恢复开始捕获设置
  )