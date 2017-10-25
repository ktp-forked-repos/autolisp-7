(defun c:pp  (/ osmode print-name paper-min paper-max p-l paper-z)
  ;; 本程序利用系统自带的dwg to pdf 打印机输出pdf图纸
  ;;布满(f)居中（c）打印
  ;;print-name 打印机名称 paper-z 纸张大小类型
  ;;p-l 竖向还是横向布局 paper-min paper-max 窗口 左下角点 右上角点
  ;;如需根据新环境修改参数，如下参数需修改 print-name 1个位置 paper-z 3个位置
  (setq osmode (getvar "osmode"))
  (setvar "osmode" 33)
  (princ "\n自动循环打印，打印比例<布满>，打印偏移<居中>")
  (while (not (= (progn	(initget "q")
			(setq paper-min
			       (getpoint "\n输入打印范围一角点或按<q>退出:")))
		 "q"))			;判断是否继续打印还是退出
    (setq paper-max  (getcorner paper-min "\n输入打印范围的对角点")
	  print-name "dwg to pdf"	;打印机名称
	  )
    (if	(<= (abs (- (car paper-min) (car paper-max)))
	    (abs (- (cadr paper-min) (cadr paper-max))))
      (setq p-l	    "p"
	    paper-z "ISO full bleed A4 (210.00 x 297.00 毫米)")
					;竖幅A4打印
      (progn
	(setq p-l "l")			;横幅打印
	(if
	  (=
	    (getstring
	      "\nA4图纸还是A3图纸横幅打印（默认A4直接回车，A3请输入3）")
	    "3")
	   (setq paper-z "ISO full bleed A3 (420.00 x 297.00 毫米)")
	   (setq paper-z "ISO full bleed A4 (297.00 x 210.00 毫米)"))))
    (setvar "cmdecho" 0)
    (command
      "plot"
      "y"				;详细配置
      ""				;默认模型空间
      print-name			;打印机名称
      paper-z				;纸张大小类型 A4 A3
      "m"				;毫米单位
      p-l				;打印方向 p 竖向 l横向
      "n"				;上下颠倒打印
      "w"				;窗口模式
      paper-min				;打印范围角点
      paper-max				;打印范围角点
      "f"				;打印比例 布满
      "c"				;打印偏移 居中
      ""				;默认是否按样式打印
      "monochrome.ctb"			;默认打印样式表 黑白打印
      ""				;默认是否打印线宽
      ""				;默认着色打印设置
      (strcat
	(getvar "DWGPREFIX")		;当前文档路径
	(vl-string-subst
	  (strcat "-" (substr (rtos (getvar "cdate") 2 6) 10 6))
	  ".dwg"
	  (getvar "DWGNAME")))		;输出的pdf文件名，以时间结尾
      "n"				;默认是否保存对页面设置的修改
      "y"				;是否继续打印
      ))
  (setvar "osmode" osmode)
  (princ))




;;;  获取文件名  (getvar "DWGPREFIX")  (getvar "DWGNAME")
;;;  获取时间 (getvar "date")





