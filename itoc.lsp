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
