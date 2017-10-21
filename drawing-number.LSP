(defun c:dn (/		    osmode	   drawing-prefix
	     drawing-no	    ss		   num
	     biaotilan-attr project-name   project-num
	    )
  ;;对标题栏图快进行图号编辑
  ;;drawing-prefix 图号前缀 drawing-no 图号起始编号
  ;;统一修改 标题栏 项目名称 项目编号 
;;;  (setq osmode (getvar "osmode"))	;保存捕获设置
;;;  (setvar "osmode" 0)			;取消捕获
  (initget)
  (setq	drawing-prefix (getstring "请输入图号前缀：")
	drawing-no     (getint "请输入图号起始编号<默认是1>：")
  )
  (if (not drawing-no)
    (setq drawing-no 1)
  )
  (setq	project-name (getstring "请输入项目名称<不改动直接回车>：")
	project-num  (getstring "请输入项目编号<不改动直接回车>：")
  )
  (princ "请选择进行图号编辑的图纸框，建议以栏选方式（F）")
  (setq ss (ssget '((2 . "标题栏"))))	;ss是标题栏图快的选择集
  (setq num 0)
  (repeat (sslength ss)
    (setq biaotilan-attr		;取得标题栏图快属性列表，赋值给biaotilan-attr
	   (vlax-safeArray->list
	     (vlax-variant-value
	       (vla-getattributes (vlax-ename->vla-object (ssname ss num)))
	     )
	   )
    )
    (vla-put-TextString
      (nth 3				;图号是标题栏的第4个属性
	   biaotilan-attr
      )					; 获取”图号“这个属性的图元
      (strcat drawing-prefix (rtos drawing-no 2)) ; 图号值：前缀-编号
    )
    (if	(not (= "" project-name))	; 修改项目名称 第5个属性
      (vla-put-TextString (nth 4 biaotilan-attr) project-name)
    )
    (if	(not (= "" project-num))	;修改项目编号 第6个属性
      (vla-put-TextString (nth 5 biaotilan-attr) project-num)
    )
    (setq num (1+ num)
	  drawing-no
	   (1+ drawing-no)
    )
  )
;;;  (setvar "osmode" osmode)		;恢复捕获设置
  (princ (strcat "一共编辑了" (rtos num) "个标题栏。"))
  (princ)
)


;;;(defun block-attributes	(block attr-name attr-value / lstAttrs attr)
;;;					;对于图元block的属性attr-name进行赋值attr-value
;;;  (setq	lstAttrs (vlax-safeArray->list
;;;		    (vlax-variant-value
;;;		      (vla-getattributes
;;;			(vlax-ename->vla-object block)
;;;		      )
;;;		    )
;;;		  )
;;;  )					;获取块block的属性列表
;;;  (foreach attr	lstAttrs
;;;    (if	(= (vla-get-TagString attr) attr-name)
;;;					;此处为你想要的属性名
;;;      (vla-put-TextString attr attr-value)
;;;    )
;;;  )
;;;  ;(vla-put-TextString (nth 3 lstAattrs) attr-value)  ;对特定块，如果知道属性顺序，可提升速度
;;;)