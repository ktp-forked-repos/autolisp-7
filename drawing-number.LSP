;;; 配合标题栏图块, 批量添加或修改图号, 适合图号不大于99
;;; 对个位数图号前导补0, 如果图号大于99, 数位对不齐
;;; 2017.11.12
;;; 2017.11.13 sort
(defun c:dn  (/ drawing-prefix drawing-no ss-list num biaotilan-attr)
  ;;对标题栏图快进行图号编辑
  ;;drawing-prefix 图号前缀 drawing-no 图号起始编号
  ;;统一修改 标题栏 项目名称 项目编号 
  (initget)
  (setq drawing-prefix (getstring "请输入图号前缀：")
        drawing-no     (getint "请输入图号起始编号<默认是1>："))
  (if (not drawing-no)
    (setq drawing-no 1))
;;;  (setq	project-name (getstring "请输入项目名称<不改动直接回车>：")
;;;	project-num  (getstring "请输入项目编号<不改动直接回车>：")
;;;  )
  (princ "请选择进行图号编辑的图纸框，建议以栏选方式（F）")
  (setq ss-list (ssget2list-sort-wc (ssget '((2 . "标题栏")))))
                                        ;选择标题栏图块,并进行排序处理
  (setq num 0)
  (repeat (length ss-list)
    (setq biaotilan-attr                ;取得标题栏图快属性列表，赋值给biaotilan-attr
           (vlax-safeArray->list
             (vlax-variant-value
               (vla-getattributes
                 (vlax-ename->vla-object (nth num ss-list))))))
    (vla-put-TextString
      (nth 3                            ;图号是标题栏的第4个属性
           biaotilan-attr)              ; 获取”图号“这个属性的图元
      (strcat drawing-prefix (itoa-add0 drawing-no))
                                        ; 图号值：前缀编号 自定义函数itoa-add0, 对个位数前导补0
      )
;;;    (if	(not (= "" project-name))	; 修改项目名称 第5个属性
;;;      (vla-put-TextString (nth 4 biaotilan-attr) project-name)
;;;    )
;;;    (if	(not (= "" project-num))	;修改项目编号 第6个属性
;;;      (vla-put-TextString (nth 5 biaotilan-attr) project-num)
;;;    )
    (setq num        (1+ num)
          drawing-no (1+ drawing-no)))
  (princ (strcat "一共编辑了" (rtos num) "个标题栏。"))
  (princ))


(defun itoa-add0  (n)
  ;;整数转字符串
  ;;对1-9个位数前导补0, 变为"01" "02"
  ;;10以上整数转为默认字符串格式
  (if (and (< n 10) (> n 0))
    (strcat "0" (itoa n))
    (itoa n)))

(defun ssget2list-sort-wc  (ss / ss-list num ss-selid)
  ;;将选择集图元组成列表, 其中
  ;;如果选择集第一个图元是以窗口w或交叉c方式被选中
  ;;则默认整个选择集都是以此种方式被选中
  ;;按位置上下左右进行排序
  ;;窗口w或交叉c方式下, 图元顺序不固定, 违背直觉
  (setq num 0
        ss-list nil)
  (repeat (sslength ss)
    (setq ss-list (cons (ssname ss num) ss-list)
          num     (1+ num)))
  (setq ss-selid (caar (ssnamex ss)))
  (if (or (= ss-selid 2) (= ss-selid 3))
    (vl-sort ss-list 'up-or-left-sort)  ;排序up-or-left-sort
    (reverse ss-list)))                 ;栏交或点选时逆序,以选中的顺序进行下一步处理


(defun up-or-left-sort  (en1 en2 / en1-double-point en2-double-point)
  ;;vl-sort 排序判别式
  ;;图元en1在en2上方
  ;;或者 en1 en2 同列(两者不上不下),且en1在en2左方
  (command "ucs" "w")
  (setq en1-double-point (Get_MinMax en1)
        en2-double-point (Get_MinMax en2))
  (defun up  (en1-double-point en2-double-point) ;en1在en2上方
    (> (cadar en1-double-point)         ; en1-double-point左下y坐标
       (cadadr en2-double-point)))      ; en2-double-point右上y坐标
  (defun left  (en1-double-point en2-double-point) ;en1在en2左方
    (< (caadr en1-double-point)         ;右上x坐标
       (caar en2-double-point)))        ; 左下x坐标
  (or (up en1-double-point en2-double-point) ; en1在en2上方
      (and (not (up en2-double-point en1-double-point))
                                        ;en2不在en1上方,即en1 en2同行
           (left en1-double-point en2-double-point)))) ;en1在en2左方

(defun Get_MinMax  (en / vl-en minext maxext)
                                        ;得到矩形最大角点 最小角点 ------别人写的
                                        ;坐标是世界坐标系下
  (vl-load-com)
;;;  (setq	*acadobject*   (vlax-get-acad-object)
;;;	*acaddocument* (vla-get-activedocument *acadobject*)
;;;	*mspace*       (vla-get-modelspace *acaddocument*)
;;;  ) 
  (setq vl-en (vlax-ename->vla-object en))
  (setq minext (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (setq maxext (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (vla-getboundingbox vl-en 'minext 'maxext)
  (list (vlax-safearray->list minext) (vlax-safearray->list maxext)))


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