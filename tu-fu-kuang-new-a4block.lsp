;;;在模型空间画出图幅框，并设置字体和标注样式
;;;只需给出图幅框的两个对角点，自动求出比例因子
;;;支持A4横幅、竖幅和A3横幅的图幅框
;;;Version: 2.5
;;;修改时间：2015/11/23
;;;作者：kangdalei#gmail.com



(defun c:tu-fu-kuang  (/ p1 p2 lenth width p0 l/w osmode)
  (setq osmode (getvar "osmode"))       ;保存捕获设置
  (setvar "osmode" 0)                   ;取消捕获
  (setvar "ATTDIA" 0) 设置系统变量为0，插入块时命令行输入属性
  (princ "给出图幅框的两个对角点：")
  (setq p1    (getpoint "\n第一点：")   ;图幅框第一点
        p2    (getcorner p1 "")         ;第二点
        lenth (len p1 p2)               ;图幅框水平距离
        width (wid p1 p2)               ;图幅框竖直距离
        p0    (bottom-left p1 p2)       ;获得图幅框的左下角点
        l/w   (/ lenth width))          ;图幅框的长宽比
  (command "style"                      ;设置字体，将来标注样式使用
           "Annotative" "gbcbig.shx" 0  ;字体高度为0
           "" "" "" "" "")
  (setvar "celweight" 25)               ;设置线宽0.25mm
  (cond ((<= l/w 1)                     ;竖幅图纸
         (a4y-kuang p0
                    (max (fix-scal lenth 180) ;在长和宽中选择以较大比例
                         (fix-scal width 287)))) ;A4竖幅
        ((a4x?)                         ;判断是A4横幅还是A3横幅?
         (a4x-kuang p0
                    (max (fix-scal lenth 267) (fix-scal width 200))))
                                        ;A4横幅
        (t
         (a3x-kuang p0
                    (max (fix-scal lenth 390) (fix-scal width 287))))
                                        ;A3横幅
        )
  (setvar "celweight" -1)               ;设置线宽随层
  (setvar "osmode" osmode)              ;恢复捕获设置
  (setvar "ATTDIA" 1) 设置系统变量为1，插入块时cui输入属性
  (princ)
  )

;;;;;;;以下是子函数
(defun a4x?  ()
  ;;用户选择以A3还是A4打印
  (if
    (/=
      (getstring
        "预计将来以A4纸（横幅）打印？(y/n，选否则以A3纸（横幅）打印。）")
      "n")
     t
     nil))

(defun fix-scal  (n1 n2)
  ;;计算比例因子，只截取1位小数
  (cond ((<= n1 n2) 1)                  ; 图幅框只放大或保持原尺寸,不缩小
        (t (/ (fix (* 10 (/ (float n1) n2))) 10.0))))


(defun bottom-left  (p1 p2 / p3 p4)
  ;;p1 p2是对角点
  ;;获得p1 p2所在矩形左下角点
  (setq p3 (list (car p1) (cadr p2))
        p4 (list (car p2) (cadr p1)))
  (list (min (car p1) (car p2) (car p3) (car p4))
        (min (cadr p1) (cadr p2) (cadr p3) (cadr p4))))

(defun len  (p1 p2)                     ;获得对角点水平长度（矩形长）
  (abs (- (car p1) (car p2))))
(defun wid  (p1 p2)                     ;获得对角点垂直高度（矩形宽）
  (abs (- (cadr p1) (cadr p2))))

(defun xy-m  (xy x-move y-move scale)   ;直角坐标系中二维点平移
                                        ;xy是原始点坐标，x-move、y-move是坐标偏移量，scale是比例因子
  (mapcar '+ xy (list (* scale x-move) (* scale y-move))))

(defun a4x-kuang  (p0 scale )
                                        ;给出左下角点和比例，画出A4横幅图幅框
                                        ;(setq osmode (getvar "osmode"))
                                        ;(setvar "osmode" 0)                   ;设置对象捕获为0

  (command "insert" "paper-a4x" p0 (rtos scale 2) "0" (rtos scale 2)) ;插入图框块
  (command "style"                      ;建立文字样式
           (strcat "s-" (rtos scale 2 1))
           "gbcbig.shx"
           (rtos (* 3 scale) 2 1)
           ""
           ""
           ""
           ""
           "")
  (biao-ti-lan (xy-m p0 287 0 scale) scale)

  (setvar "celweight" 13)  
;;;    (command "text"
;;;           (xy-m p0- 120 32 scale)
;;;           ""
;;;           "首件必检！中间抽查！首件必检！")
  (tufu-dim scale)                      ;设置标注样式
                                        ;(setvar "osmode" osmode)              ;恢复对象捕获
  )

(defun a4y-kuang  (p0 scale  )
                                        ;给出左下角点和比例，画出A4竖幅图幅框
                                        ;(setq osmode (getvar "osmode"))
                                        ;(setvar "osmode" 0)                   ;设置对象捕获为0

  (command "insert" "paper-a4y" p0 (rtos scale 2) "0" (rtos scale 2)) ;插入图框块
  (command "style"                      ;建立文字样式
           (strcat "s-" (rtos scale 2 1))
           "gbcbig.shx"
           (rtos (* 3 scale) 2 1)
           ""
           ""
           ""
           ""
           "")
  (biao-ti-lan (xy-m p0 180 0 scale) scale)
  (setvar "celweight" 13)
;;;  (command "text"
;;;           (xy-m p0 32 32 scale)
;;;           ""
;;;           "首件必检！中间抽查！首件必检！")
  
  (tufu-dim scale)                      ;设置标注样式
                                        ;(setvar "osmode" osmode)              ;恢复对象捕获
  )

(defun a3x-kuang  (p0 scale  )
                                        ;给出左下角点和比例，画出A3横幅图幅框
                                        ;(setq osmode (getvar "osmode"))
                                        ; (setvar "osmode" 0)                   ;设置对象捕获为0

  (command "insert" "paper-a3x" p0 (rtos scale 2) "0" (rtos scale 2)) ;插入图框块
  (command "style"                      ;建立文字样式
           (strcat "s-" (rtos scale 2 1))
           "gbcbig.shx"
           (rtos (* 3 scale) 2 1)
           ""
           ""
           ""
           ""
           "")
  (biao-ti-lan (xy-m p0 390 0 scale) scale)
  (setvar "celweight" 13)
;;;    (command "text"
;;;           (xy-m p0- 242 32 scale)
;;;           ""
;;;           "首件必检！中间抽查！首件必检！")
  (tufu-dim scale)                      ;设置标注样式
                                        ;(setvar "osmode" osmode)              ;恢复对象捕获
  )

(defun tufu-dim  (scale)
  ;;设置标注样式
  ;;scale 打印比例 
  (command "dimstyle" "r" "iso-25-瘦")     ;恢复iso-25-瘦标注样式，以此为基准
  (setvar "dimtxt" 2.5)                 ;字体标准高度
  (setvar "dimtxsty" "Annotative")      ;字体名称
  (setvar "dimscale" scale)             ;全局比例因子
  (command "dimstyle" "s" (strcat "s-" (rtos scale 2 1)))) ;保存设置


(defun biao-ti-lan (p1 scale) ;给出右下角点和缩放比例scale，画出标题栏。标题栏块预存在图纸中
  (command "insert" "标题栏" p1 scale "0" "图名" "数量" "材料" "图号" "" ""
	   ""))


