;;三轴钻生成cnc代码
;;2016-07-19第1版 完成简易功能
;;第4版 写子程序代码单独提出来，作为一个函数
;;2016-07-26 V4.1 根据A轴是否有孔，写0005、0006子程序
;;2016-07-26 V4.2 修正了一个bug，开关错误。设立了四个子程序的标志开关。

(defun make-axis-pair (dist-list / first-name zhou-pair reverse-dist-list)
                                        ;由表(A 20 30 40 50)格式，生成表
                                        ;((A . 20) (A . 30) (A . 40) (A . 50))
  (setq first-name        (car dist-list)
        reverse-dist-list (reverse (cdr dist-list))
        )
  (setq axis-pair nil)
  (while reverse-dist-list
    (setq axis-pair         (cons (cons first-name (car reverse-dist-list)) axis-pair)
          reverse-dist-list (cdr reverse-dist-list)
          )
    )
  axis-pair
  )

(defun A>y? (a y)                       ; 判断两个点对a和y的第二个元素大小
  (> (cdr a) (cdr y))
  )

(defun yz-drilling (ff p6l1)            ;yz轴钻孔子程序
                                        ;ff 文件指针
                                        ;p6l1 为真，则内夹头不工作，子程序段为 O0003,否则为 O0002
  (if p6l1
    (write-line "O0003" ff)             ;0003子程序名，内夹头不工作
    (progn (write-line "O0002" ff)      ;0002子程序名，内夹头工作
           (write-line "M89 P6 L1" ff)  ;加紧内夹头
           )
    )
  (write-line "M89 P2 L1" ff)           ;加紧外夹头
  (write-line "G04 P200" ff)            ;暂停200毫秒
  (write-line "G01 Y80 Z80 F6000" ff)   ;钻头快速进给到 Y轴76 Z轴89的位置 []实际数值需要根据钻头长度调整
  (write-line "G01 Y85 Z85 F100" ff)    ;钻头钻孔，慢速进给
  (write-line "G01 Y75 Z75 F6000" ff)   ;钻完，快速退出
  (if (not p6l1)
    (write-line "M89 P6 L0" ff)         ;是否松内夹头
    )                                   ;松内夹头
  (write-line "M89 P2 L0" ff)           ;松外夹头
  (write-line "G04 P1000" ff)           ;暂停
  (write-line "M99" ff)                 ;返回主程序
  (write-line "%" ff)
  )                                     ;子程序结束


(defun a-drilling (ff p6l1)             ;a轴钻孔子程序
                                        ;ff 文件指针
                                        ;p6l1 为真，则内夹头不工作，子程序段为 O0006,否则为 O0005
  (if p6l1
    (write-line "O0006" ff)             ;O0006子程序名，内夹头不工作
    (progn (write-line "O0005" ff)      ;0005子程序名，内夹头工作
           (write-line "M89 P6 L1" ff)  ;加紧内夹头
           )
    )
  (write-line "M89 P2 L1" ff)           ;夹紧外夹头
  (write-line "G04 P200" ff)
  (write-line "G01 A35 F6000" ff)       ;到上面
  (write-line "G01 A40 F100" ff)        ;钻孔
  (write-line "G01 A85 F6000" ff)       ;到下表面
  (write-line "G01 A90 F100" ff)        ;钻孔
  (write-line "G01 A30 F6000" ff)       ;退回
  (if (not p6l1)
    (write-line "M89 P6 L0" ff)         ;松内夹头
    )
  (write-line "M89 P2 L0" ff)           ;松外夹头
  (write-line "G04 P1000" ff)
  (write-line "M99" ff)
  (write-line "%" ff)
  )

(defun c:szz (/          det-x      det-yz-a   point      yz-point-list
              n          a-point-list          yz-dist-list
              a-dist-list           x-axis     a          yz
              yz-a-x     ffd        ff         osmode     
              p0002      p0003      p0005      p0006
              )
;;;;主程序
                                        ;参数，需根据实际情况调整
  (setq det-x 18                        ;机械归零后， yz轴距管材边距离
        det-yz-a 112                    ;yz轴和A轴水平距离
        x-limit 120                     ;X轴限位距离，小于此数，钻孔时内夹头不工作
        )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq point (getpoint "指定坐标原点")) ;设定管材的边为原点
  (command "ucs" point)
  (command "")
  (setq osmode (getvar "osmode"))       ;对象捕获
  (setvar "osmode" 36)                  ;只捕获圆心和交点
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;处理YZ轴  
  (setq yz-point-list nil)              ;YZ轴钻孔位置存储列表
  (setq point         (getpoint "请输入YZ轴要钻孔的第一个圆心位置")
        yz-point-list (cons point nil)
        n             2
        )
  (while (setq point (getpoint point
                               (strcat "\n请输入YZ轴第"
                                       (rtos n 2)
                                       "个要钻孔的圆心位置（结束请按ENT）:"
                                       )
                               )
               )
    (setq yz-point-list
           (cons point yz-point-list)
          n (1+ n)
          )
    )
  (setq yz-dist-list (mapcar 'car yz-point-list)) ;提取YZ轴钻孔X值
  (setq yz (cons 'yz (mapcar '(lambda (x) (- x det-x)) yz-dist-list)))
                                        ;YZ轴X值变换到机械原点
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;处理A轴
  (setq a-point-list nil                ;A轴钻孔位置存储列表
        point        (getpoint "请输入A轴要钻孔的第一个圆心位置(取消请按ENT)")
        )
  (if point                             ;如果有A轴钻孔
    (progn
      (setq a-point-list
             (cons point nil)
            n 2
            )
      (while (setq
               point (getpoint point
                               (strcat "\n请输入A轴第"
                                       (rtos n 2)
                                       "个要钻孔的圆心位置（结束请按ENT）:"
                                       )
                               )
               )
        (setq a-point-list
               (cons point a-point-list)
              n (1+ n)
              )
        )
      (setq a-dist-list (mapcar 'car a-point-list))
                                        ;如果有A轴钻孔提取A轴钻孔x值
      (princ "\nA轴距管边距离：")
      (princ a-dist-list)
      (setq a
             (cons 'A (mapcar '(lambda (x) (- x det-x det-yz-a)) a-dist-list))
            )                           ;A轴X值变换到机械原点
      )
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;A轴处理结束
  (setvar "osmode" osmode)              ;恢复原有对象捕获
  (princ "\nYZ轴距管边距离：")
  (princ yz-dist-list)
  (princ "\n")
  (setq yz-a-x
         (vl-sort (append (make-axis-pair a) (make-axis-pair yz)) 'a>y?)
        )                               ;yz轴与A轴统一排序,大的在前
  (princ yz-a-x)
  (setq p0002 nil                       ;O0002子程序标志位,为真则写
        p0003 nil                       ;O0003子程序标志位
        p0005 nil                       ;O0005子程序标志位
        p0006 nil                       ;O0006子程序标志位
        )                               ;恢复默认值
  (setq ffd (getfiled "写入钻孔程序文件" "" "cnc" 1))
  (setq ff (open ffd "w"))              ;文件指针
  (write-line "O0001" ff)               ;程序头
  (write-line "M89 P3 L1" ff)           ;夹嘴夹紧
  (write-line "G04 P500" ff)            ;暂停500毫秒
  (while (car yz-a-x)
    (if (= 'A (caar yz-a-x))            ;如果是A轴钻孔
      (progn (write-line (strcat "G00 X" (rtos (cdar yz-a-x) 2)) ff)
             (if (> (cdar yz-a-x) x-limit)
               (progn (write-line "M98 P0005" ff) ;调用0005钻孔子程序
                      (setq p0005 t)
                      )
               (progn (write-line "M98 P0006" ff) ;调用0006钻孔子程序
                      (setq p0006 t)
                      )
               )
             )
      (progn (write-line (strcat "G00 X" (rtos (cdar yz-a-x) 2)) ff)
                                        ;YZ轴钻孔
             (if (> (cdar yz-a-x) x-limit)
               (progn (write-line "M98 P0002" ff) ;调用0002钻孔子程序
                      (setq p0002 t)
                      )
               (progn (write-line "M98 P0003" ff) ;调用0006钻孔子程序
                      (setq p0003 t)
                      )
               )
             )
      )
    (setq yz-a-x (cdr yz-a-x))
    )
  (write-line "M89 P3 L0" ff)           ;松加嘴
  (write-line "G04 P500" ff)            ;暂停0.5秒
  (write-line "M30" ff)                 ;循环返回开始
  (write-line "%" ff)                   ;结束
  (if p0002                             ;写O0002子程序
    (yz-drilling ff nil)
    )
  (if p0003                             ;写O0003子程序
    (yz-drilling ff t)
    )
  (if p0005                             ;写O0005子程序
    (a-drilling ff nil)
    )
  (if p0006
    (a-drilling ff t)
    )                                   ;写O0006子程序
  (close ff)                            ;关闭指针
  (princ)
  )