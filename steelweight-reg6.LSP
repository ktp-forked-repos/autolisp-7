;;;增加了槽钢
(defun weight (density guige-list / form z-length num)
                                        ;接受密度和一个规格表 (型材类型代号 规格参数... )
                                        ;计算重量
  (setq form (car guige-list))          ;型材代号
  (cond
    ((= form 'o)                        ; 空心圆环 (o 32 2 length) 
     (* (/ pi 4)
        4
        (nth 2 guige-list)
        (- (nth 1 guige-list) (nth 2 guige-list))
        (nth 3 guige-list)
        density
        )
     )
    ((= form 'q)                        ;实心圆棒 (q 32 length)
     (* (/ pi 4)
        (nth 1 guige-list)
        (nth 1 guige-list)
        (nth 2 guige-list)
        density
        )
     )
    ((= form 'd)                        ;矩形管 (d 50 30 2 length)
     (* 2
        (nth 3 guige-list)
        (+ (nth 1 guige-list) (nth 2 guige-list) (* -2 (nth 3 guige-list)))
        (nth 4 guige-list)
        density
        )
     )
    ((= form 'j)                        ;几字钢 （j 50 length)
     (cond ((= (nth 1 guige-list) '38) (* (nth 2 guige-list) 1.6e-3))
                                        ;展开 130 ，1.03镀锌系数
           ((= (nth 1 guige-list) '50) (* (nth 2 guige-list) 1.94e-3))
                                        ;展开 160
           ((= (nth 1 guige-list) '70) (* (nth 2 guige-list) 2.6e-3))
           )
     )                                  ;展开 214
    ((= form 'b
	)                        ;板状物(i 边长 边长 厚度)
     (* (nth 1 guige-list) (nth 2 guige-list) (nth 3 guige-list) density)
     )
    ((= form 'z)                        ; 多边折弯件 (z 边长1 边长2 ... 厚度 延长)
     (setq z-length 0                   ;封边各边长总长
           num 1
           )
     (while (<= num (- (length guige-list) 3))
       (setq z-length (+ (nth num guige-list) z-length))
       (setq num (1+ num))
       )
     (* density
        z-length
        (nth num guige-list)            ;厚度
        (nth (1+ num) guige-list)       ;延长
        )
     )
    ((= form 'v)                        ;角钢 （v 3 length)
     (cond ((= (nth 1 guige-list) '3) (* (nth 2 guige-list) 1.09e-3))
                                        ;3号角钢6.5kg/6m
           ((= (nth 1 guige-list) '4) (* (nth 2 guige-list) 1.75e-3))
                                        ;4号角钢10.5kg/6m
           ((= (nth 1 guige-list) '5) (* (nth 2 guige-list) 3.77e-3))
                                        ;5号角钢22.62kg/6m
           )
     )
    ((= form 'u)                        ;槽钢 （u 8 length)
     (cond ((= (nth 1 guige-list) '8) (* (nth 2 guige-list) 8.03e-3))
                                        ;8号角钢8.03kg/m
           ((= (nth 1 guige-list) '10) (* (nth 2 guige-list) 0.01))
                                        ;10号角钢10kg/m
           
                                   
           )
     )


    
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;正则表达式

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


(defun regex-test (pat str key / regex test)
  ;; 测试字符串str是否存在字串符合正则表达式模式pat
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
  (setq test (if (eq (vlax-invoke-method regex "test" str) :vlax-true)
               t
               nil
               )
        )
  (vlax-release-object regex)
  test
  )

(defun regex-replace (pat str str1 key / regex S str2)
  ;; pat 正则表达式 str 字符串 str1 替换的字符串
  ;; pat 中 \ 使用 \\
  ;; key "igm" i(Ignorecase)忽略大小写 g (Global)全局匹配 m (Multili) 多行模式
  ;; 注意：一般使用全局匹配 g
  ;; 可组合使用或单独使用 或置空 ""
  ;; 返回替换后的字符串
  (vl-load-com)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;引用正则表达式控件
  (if (wcmatch key "*i*,*I*")
    (vlax-put-property regex "IgnoreCase" 1) ;忽略大小写
    (vlax-put-property regex "IgnoreCase" 0) ;不忽略大小写
    )
  (if (wcmatch key "*g*,*G*")
    (vlax-put-property regex "Global" 1) ;匹配方式，全文字匹配
    (vlax-put-property regex "Global" 0) ;只检查第一处出现的位置
    )
  (if (wcmatch key "*m*,*M*")
    (vlax-put-property regex "Multiline" 1) ;多行模式
    (vlax-put-property regex "Multiline" 0) ;单行模式
    )
  (vlax-put-property regex "Pattern" pat)
  (setq STR2 (vlax-invoke-method regex "Replace" STR STR1))
  (vlax-release-object regex)
  STR2
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-str (str)                   ;判断str是否符合特定模式
  (cond ((regex-test "^\\d+-o\\d+[*x]\\d+\\.?\\d*[*lx]\\d+$" str "ig") t)
                                        ;1-o32x1.8l2000
        ((regex-test "^\\d+-q\\d+\\.?\\d*[x*l]\\d+$" str "ig") t) ;24-q16l500
        ((regex-test "^\\d+-d\\d+[*x]\\d+[*x]\\d+\\.?\\d*[lx*]\\d+$" str "ig") t)
                                        ;2-d50x50x2.5l1000
        ((regex-test "^\\d+-j(38|50|70)[x*l]\\d+$" str "ig") t)
                                        ;1-j38|50|70l1000
        ((regex-test "^\\d+-b\\d+[*x]\\d+[*xl]\\d+\\.?\\d*$" str "ig") t) ;1-b100x100x7.5
        ((regex-test "^\\d+-z\\d+([*x]\\d+)+[x*l]\\d+$" str "ig") t)
                                        ;2-z10x10x20x2l1000
        ((regex-test "^\\d+-v[345][x*l]\\d+$" str "ig") t) ;24-v3|4|5l500
        ((regex-test "^\\d+-u(8|10)[x*l]\\d+$" str "ig") t) ;24-u8|10l500
        ((= str "1-r") t)               ;重算算式
        ((= str "1-e") t)               ;退出计算，写入结果
        (t nil)
        )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(defun parsel (str delim / LST POS)
  ;;字符串分割 delim是用来分割的字符
  (setq lst nil)
  (while (setq pos (vl-string-search delim str))
    (setq lst (cons (substr str 1 pos) lst)
          str (substr str (+ pos 2))
          )
    )
  (if (> (strlen str) 0)
    (setq lst (cons str lst))
    )
  (reverse lst)
  )


(defun add1- (str)                      ;辅助函数 如果字符串中不含"-"，则在开头添加"1-"
  (if (not (vl-string-position (ascii "-") str))
    (strcat "1-" str)
    str
    )
  )

(defun str->weight
       (str-n / density str->list number guige-list str-weight canshu form)
                                        ;从字符串计算重量列表
                                        ;返回一个列表 (字符串：数量-类型-单重-总重 总重）
  (setq density 7.85e-6)                ;钢材密度
  (setq str->list
         (read
           (strcat
             "("                        ;将字符串str-n分解成各参数的列表
             (vl-string-translate
               "-xXLl*"                  ;参数分割字符集 -xl
               "      "
               (strcat
                 (substr str-n
                         1
                         (+ 2 (vl-string-position (ascii "-") str-n))
                         )
                 "-"
                 (substr str-n
                         (+ 3 (vl-string-position (ascii "-") str-n))
                         )
                 )
               )
             ")"
             )
           )
        )
  (setq number     (car str->list)      ; 该类型钢材数量
        guige-list (cdr str->list)      ;该类型钢材重量计算的规格表
        str-weight (weight density guige-list) ;钢材重量
        canshu     (strcase
                     (substr str-n (+ 3 (vl-string-position (ascii "-") str-n)))
                     )
        )                               ;规格表转换大写
  (setq form (cadr str->list))
  (cond ((= form 'o) (setq form "圆钢管%%C"))
        ((= form 'q) (setq form "圆钢%%C"))
        ((= form 'd) (setq form "钢矩管D"))
        ((= form 'j) (setq form "几字钢J"))
        ((= form 'b) (setq form "钢板B"))
        ((= form 'z) (setq form "折弯件Z"))
        ((= form 'v) (setq form "角钢V"))
        ((= form 'u) (setq form "槽钢U"))
        )
  (if (not (= form "钢板B"))
    (setq canshu (regex-replace "[*X](\\d+)$" canshu  "L$1" "ig"))) ;对除钢板之外的其他型材，长度前的分隔符改为L
  (list (strcat (rtos (nth 0 str->list))
                "个"
                form
                canshu
                ",单重："
                (rtos str-weight 2 3)
                "kg"
                ",总重： "
                (rtos (* str-weight number) 2 3)
                "kg。"
                )
        (* str-weight number)
        )
  )


(defun c:sw (/             str           weight-sum    num
             str-str-sum   point         text-size     text-edit
             vlax-string   string        text-weight   str->weight-list
             )
  (princ "语法：[<number>-]<form><n>x<n>...[l<length>](x或l可用*代替)")
  (princ
    "\n<number>数量；<form>类型：实心圆棒q 空心圆环o 方管d 几字钢j 钢板b 折弯件z 角钢v <length>长度"
    )
  (princ
    "\n例子：实心圆棒q16l500  空心圆环2-o32x1.8l1000 方管3-d50x50x2l1500 \n
    几字钢1-j38|50|70l1000 钢板b-100x100x8 折弯件z30x30x2l1000 角钢v3|4|5l500"
    )
  (setq weight-sum 0
        str-str-sum ""
        )
  (while (progn (while (not (test-str (setq str
                                             (add1-
                                               (getstring
                                                 "\n输入算式|e|r [圆管O|圆钢Q|矩管D|几字钢J|钢板B|折弯Z|角钢V][e写入算式][r重算算式]："
                                                 )
                                               )
                                            )
                                      )
                            )
                  (princ "\n输入语法有误，请重新输入")
                  )
                (if (not (= str "1-e"))
                  t
                  )
                )
    (if (not (= str "1-r"))
      (progn (setq str->weight-list (str->weight str))
             (setq weight-sum  (+ weight-sum (cadr str->weight-list))
                   str-str-sum (strcat str-str-sum (car str->weight-list) "\\P")
                   )
             (princ (strcat (car str->weight-list)
                            "=>以上重量加总"
                            (rtos weight-sum 2 3)
                            "kg"
                            )
                    )
             )
      )
    (if (= str "1-r")
      (progn
        (setq text-edit t)              ;设置标志位，重新编辑已有算式
        (VL-LOAD-COM)
        (setq vlax-string
               (vlax-ename->vla-object (car (entsel "请选择算式:")))
              )                         ;已有文本的vlax对象
        (setq string (vlax-get-property vlax-string "TextString"))
        (defun string-trim (string / str s tmp) ;字符串重新格式化函数
          (setq str (regex-extract "(^|\\\\P)[^,]+(?=,)" string "igm"))
          (defun trim (str)
            (setq str (vl-string-subst "" "\\P" str))
            (setq str (vl-string-subst "-o" "个圆钢管%%C" str))
            (setq str (vl-string-subst "-d" "个钢矩管D" str))
            (setq str (vl-string-subst "-b" "个钢板B" str))
            (setq str (vl-string-subst "-j" "个几字钢J" str))
            (setq str (vl-string-subst "-z" "个折弯件Z" str))
            (setq str (vl-string-subst "-v" "个角钢V" str))
            (setq str (vl-string-subst "-u" "个槽钢U" str))
            (setq str (vl-string-subst "-q" "个圆钢%%C" str))
            )
          (setq tmp nil)
          (foreach s str (setq tmp (cons (trim s) tmp)))
          (reverse tmp)
          )
        (setq string (string-trim string))
        (setq text-weight (mapcar 'str->weight string))
        (setq num 0
              weight-sum 0
              )
        (repeat (length text-weight)    ;计算已有算式的总重
          (setq weight-sum (+ (cadr (nth num text-weight)) weight-sum))
          (setq num (1+ num))
          )
        (setq str-str-sum "")           ;计算已有算式的字符串说明汇总
        (repeat (length text-weight)
          (setq str-str-sum (strcat str-str-sum (caar text-weight) "\\P"))
          (setq text-weight (cdr text-weight))
          )
        (princ str-str-sum)
        (princ (strcat "=>以上重量加总" (rtos weight-sum 2 3) "kg"))
        )
      )
    )
  (if text-edit
    (progn (setq str-str-sum
                  (strcat str-str-sum
                          "----------------\\P总重："
                          (rtos weight-sum 2 3)
                          "kg"
                          )
                 )
           (vlax-put-property vlax-string "TextString" str-str-sum)
           (vlax-object-released-p vlax-string)
           (setq text-edit nil)
           )
    (progn (setq str-str-sum
                  (strcat str-str-sum
                          "----------------\\P总重："
                          (rtos weight-sum 2 3)
                          "kg"
                          )
                 )
           (setq point (getpoint "输入文字起始位置"))
           (setq text-size              ;当前字体高度
                  (cdr (assoc 40 (entget (tblobjname "style" (getvar "textstyle")))))
                 )
           (command "mtext"
                    point
                    (polar point -0.1 (* 45 text-size))
                                        ; 设置多行文字宽度 字体高度的45倍
                    str-str-sum
                    ""
                    )
           )
    )
  )


