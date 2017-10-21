;;;�����˲۸�
(defun weight (density guige-list / form z-length num)
                                        ;�����ܶȺ�һ������ (�Ͳ����ʹ��� ������... )
                                        ;��������
  (setq form (car guige-list))          ;�ͲĴ���
  (cond
    ((= form 'o)                        ; ����Բ�� (o 32 2 length) 
     (* (/ pi 4)
        4
        (nth 2 guige-list)
        (- (nth 1 guige-list) (nth 2 guige-list))
        (nth 3 guige-list)
        density
        )
     )
    ((= form 'q)                        ;ʵ��Բ�� (q 32 length)
     (* (/ pi 4)
        (nth 1 guige-list)
        (nth 1 guige-list)
        (nth 2 guige-list)
        density
        )
     )
    ((= form 'd)                        ;���ι� (d 50 30 2 length)
     (* 2
        (nth 3 guige-list)
        (+ (nth 1 guige-list) (nth 2 guige-list) (* -2 (nth 3 guige-list)))
        (nth 4 guige-list)
        density
        )
     )
    ((= form 'j)                        ;���ָ� ��j 50 length)
     (cond ((= (nth 1 guige-list) '38) (* (nth 2 guige-list) 1.6e-3))
                                        ;չ�� 130 ��1.03��пϵ��
           ((= (nth 1 guige-list) '50) (* (nth 2 guige-list) 1.94e-3))
                                        ;չ�� 160
           ((= (nth 1 guige-list) '70) (* (nth 2 guige-list) 2.6e-3))
           )
     )                                  ;չ�� 214
    ((= form 'b
	)                        ;��״��(i �߳� �߳� ���)
     (* (nth 1 guige-list) (nth 2 guige-list) (nth 3 guige-list) density)
     )
    ((= form 'z)                        ; �������� (z �߳�1 �߳�2 ... ��� �ӳ�)
     (setq z-length 0                   ;��߸��߳��ܳ�
           num 1
           )
     (while (<= num (- (length guige-list) 3))
       (setq z-length (+ (nth num guige-list) z-length))
       (setq num (1+ num))
       )
     (* density
        z-length
        (nth num guige-list)            ;���
        (nth (1+ num) guige-list)       ;�ӳ�
        )
     )
    ((= form 'v)                        ;�Ǹ� ��v 3 length)
     (cond ((= (nth 1 guige-list) '3) (* (nth 2 guige-list) 1.09e-3))
                                        ;3�ŽǸ�6.5kg/6m
           ((= (nth 1 guige-list) '4) (* (nth 2 guige-list) 1.75e-3))
                                        ;4�ŽǸ�10.5kg/6m
           ((= (nth 1 guige-list) '5) (* (nth 2 guige-list) 3.77e-3))
                                        ;5�ŽǸ�22.62kg/6m
           )
     )
    ((= form 'u)                        ;�۸� ��u 8 length)
     (cond ((= (nth 1 guige-list) '8) (* (nth 2 guige-list) 8.03e-3))
                                        ;8�ŽǸ�8.03kg/m
           ((= (nth 1 guige-list) '10) (* (nth 2 guige-list) 0.01))
                                        ;10�ŽǸ�10kg/m
           
                                   
           )
     )


    
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;�������ʽ

(defun regex-extract (pat str key / regex S tmp str1)
  ;; ��ȡ�������ʽƥ�䵽������
  ;; pat �������ʽ str �ַ��� 
  ;; pat �� \ ʹ�� \\
  ;; key "igm" i(Ignorecase)���Դ�Сд g (Global)ȫ��ƥ�� m (Multili) ����ģʽ
  ;; ע�⣺һ��ʹ��ȫ��ƥ�� g
  ;; �����ʹ�û򵥶�ʹ�� ���ÿ� ""
  (vl-load-com)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;�����������ʽ�ؼ�
  (if (wcmatch key "*i*,*I*")
    (vlax-put-property regex "IgnoreCase" 1) ;���Դ�Сд
    (vlax-put-property regex "IgnoreCase" 0) ;�����Դ�Сд
    )
  (if (wcmatch key "*g*,*G*")
    (vlax-put-property regex "Global" 1) ;ƥ�䷽ʽ��ȫ����ƥ��
    (vlax-put-property regex "Global" 0)
    )
  (if (wcmatch key "*m*,*M*")
    (vlax-put-property regex "Multiline" 1) ;����ģʽ
    (vlax-put-property regex "Multiline" 0)
    )
  (vlax-put-property regex "Pattern" pat)
  (setq s (vlax-invoke-method regex "Execute" str))
  ;;���������õ�STR�ַ����õ���ȡ������������
  (VLAX-FOR tmp s                       ;�������϶���
    (setq str1 (cons (vlax-get-property tmp "value") str1))
    )
  ;;������ת��ΪLISP���ԾͿ���ֱ�ӹ۲���
  (vlax-release-object regex)
  (REVERSE str1)
  )


(defun regex-test (pat str key / regex test)
  ;; �����ַ���str�Ƿ�����ִ������������ʽģʽpat
  ;; pat �������ʽ str �ַ���
  ;; pat �� \ ʹ�� \\
  ;; key "igm" i(Ignorecase)���Դ�Сд g (Global)ȫ��ƥ�� m (Multili) ����ģʽ
  ;; ע�⣺һ��ʹ��ȫ��ƥ�� g
  ;; �����ʹ�û򵥶�ʹ�� ���ÿ� ""
  (vl-load-com)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;�����������ʽ�ؼ�
  (if (wcmatch key "*i*,*I*")
    (vlax-put-property regex "IgnoreCase" 1) ;���Դ�Сд
    (vlax-put-property regex "IgnoreCase" 0) ;�����Դ�Сд
    )
  (if (wcmatch key "*g*,*G*")
    (vlax-put-property regex "Global" 1) ;ƥ�䷽ʽ��ȫ����ƥ��
    (vlax-put-property regex "Global" 0)
    )
  (if (wcmatch key "*m*,*M*")
    (vlax-put-property regex "Multiline" 1) ;����ģʽ
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
  ;; pat �������ʽ str �ַ��� str1 �滻���ַ���
  ;; pat �� \ ʹ�� \\
  ;; key "igm" i(Ignorecase)���Դ�Сд g (Global)ȫ��ƥ�� m (Multili) ����ģʽ
  ;; ע�⣺һ��ʹ��ȫ��ƥ�� g
  ;; �����ʹ�û򵥶�ʹ�� ���ÿ� ""
  ;; �����滻����ַ���
  (vl-load-com)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;�����������ʽ�ؼ�
  (if (wcmatch key "*i*,*I*")
    (vlax-put-property regex "IgnoreCase" 1) ;���Դ�Сд
    (vlax-put-property regex "IgnoreCase" 0) ;�����Դ�Сд
    )
  (if (wcmatch key "*g*,*G*")
    (vlax-put-property regex "Global" 1) ;ƥ�䷽ʽ��ȫ����ƥ��
    (vlax-put-property regex "Global" 0) ;ֻ����һ�����ֵ�λ��
    )
  (if (wcmatch key "*m*,*M*")
    (vlax-put-property regex "Multiline" 1) ;����ģʽ
    (vlax-put-property regex "Multiline" 0) ;����ģʽ
    )
  (vlax-put-property regex "Pattern" pat)
  (setq STR2 (vlax-invoke-method regex "Replace" STR STR1))
  (vlax-release-object regex)
  STR2
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-str (str)                   ;�ж�str�Ƿ�����ض�ģʽ
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
        ((= str "1-r") t)               ;������ʽ
        ((= str "1-e") t)               ;�˳����㣬д����
        (t nil)
        )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(defun parsel (str delim / LST POS)
  ;;�ַ����ָ� delim�������ָ���ַ�
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


(defun add1- (str)                      ;�������� ����ַ����в���"-"�����ڿ�ͷ����"1-"
  (if (not (vl-string-position (ascii "-") str))
    (strcat "1-" str)
    str
    )
  )

(defun str->weight
       (str-n / density str->list number guige-list str-weight canshu form)
                                        ;���ַ������������б�
                                        ;����һ���б� (�ַ���������-����-����-���� ���أ�
  (setq density 7.85e-6)                ;�ֲ��ܶ�
  (setq str->list
         (read
           (strcat
             "("                        ;���ַ���str-n�ֽ�ɸ��������б�
             (vl-string-translate
               "-xXLl*"                  ;�����ָ��ַ��� -xl
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
  (setq number     (car str->list)      ; �����͸ֲ�����
        guige-list (cdr str->list)      ;�����͸ֲ���������Ĺ���
        str-weight (weight density guige-list) ;�ֲ�����
        canshu     (strcase
                     (substr str-n (+ 3 (vl-string-position (ascii "-") str-n)))
                     )
        )                               ;����ת����д
  (setq form (cadr str->list))
  (cond ((= form 'o) (setq form "Բ�ֹ�%%C"))
        ((= form 'q) (setq form "Բ��%%C"))
        ((= form 'd) (setq form "�־ع�D"))
        ((= form 'j) (setq form "���ָ�J"))
        ((= form 'b) (setq form "�ְ�B"))
        ((= form 'z) (setq form "�����Z"))
        ((= form 'v) (setq form "�Ǹ�V"))
        ((= form 'u) (setq form "�۸�U"))
        )
  (if (not (= form "�ְ�B"))
    (setq canshu (regex-replace "[*X](\\d+)$" canshu  "L$1" "ig"))) ;�Գ��ְ�֮��������Ͳģ�����ǰ�ķָ�����ΪL
  (list (strcat (rtos (nth 0 str->list))
                "��"
                form
                canshu
                ",���أ�"
                (rtos str-weight 2 3)
                "kg"
                ",���أ� "
                (rtos (* str-weight number) 2 3)
                "kg��"
                )
        (* str-weight number)
        )
  )


(defun c:sw (/             str           weight-sum    num
             str-str-sum   point         text-size     text-edit
             vlax-string   string        text-weight   str->weight-list
             )
  (princ "�﷨��[<number>-]<form><n>x<n>...[l<length>](x��l����*����)")
  (princ
    "\n<number>������<form>���ͣ�ʵ��Բ��q ����Բ��o ����d ���ָ�j �ְ�b �����z �Ǹ�v <length>����"
    )
  (princ
    "\n���ӣ�ʵ��Բ��q16l500  ����Բ��2-o32x1.8l1000 ����3-d50x50x2l1500 \n
    ���ָ�1-j38|50|70l1000 �ְ�b-100x100x8 �����z30x30x2l1000 �Ǹ�v3|4|5l500"
    )
  (setq weight-sum 0
        str-str-sum ""
        )
  (while (progn (while (not (test-str (setq str
                                             (add1-
                                               (getstring
                                                 "\n������ʽ|e|r [Բ��O|Բ��Q|�ع�D|���ָ�J|�ְ�B|����Z|�Ǹ�V][eд����ʽ][r������ʽ]��"
                                                 )
                                               )
                                            )
                                      )
                            )
                  (princ "\n�����﷨��������������")
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
                            "=>������������"
                            (rtos weight-sum 2 3)
                            "kg"
                            )
                    )
             )
      )
    (if (= str "1-r")
      (progn
        (setq text-edit t)              ;���ñ�־λ�����±༭������ʽ
        (VL-LOAD-COM)
        (setq vlax-string
               (vlax-ename->vla-object (car (entsel "��ѡ����ʽ:")))
              )                         ;�����ı���vlax����
        (setq string (vlax-get-property vlax-string "TextString"))
        (defun string-trim (string / str s tmp) ;�ַ������¸�ʽ������
          (setq str (regex-extract "(^|\\\\P)[^,]+(?=,)" string "igm"))
          (defun trim (str)
            (setq str (vl-string-subst "" "\\P" str))
            (setq str (vl-string-subst "-o" "��Բ�ֹ�%%C" str))
            (setq str (vl-string-subst "-d" "���־ع�D" str))
            (setq str (vl-string-subst "-b" "���ְ�B" str))
            (setq str (vl-string-subst "-j" "�����ָ�J" str))
            (setq str (vl-string-subst "-z" "�������Z" str))
            (setq str (vl-string-subst "-v" "���Ǹ�V" str))
            (setq str (vl-string-subst "-u" "���۸�U" str))
            (setq str (vl-string-subst "-q" "��Բ��%%C" str))
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
        (repeat (length text-weight)    ;����������ʽ������
          (setq weight-sum (+ (cadr (nth num text-weight)) weight-sum))
          (setq num (1+ num))
          )
        (setq str-str-sum "")           ;����������ʽ���ַ���˵������
        (repeat (length text-weight)
          (setq str-str-sum (strcat str-str-sum (caar text-weight) "\\P"))
          (setq text-weight (cdr text-weight))
          )
        (princ str-str-sum)
        (princ (strcat "=>������������" (rtos weight-sum 2 3) "kg"))
        )
      )
    )
  (if text-edit
    (progn (setq str-str-sum
                  (strcat str-str-sum
                          "----------------\\P���أ�"
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
                          "----------------\\P���أ�"
                          (rtos weight-sum 2 3)
                          "kg"
                          )
                 )
           (setq point (getpoint "����������ʼλ��"))
           (setq text-size              ;��ǰ����߶�
                  (cdr (assoc 40 (entget (tblobjname "style" (getvar "textstyle")))))
                 )
           (command "mtext"
                    point
                    (polar point -0.1 (* 45 text-size))
                                        ; ���ö������ֿ��� ����߶ȵ�45��
                    str-str-sum
                    ""
                    )
           )
    )
  )

