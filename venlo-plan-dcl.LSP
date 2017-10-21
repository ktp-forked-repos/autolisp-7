(defun c:venlo-p ()
;;;��������ʽ��������ƽ������ͼ
  (defun getdata ()
    (setq kuadu (get_tile "kuadu")
	  kaijian (get_tile "kaijian")
	  jiangao (get_tile "jiangao")
	  duan_lizhu (get_tile "duan_lizhu")
	  bian_lizhu (get_tile "bian_lizhu")
	  p0_x (atof (get_tile "p0_x"))
	  p0_y (atof (get_tile "p0_y"))
    ))
  
  (setq id (load_dialog "venlo-p.dcl"))
  (if (< id 0) (exit))
  (setq std 2
	kuadu "(10.8 5)"
	kaijian "(4 10)"
	jiangao "4"
	duan_lizhu "1"
	bian_lizhu "0"
	p0_x 0 p0_y 0)
  (while (> std 1)
    (if (not (new_dialog "venlo_plan" id)) (exit))
    (set_tile "kuadu" kuadu)
    (set_tile "kaijian" kaijian)
    (set_tile "jiangao" jiangao)
    (set_tile "duan_lizhu" duan_lizhu)
    (set_tile "bian_lizhu" bian_lizhu)
    (set_tile "p0_x" (rtos p0_x 2 2))
    (set_tile "p0_y" (rtos p0_y 2 2))
    (action_tile "pick"  "(getdata) (done_dialog 2)")
    (action_tile "accept" "(getdata) (done_dialog 1)")
    (action_tile "cancel" "(done_dialog 0)")
    (setq std (start_dialog))
    (if (= std 2)
      (progn
	(initget 1)
	(setq p (getpoint "�������¶�λ�㣺"))
	(setq p0_x (car p) p0_y (cadr p))))
    )
  (if  (= std 1)
    (progn
      (venlo-plan kuadu kaijian duan_lizhu
	bian_lizhu p0_x p0_y))
    )
  (unload_dialog id)
  (command "zoom" "e")
  (princ)
  )
 
  

(defun venlo-plan (kuadu kaijian duan_lizhu bian_lizhu
		    p0_x p0_y / osmode celtscale)
  ;;����ʽ�������һ���ƽ��ͼ
  ;;�ʺ�9.6m��3.2m��6.4m��10.8m��3.6m 7.2m��12m��4m 8m��16m(4m 8m)���,4m\8m����
  ;;���β���
  ;;��ȹ�����ʽ(��� ���� [��� ����])��(9.6 3 6.4 1 9.6 3)
  ;;�����������(���� ������ [���� ������])��(4 10)��(4 4 8 1 4 4)
  (setq osmode (getvar "osmode"))
  (setvar "osmode" 0)
  (setvar "pdmode" 66)
  (setvar "pdsize" 1200)
  (setvar "cmdecho" 0)
  (command "layer" "Make" "����" "")	;���û���������ͼ��
  (command "layer" "Make" "�칵" "Color" 1 "" "Ltype" "Center" "" "") ;�����칵������ͼ��
  (command "layer" "Make" "���" "Color" 3 "" "Ltype" "Center" "" "") ;�������������ͼ��
  (setq	
	kuadu	    (read kuadu)	;��ȹ���
	kaijian	    (read kaijian)	;�������
	kuadubiao   kuadu
	kaijianbiao kaijian
	p0 (list p0_x p0_y)
	text-size  700                   ;���ñ���������ִ�С��������С
  )
  (setvar "clayer" "����")
  
  (command "point" p0 "")
  (setq p-set (ssadd (entlast)))	;�����˵�ѡ��
  (setq	vl 0
	vw 0
  )					;vl���ҿ�ȷ��򳤶�,vw���ҿ��䷽�򳤶�
  (setq	tj (cond ((= (car kuadu) 16) 4000)
		 ((= (rem (* 10 (car kuadu)) 32) 0) 3200)
		 ((= (rem (* 10 (car kuadu)) 36) 0) 3600)
		 ((= (rem (* 10 (car kuadu)) 40) 0) 4000)
	   )
  )					; tj �����칵ֱ�Ӿ���,�������ݼ����
;;;;;;;;��ȷ����һ�Ų���
  (setq pp p0)
  (while (/= (car kuadu) nil)
    (setq kd (* 1000 (car kuadu))	;���
	  ks (cadr kuadu)		;�������
    )
    (setq vl (+ vl (* kd ks)))
    (repeat ks
      (setq pp (polar pp 0 kd))
      (command "point" pp "")
      (ssadd (entlast) p-set)
    )
    (setq kuadu (cddr kuadu))		;��һ����ȹ��
  )
;;;;;;;;;���䷽������
  (while (/= (car kaijian) nil)
    (setq kj  (* 1000 (car kaijian))	;kj�������(mm)
	  kjs (cadr kaijian)		;kjs��������
    )
    (repeat kjs
      (setq vw (+ vw kj))
      (command "copy" p-set ""
	       "d" (list 0 vw))
      )    
    (setq kaijian (cddr kaijian))	;��һ��������
  )
;;;;;;;;����ߺ��칵��
  (setq	tjs   (fix (1+ (/ vl tj)))	;tjs �칵����
	kjs+1 (1+ (/ vw 4000))
  )					;kjs+1 ���䷽���������

  
  (setq celtscale (getvar "celtscale")) ;�������ͱ���
  (setvar "celtscale" 1) ;�������ͱ�������
  
  (setvar "clayer" "���")
  (command "line" p0 (polar p0 0 vl) "") ;�����
  (command "array"
	   (entlast)
	   ""
	   "r"
	   kjs+1
	   1
	   4000
  )

  (setvar "clayer" "�칵")
  (command "line" p0 (polar p0 (/ pi 2) vw) "") ;�칵��
  (command "array"
	   (entlast)
	   ""
	   "r"
	   1
	   tjs
	   tj
  )
  (setvar "celtscale" celtscale) ;�ָ����ͱ���

  (setvar "clayer" "0")
  (command "style" "number" "gbcbig" (rtos text-size 2 0) "" "" "" "" "")
					;���ñ��������
  (setq	p0- (polar p0 (/ pi -2) 4000)	;p0- �б����ʼ��
	p0+ (polar p0 pi tj)
  )					;p0+ �б����ʼ��
  (setq n 1)
  (repeat tjs				;X���б��
    (numer p0- (+ 50 text-size) n)			;��ź���
    (setq p0- (polar p0- 0 tj)
	  n   (1+ n)
    )
  )
  (setq n 1)
  (repeat kjs+1				;Y���б��,ת���ɺ���
    (numer-to-c p0+ (+ 50 text-size) n)
    (setq p0+ (polar p0+ (/ pi 2) 4000)
	  n   (1+ n)
    )
  )
;;;;;;;;;;;�Ƿ����Ӷ������ͱ�����
 
  (if (= duan_lizhu "1")
    (progn
      (setq pp	   p0
	    pp-set (ssadd)
      )
      (while (/= (car kuadubiao) nil)
	(setq kd (* 1000 (car kuadubiao)) ;���
	      ks (cadr kuadubiao)	;�������
	)
	(repeat	ks
	  (repeat (fix (1- (/ kd tj)))
	    (setq pp (polar pp 0 tj))
	    (command "point" pp "")
	    (ssadd (entlast) pp-set)
	  )
	  (setq pp (polar pp 0 tj))
	)
	(setq kuadubiao (cddr kuadubiao)) ;��һ����ȹ��
      )
      (command "copy"
	       pp-set
	       ""
	       "d"
	       (list 0 vw)
      )
    )
  )
;;;;;;;;;;

  (if (= bian_lizhu "1")
    (progn
      (setq pp	   p0
	    pp-set (ssadd)
	    vw	   0
      )

      (while (/= (car kaijianbiao) nil)
	(setq kj  (* 1000 (car kaijianbiao)) ;kj�������(mm)
	      kjs (cadr kaijianbiao)
	)				;kjs��������
	(if (= kj 8000)
	  (progn
	    (repeat kjs
	      (setq pp (polar pp (/ pi 2) 4000))
	      (command "point" pp "")
	      (ssadd (entlast) pp-set)
	      (setq pp (polar pp (/ pi 2) 4000))
	    )
	  )

	  (setq	vw (+ vw (* kj kjs))
		pp (polar pp (/ pi 2) vw)
	  )
	)
	(setq kaijianbiao (cddr kaijianbiao)) ;��һ��������
      )
      (command "copy" pp-set ""
	       "d" (list vl 0))
    )
  )
  (setvar "osmode" osmode)
)

(defun itoc (n / n-str str jump)
					;������nת���ɺ��֡���һ����...�š�����ʽ
  (setq	n-str (itoa n) ;������ת�����ַ���
	jump  1 ;ѭ������
	str   "" ;����ת������ַ����������óɿմ�
  )
  (repeat (strlen n-str) ;��������λ������ѭ������

    (setq
      str (strcat
	    str
	    (nth (atoi (substr n-str jump 1))
		 '("��" "һ" "��" "��" "��" "��" "��" "��" "��" "��")
	    )
	  )
    )
    (setq jump (1+ jump))
  )
  str
)

(defun numer (p r n)
  ;;pԲ�� r�뾶 n���
  (command "circle" p r)
  (command "text" "m" p "" (itoa n))
)

(defun numer-to-c (p r n)
   ;;pԲ�� r�뾶 n���
  (command "circle" p r)
  (command "text" "m" p "" (itoc n));ת���ɺ���������ʽ
)

  (print "����ʽ�������һ���ƽ��ͼ")
  (print "�ʺ�9.6m��3.2m��6.4m��10.8m��3.6m 7.2m��12m��4m 8m��16m(4m 8m)���,4m\8m����")
  (print "���β���")
  (print "��ȹ�����ʽ(��� ���� [��� ����])��(9.6 3 6.4 1 9.6 3)")
  (print "�����������(���� ������ [���� ������])��(4 10)��(4 4 8 1 4 4)")