(defun xy-move (X0Y0 x-det y-det) (mapcar '+ x0y0 (list x-det y-det)))

(defun itoc (n / n-str str jump)	;������nת���ɺ��֡���һ����...�š�����ʽ
  (setq	n-str (itoa n)			;������ת�����ַ���
	jump  1				;ѭ������
	str   ""			;����ת������ַ����������óɿմ�
  )
  (repeat (strlen n-str)		;��������λ������ѭ������
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

(defun numer (p r n c)
  ;;pԲ�� r�뾶 n��� c��ת������0������ʽ��1������������ʽ
  (command "circle" p r)
  (if (= c 0)
    (command "text" "m" p "" (itoa n))	;������ʽ
    (command "text" "m" p "" (itoc n))	;������ʽ
  )
)

(defun dim-style-set (scal)
  ;;���ñ�ע��ʽ
  ;;scal ��ӡ����
  (command "style" "dim-scal-text" "gbcbig.shx" 0 "" "" "" "" "")
					;����������ʽ
  (setvar "dimtxsty" "dim-scal-text")	;���ñ�ע����               
  (setvar "dimscale" scal)		;���÷Ŵ����
  (command "dimstyle"
	   "s"
	   (strcat "s-"
		   (rtos scal 2 1)
		   "-t"
		   (substr (rtos (getvar "cdate") 2 6) 10 4)
	   )
  )					;���ݵ�ǰʱ�����ñ�ע��ʽ����
)



(defun c:zw (/		   shang-kai	 shang-kai-dist
	     shang-kai-num zw-l1	 zw-l2	       x0y0
	     text-size	   you-jin	 dim-length    num
	     osmode	   dim-dist-scale
	    )				;������������
					;�����Ͽ�����(��� ������ ��� ������ ...)���磺(4000 10 ...)
					;�����ҿ�����(��� ������ ��� ������ ...)���磺(10800 10 ...)
					;�����������ߴ��߾���
					;�汾 ʱ�� ����
  (while
    (progn
      (setq shang-kai  (read (strcat "(" (getstring t "\n�����Ͽ����") ")")
		       )
	    you-jin    (read (strcat "(" (getstring t "\n�����ҽ����") ")"))
	    dim-length (read (getstring "\n���������ӳ��߾���<250>:"))
	    x0y0       (getpoint "\n�����������½ǵ�:")
	    bianhaoqiu-zhijing  (getint "��������ֱ��<1500>:")
      )
      (if (not dim-length)
	(setq dim-length 250)
      )
      (if (not bianhaoqiu-zhijing)
	(setq bianhaoqiu-zhijing 1500)
      )
      (setq text-size (- (/ bianhaoqiu-zhijing 2) 50)) ;�����������߶�
      (setq dim-dist-scale 4)		;�����ߴ��߾���������ӳ��߾���ı���
      (princ "�Ͽ�������")
      (princ shang-kai)
      (princ ";�ҽ�������")
      (princ you-jin)
      (princ (strcat ";�����ӳ��߳���"
		     (rtos dim-length 2)
		     "; ���������߶�"
		     (rtos text-size 2)
	     )
      )
      (princ ";�������½ǵ㣺")
      (princ x0y0)
      (initget "r")
      (=
	(getkword
	  "\n�鿴�����Ƿ���ȷ�����ִ�л�ͼ��ֱ�ӻس�����������������������r��"
	)
	"r"
      )
    )
  )
  (setq osmode (getvar "osmode"))
  (setvar "osmode" 0)



  
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;���Ͽ����ϵ�б�
  (setq	shang-kai-dist nil
	shang-kai-num nil
  )
  (repeat (/ (length shang-kai) 2)
    (setq shang-kai-dist (cons (car shang-kai) shang-kai-dist)
	  shang-kai-num	 (cons (car (cdr shang-kai)) shang-kai-num)
	  shang-kai	 (cddr shang-kai)
    )
  )
  (setq	shang-kai-dist (reverse shang-kai-dist)
	shang-kai-num  (reverse shang-kai-num)
  )
  (setq	zw-y (+	(* 2 dim-length)
		(apply '+ (mapcar '* shang-kai-dist shang-kai-num))
	     )				;����Y���߳���
  )
;;;;;;;;;;;;;;;;;;;;;;���ҽ�ϵ�й���
  (setq	you-jin-dist nil
	you-jin-num nil
  )
  (repeat (/ (length you-jin) 2)
    (setq you-jin-dist (cons (car you-jin) you-jin-dist)
	  you-jin-num  (cons (car (cdr you-jin)) you-jin-num)
	  you-jin      (cddr you-jin)
    )
  )
  (setq	you-jin-dist (reverse you-jin-dist)
	you-jin-num  (reverse you-jin-num)
  )
  (setq	zw-x (+	(* 2 dim-length)
		(apply '+ (mapcar '* you-jin-dist you-jin-num))
	     )
  )					;����X���߳���
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;���ñ�ע�����ͱ���������С
  (dim-style-set (/ (car shang-kai-dist) 40)) ;���ñ�ע��ʽ����
  (command "style"
	   "number"
	   "gbcbig"
	   (rtos text-size 2 0)
	   ""
	   ""
	   ""
	   ""
	   ""
  )					;���ñ��������
;;;;;;;;;;;;;;;;;;;;;;;;;��X����
  (setq zw-x0 (xy-move x0y0 (- dim-length) 0)) ;X�������½ǵ�
  (setq zw-x-list (cons (list zw-x0 (xy-move zw-x0 zw-x 0)) nil))
					;X���ߵ�Ա�
  (while (car shang-kai-num)
    (repeat (car shang-kai-num)
      (setq zw-x-list
	     (cons
	       (list
		 (xy-move (caar zw-x-list) 0 (car shang-kai-dist))
		 (xy-move (caar zw-x-list)
			  zw-x
			  (car shang-kai-dist)
		 )
	       )
	       zw-x-list
	     )
      )
    )
    (setq shang-kai-num	 (cdr shang-kai-num)
	  shang-kai-dist (cdr shang-kai-dist)
    )
  )
  (setq zw-x-list (reverse zw-x-list))
  (command "layer" "M" "zhouwang" "C" "red" "" "Lw" 0.13 "" "")
					;����������
  (foreach point zw-x-list (command "line" (car point) (cadr point) ""))
  (command "layer" "M" "zhouwang-num" "C" "green" "" "Lw" 0.13 "" "")
					;����������ż��ߴ��߲�
;;;;;;;;;;;;;;����x����ű�ע
  (setq	num 1
	zw-x-end-list nil
  )
  (foreach point zw-x-list
    (numer (xy-move (car point) -2000 0) (+ 50 text-size) num 1)
					;������ʽ�����
    (setq zw-x-end-list (cons (cadr point) zw-x-end-list))
					;����X����յ��б�
    (setq num (1+ num))
  )
  (setq zw-x-end-list (reverse zw-x-end-list))
  (setq num 0)
  (repeat (1- (length zw-x-end-list))
    (command "dimlinear"
	     (nth num zw-x-end-list)
	     (nth (1+ num) zw-x-end-list)
	     "v"
	     (xy-move (nth (1+ num) zw-x-end-list)
		      (* dim-dist-scale dim-length)
		      -1000
	     )
    )
    (setq num (1+ num))
  )
  (command "dimlinear"
	   (nth 0 zw-x-end-list)
	   (nth num zw-x-end-list)
	   "v"
	   (xy-move (nth num zw-x-end-list)
		    (* 2 dim-dist-scale dim-length)
		    -1000
	   )
  )
;;;;;;;;;;;;;;;��Y����
  (setq zw-y0 (xy-move x0y0 0 (- dim-length))) ;y�������½ǵ�
  (setq zw-y-list (cons (list zw-y0 (xy-move zw-y0 0 zw-y)) nil))
					;y���ߵ�Ա�
  (while (car you-jin-num)
    (repeat (car you-jin-num)
      (setq zw-y-list
	     (cons
	       (list
		 (xy-move (caar zw-y-list) (car you-jin-dist) 0)
		 (xy-move (caar zw-y-list) (car you-jin-dist) zw-y)
	       )
	       zw-y-list
	     )
      )
    )
    (setq you-jin-num  (cdr you-jin-num)
	  you-jin-dist (cdr you-jin-dist)
    )
  )
  (setq zw-y-list (reverse zw-y-list))
  (command "layer" "S" "zhouwang" "")
  (foreach point zw-y-list (command "line" (car point) (cadr point) ""))
;;;;;;;;;;;;;;����y����ű�ע
  (setq	num 1
	zw-y-end-list nil
  )
  (command "layer" "S" "zhouwang-num" "")
  (foreach point zw-y-list
    (numer (xy-move (car point) 0 -2000) (+ 50 text-size) num 0)
					;������ʽ�����
    (setq zw-y-end-list (cons (cadr point) zw-y-end-list))
					;����y����յ��б�
    (setq num (1+ num))
  )
  (setq zw-y-end-list (reverse zw-y-end-list))
  (setq num 0)
  (repeat (1- (length zw-y-end-list))
    (command "dimlinear"
	     (nth num zw-y-end-list)
	     (nth (1+ num) zw-y-end-list)
	     "h"
	     (xy-move (nth (1+ num) zw-y-end-list)
		      -1000
		      (* dim-dist-scale dim-length)
	     )
    )					;�����߱�ע
    (setq num (1+ num))
  )
  (command "dimlinear"
	   (nth 0 zw-y-end-list)
	   (nth num zw-y-end-list)
	   "h"
	   (xy-move (nth num zw-y-end-list)
		    -1000
		    (* 2 dim-dist-scale dim-length)
	   )
  )					;�����߱�ע
;;;;;;;;;;;;;;;;;
  (command "zoom" "a")
  (setvar "osmode" osmode)
)