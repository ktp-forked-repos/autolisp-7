(defun c:pm (/ osmode print-name paper-min paper-max p-l paper-z)
  ;;����(f)���У�c����ӡ
  ;;print-name ��ӡ������ paper-z ֽ�Ŵ�С����
  ;;p-l �����Ǻ��򲼾� paper-min paper-max ���� ���½ǵ� ���Ͻǵ�
  ;;��������»����޸Ĳ��������²������޸� print-name 1��λ�� paper-z 3��λ��


(setq osmode (getvar "osmode"))
  (setvar "osmode" 33)
  
  (princ "\n�Զ�ѭ����ӡ����ӡ����<����>����ӡƫ��<����>")
  (while
    (not
      (= (progn
	   (initget "q")
	   (setq paper-min (getpoint "\n�����ӡ��Χһ�ǵ��<q>�˳�:"))
	 )
	 "q"
      )
    )					;�ж��Ƿ������ӡ�����˳�

     (setq
       paper-max  (getcorner paper-min "\n�����ӡ��Χ�ĶԽǵ�")
       print-name "HP LaserJet Pro M701 PCL 6"  ;��ӡ������
     )					
  
  (if (<= (abs (- (car paper-min) (car paper-max)))
	  (abs (- (cadr paper-min) (cadr paper-max)))
      )
    (setq p-l	  "p"
	  paper-z "A4"
    )					;����A4��ӡ
    (progn
      (setq p-l "l")			;�����ӡ
      (if
	(= (getstring
	     "\nA4ͼֽ����A3ͼֽ�����ӡ��Ĭ��A4ֱ�ӻس���A3������3��"
	   )
	   "3"
	)
	 (setq paper-z "A3")
	 (setq paper-z "A4")
      )
    )
  )

  (setvar "cmdecho" 0)
  (command "plot"  "y"			;��ϸ����
	   ""				;Ĭ��ģ�Ϳռ�
	   print-name			;��ӡ������
	   paper-z			;ֽ�Ŵ�С���� A4 A3
	   "m"				;���׵�λ
	   p-l				;��ӡ���� p ���� l����
	   "y"				;���µߵ���ӡ
	   "w"				;����ģʽ
	   paper-min                    ;��ӡ��Χ�ǵ�
	   paper-max                    ;��ӡ��Χ�ǵ�
	   "f"                          ;��ӡ���� ����
	   "c"				;��ӡƫ�� ����
	   "y"				;Ĭ�ϰ���ʽ��ӡ
	   "monochrome.ctb"		;Ĭ�ϴ�ӡ��ʽ�� monochrome.ctb�ڰ״�ӡ
	   ""				;Ĭ���Ƿ��ӡ�߿�
	   ""				;Ĭ����ɫ��ӡ����
	   ""				;Ĭ���Ƿ��ӡ���ļ�
	   ""				;Ĭ���Ƿ񱣴��ҳ�����õ��޸�
	   "y"				;�Ƿ������ӡ
	  )
)
  (setvar "osmode" osmode)
(princ)
)

