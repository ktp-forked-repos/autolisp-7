(defun c:pp  (/ osmode print-name paper-min paper-max p-l paper-z)
  ;; ����������ϵͳ�Դ���dwg to pdf ��ӡ�����pdfͼֽ
  ;;����(f)���У�c����ӡ
  ;;print-name ��ӡ������ paper-z ֽ�Ŵ�С����
  ;;p-l �����Ǻ��򲼾� paper-min paper-max ���� ���½ǵ� ���Ͻǵ�
  ;;��������»����޸Ĳ��������²������޸� print-name 1��λ�� paper-z 3��λ��
  (setq osmode (getvar "osmode"))
  (setvar "osmode" 33)
  (princ "\n�Զ�ѭ����ӡ����ӡ����<����>����ӡƫ��<����>")
  (while (not (= (progn	(initget "q")
			(setq paper-min
			       (getpoint "\n�����ӡ��Χһ�ǵ��<q>�˳�:")))
		 "q"))			;�ж��Ƿ������ӡ�����˳�
    (setq paper-max  (getcorner paper-min "\n�����ӡ��Χ�ĶԽǵ�")
	  print-name "dwg to pdf"	;��ӡ������
	  )
    (if	(<= (abs (- (car paper-min) (car paper-max)))
	    (abs (- (cadr paper-min) (cadr paper-max))))
      (setq p-l	    "p"
	    paper-z "ISO full bleed A4 (210.00 x 297.00 ����)")
					;����A4��ӡ
      (progn
	(setq p-l "l")			;�����ӡ
	(if
	  (=
	    (getstring
	      "\nA4ͼֽ����A3ͼֽ�����ӡ��Ĭ��A4ֱ�ӻس���A3������3��")
	    "3")
	   (setq paper-z "ISO full bleed A3 (420.00 x 297.00 ����)")
	   (setq paper-z "ISO full bleed A4 (297.00 x 210.00 ����)"))))
    (setvar "cmdecho" 0)
    (command
      "plot"
      "y"				;��ϸ����
      ""				;Ĭ��ģ�Ϳռ�
      print-name			;��ӡ������
      paper-z				;ֽ�Ŵ�С���� A4 A3
      "m"				;���׵�λ
      p-l				;��ӡ���� p ���� l����
      "n"				;���µߵ���ӡ
      "w"				;����ģʽ
      paper-min				;��ӡ��Χ�ǵ�
      paper-max				;��ӡ��Χ�ǵ�
      "f"				;��ӡ���� ����
      "c"				;��ӡƫ�� ����
      ""				;Ĭ���Ƿ���ʽ��ӡ
      "monochrome.ctb"			;Ĭ�ϴ�ӡ��ʽ�� �ڰ״�ӡ
      ""				;Ĭ���Ƿ��ӡ�߿�
      ""				;Ĭ����ɫ��ӡ����
      (strcat
	(getvar "DWGPREFIX")		;��ǰ�ĵ�·��
	(vl-string-subst
	  (strcat "-" (substr (rtos (getvar "cdate") 2 6) 10 6))
	  ".dwg"
	  (getvar "DWGNAME")))		;�����pdf�ļ�������ʱ���β
      "n"				;Ĭ���Ƿ񱣴��ҳ�����õ��޸�
      "y"				;�Ƿ������ӡ
      ))
  (setvar "osmode" osmode)
  (princ))




;;;  ��ȡ�ļ���  (getvar "DWGPREFIX")  (getvar "DWGNAME")
;;;  ��ȡʱ�� (getvar "date")





