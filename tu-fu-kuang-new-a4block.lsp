;;;��ģ�Ϳռ仭��ͼ���򣬲���������ͱ�ע��ʽ
;;;ֻ�����ͼ����������Խǵ㣬�Զ������������
;;;֧��A4�����������A3�����ͼ����
;;;Version: 2.5
;;;�޸�ʱ�䣺2015/11/23
;;;���ߣ�kangdalei#gmail.com



(defun c:tu-fu-kuang  (/ p1 p2 lenth width p0 l/w osmode)
  (setq osmode (getvar "osmode"))       ;���沶������
  (setvar "osmode" 0)                   ;ȡ������
  (setvar "ATTDIA" 0) ����ϵͳ����Ϊ0�������ʱ��������������
  (princ "����ͼ����������Խǵ㣺")
  (setq p1    (getpoint "\n��һ�㣺")   ;ͼ�����һ��
        p2    (getcorner p1 "")         ;�ڶ���
        lenth (len p1 p2)               ;ͼ����ˮƽ����
        width (wid p1 p2)               ;ͼ������ֱ����
        p0    (bottom-left p1 p2)       ;���ͼ��������½ǵ�
        l/w   (/ lenth width))          ;ͼ����ĳ����
  (command "style"                      ;�������壬������ע��ʽʹ��
           "Annotative" "gbcbig.shx" 0  ;����߶�Ϊ0
           "" "" "" "" "")
  (setvar "celweight" 25)               ;�����߿�0.25mm
  (cond ((<= l/w 1)                     ;����ͼֽ
         (a4y-kuang p0
                    (max (fix-scal lenth 180) ;�ڳ��Ϳ���ѡ���Խϴ����
                         (fix-scal width 287)))) ;A4����
        ((a4x?)                         ;�ж���A4�������A3���?
         (a4x-kuang p0
                    (max (fix-scal lenth 267) (fix-scal width 200))))
                                        ;A4���
        (t
         (a3x-kuang p0
                    (max (fix-scal lenth 390) (fix-scal width 287))))
                                        ;A3���
        )
  (setvar "celweight" -1)               ;�����߿����
  (setvar "osmode" osmode)              ;�ָ���������
  (setvar "ATTDIA" 1) ����ϵͳ����Ϊ1�������ʱcui��������
  (princ)
  )

;;;;;;;�������Ӻ���
(defun a4x?  ()
  ;;�û�ѡ����A3����A4��ӡ
  (if
    (/=
      (getstring
        "Ԥ�ƽ�����A4ֽ���������ӡ��(y/n��ѡ������A3ֽ���������ӡ����")
      "n")
     t
     nil))

(defun fix-scal  (n1 n2)
  ;;����������ӣ�ֻ��ȡ1λС��
  (cond ((<= n1 n2) 1)                  ; ͼ����ֻ�Ŵ�򱣳�ԭ�ߴ�,����С
        (t (/ (fix (* 10 (/ (float n1) n2))) 10.0))))


(defun bottom-left  (p1 p2 / p3 p4)
  ;;p1 p2�ǶԽǵ�
  ;;���p1 p2���ھ������½ǵ�
  (setq p3 (list (car p1) (cadr p2))
        p4 (list (car p2) (cadr p1)))
  (list (min (car p1) (car p2) (car p3) (car p4))
        (min (cadr p1) (cadr p2) (cadr p3) (cadr p4))))

(defun len  (p1 p2)                     ;��öԽǵ�ˮƽ���ȣ����γ���
  (abs (- (car p1) (car p2))))
(defun wid  (p1 p2)                     ;��öԽǵ㴹ֱ�߶ȣ����ο�
  (abs (- (cadr p1) (cadr p2))))

(defun xy-m  (xy x-move y-move scale)   ;ֱ������ϵ�ж�ά��ƽ��
                                        ;xy��ԭʼ�����꣬x-move��y-move������ƫ������scale�Ǳ�������
  (mapcar '+ xy (list (* scale x-move) (* scale y-move))))

(defun a4x-kuang  (p0 scale )
                                        ;�������½ǵ�ͱ���������A4���ͼ����
                                        ;(setq osmode (getvar "osmode"))
                                        ;(setvar "osmode" 0)                   ;���ö��󲶻�Ϊ0

  (command "insert" "paper-a4x" p0 (rtos scale 2) "0" (rtos scale 2)) ;����ͼ���
  (command "style"                      ;����������ʽ
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
;;;           "�׼��ؼ죡�м��飡�׼��ؼ죡")
  (tufu-dim scale)                      ;���ñ�ע��ʽ
                                        ;(setvar "osmode" osmode)              ;�ָ����󲶻�
  )

(defun a4y-kuang  (p0 scale  )
                                        ;�������½ǵ�ͱ���������A4����ͼ����
                                        ;(setq osmode (getvar "osmode"))
                                        ;(setvar "osmode" 0)                   ;���ö��󲶻�Ϊ0

  (command "insert" "paper-a4y" p0 (rtos scale 2) "0" (rtos scale 2)) ;����ͼ���
  (command "style"                      ;����������ʽ
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
;;;           "�׼��ؼ죡�м��飡�׼��ؼ죡")
  
  (tufu-dim scale)                      ;���ñ�ע��ʽ
                                        ;(setvar "osmode" osmode)              ;�ָ����󲶻�
  )

(defun a3x-kuang  (p0 scale  )
                                        ;�������½ǵ�ͱ���������A3���ͼ����
                                        ;(setq osmode (getvar "osmode"))
                                        ; (setvar "osmode" 0)                   ;���ö��󲶻�Ϊ0

  (command "insert" "paper-a3x" p0 (rtos scale 2) "0" (rtos scale 2)) ;����ͼ���
  (command "style"                      ;����������ʽ
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
;;;           "�׼��ؼ죡�м��飡�׼��ؼ죡")
  (tufu-dim scale)                      ;���ñ�ע��ʽ
                                        ;(setvar "osmode" osmode)              ;�ָ����󲶻�
  )

(defun tufu-dim  (scale)
  ;;���ñ�ע��ʽ
  ;;scale ��ӡ���� 
  (command "dimstyle" "r" "iso-25-��")     ;�ָ�iso-25-�ݱ�ע��ʽ���Դ�Ϊ��׼
  (setvar "dimtxt" 2.5)                 ;�����׼�߶�
  (setvar "dimtxsty" "Annotative")      ;��������
  (setvar "dimscale" scale)             ;ȫ�ֱ�������
  (command "dimstyle" "s" (strcat "s-" (rtos scale 2 1)))) ;��������


(defun biao-ti-lan (p1 scale) ;�������½ǵ�����ű���scale����������������������Ԥ����ͼֽ��
  (command "insert" "������" p1 scale "0" "ͼ��" "����" "����" "ͼ��" "" ""
	   ""))


