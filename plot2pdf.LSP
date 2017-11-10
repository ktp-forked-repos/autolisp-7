;;;������ͼ��paper-a4x paper-a4y paper-a3 ��Χ�ڴ�ӡ��pdfͼֽ
;;;2017.11.09
(defun c:p2p  (/            osmode       ss           ss-name
               scale        paper-min    paper-max    num
               insert-name  draw-paper-name)
  ;; osmode �洢����ģʽ
  ;; ss ͼ��ѡ��
  (setq osmode (getvar "osmode"))       ;���沶������
  (setvar "osmode" 0)                   ;ȡ������
  (setvar "PLOTOFFSET" 1)               ; ���ý���ӡƫ���趨Ϊ�����ͼֽ��
  (princ "��ѡ��Ҫ��ӡ��ͼֽ�򣬽�������ѡ��ʽ��F��")
  (setq ss (ssget '((0 . "insert")      ;�����б� ֻѡ���ض����Ƶ�ͼ��
                    (-4 . "<or")
                    (2 . "paper-a4y")
                    (2 . "a4y")
                    (2 . "paper-a4x")
                    (2 . "a4x")
                    (2 . "paper-a3x")
                    (2 . "a3x")
                    (-4 . "or>"))))
  (command "ucs" "w")                   ;�ָ���������ϵ,Ϊ����get_minmax������ȡ��ȷ���������׼��
  (setq num 0)                          ; ʹ��num ѭ������ ͼ��ѡ��
  (repeat (sslength ss)
    (setq ss-name     (ssname ss num)   ;��ȡ��n��ͼԪ��
          scale       (cdr (assoc 41 (entget ss-name)))
                                        ;scale ͼ��ķŴ����
          insert-name (cdr (assoc 2 (entget ss-name))) ;ͼ��������
          )
    (if (= insert-name "a4y")           ;������ʷ������a4x a4y a3x ͼ������ 
      (setq insert-name "paper-a4y"))
    (if (= insert-name "a4x")
      (setq insert-name "paper-a4x"))
    (if (= insert-name "a3x")
      (setq insert-name "paper-a3x"))
    (setq paper-min       (car (get_minmax ss-name))
                                        ;�Զ���get_minmax����,��ȡͼ������½������
          paper-max       (cadr (get_minmax ss-name)) ;ͼ�����Ͻ������
          draw-paper-name (get-biaotilan-attr-papername paper-min paper-max))
                                        ;�Զ��庯��,��ȡ��������ͼֽ��
    (plot insert-name scale paper-min paper-max draw-paper-name)
                                        ;�Զ���plot����,��ӡ��pdf�ļ�
    (command "_text"                    ;ͼ���µ�������,��ע��ӡ����ʱ��
             "s"
             (strcat "s-" (rtos scale)) ; ����������ʽ,��ͼ��ʱ��������Ӧ��ʽ
             (mapcar '+ paper-min (list 0 (* -4 scale))) ;����λ��
             "0"
             (strcat "���� "
                     (substr (rtos (getvar "cdate") 2 7) 1 15)
                                        ;��ӡ����ʱ��
                     " ��ӡΪPDF�ļ�"))
    (setq num (1+ num)))
  (princ (strcat "����ӡ��" (rtos num) "��ͼֽ"))
  (setvar "osmode" osmode)              ;�ָ���������
  (princ))

(defun Get_MinMax  (en / vl-en minext maxext)
                                        ;�õ��������ǵ� ��С�ǵ� ------����д��
                                        ;��������������ϵ��
  (vl-load-com)
  (setq *acadobject*   (vlax-get-acad-object)
        *acaddocument* (vla-get-activedocument *acadobject*)
        *mspace*       (vla-get-modelspace *acaddocument*)) ;_ Endsetq
;;;  (setq en (entsel "Please select a object: "))
  (setq vl-en (vlax-ename->vla-object en))
  (setq minext (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (setq maxext (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (vla-getboundingbox vl-en 'minext 'maxext)
  (list (vlax-safearray->list minext) (vlax-safearray->list maxext)))

(defun plot  (paper        scale        paper-min    paper-max
              pdf-filename /            p-l          paper-z
              transpose    plot-offset  plot-min     plot-max)
  ;;����������c:p2p���ɲθ�������Ȼ�����cad������plot�����ӡͼֽ
  ;;����ͼֽ���� paper(a4x a4y a3x) ��ӡ���� scale ��ӡ��Χ ��ӡͼֽ ��ӡƫ�� plot-offset
  ;;��ӡ��Χ plot-min plot-max
  ;;pdf-filename ��ӡ���ļ���
  ;;paper-z ֽ������ paper-min paper-max ���� ���½ǵ� ���Ͻǵ�
  ;;transpose �Ƿ����µߵ���ӡ
  (cond ((= paper "paper-a4y")
         (setq p-l         "P"          ; ������ӡ
               paper-z     "ISO full bleed A4 (210.00 x 297.00 ����)"
                                        ;ֽ������
               transpose   "y"
               plot-offset "25,5"       ; ��ӡƫ��
               plot-min    (mapcar '+ paper-min (list (* scale 25) (* scale 5)))
                                        ;��ȡ��ӡ��Χ���¿��
               plot-max    (mapcar '+ paper-max (list (* scale -5) (* scale -5)))
                                        ;��ȡ��ӡ��Χ���Ͽ��
               ))
        ((= paper "paper-a4x")
         (setq p-l         "l"          ;�����ӡ
               paper-z     "ISO full bleed A4 (297.00 x 210.00 ����)"
               transpose   "n"
               plot-offset "5,5"
               plot-min    (mapcar '+ paper-min (list (* scale 5) (* scale 5)))
                                        ;��ȡ��ӡ��Χ���¿��
               plot-max    (mapcar '+ paper-max (list (* scale -5) (* scale -25)))
                                        ;��ȡ��ӡ��Χ���Ͽ��
               ))
        ((= paper "paper-a3x")
         (setq p-l         "l"
               paper-z     "ISO full bleed A3 (420.00 x 297.00 ����)"
               transpose   "n"
               plot-offset "5,5"
               plot-min    (mapcar '+ paper-min (list (* scale 5) (* scale 5)))
                                        ;��ȡ��ӡ��Χ���¿��
               plot-max    (mapcar '+ paper-max (list (* scale -5) (* scale -25)))
                                        ;��ȡ��ӡ��Χ���Ͽ��
               )))                      ;ȷ��ͼֽ�����Ǻ����ӡ
  (setvar "cmdecho" 0)
  (command "plot"
           "y"                          ;��ϸ����
           ""                           ;Ĭ��ģ�Ϳռ�
           "dwg to pdf"                 ;��ӡ������
           paper-z                      ;ֽ������
           "m"                          ;���׵�λ
           p-l                          ;��ӡ����
           transpose                    ;���µߵ���ӡ
           "W"                          ;����ģʽ
           plot-min
           plot-max
           (strcat "1:" (rtos scale 2)) ;��ӡ����
           plot-offset                  ;��ӡƫ��
           ""                           ;Ĭ���Ƿ���ʽ��ӡ
           "."                          ;Ĭ�ϴ�ӡ��ʽ�� .��ʾ��
           ""                           ;Ĭ���Ƿ��ӡ�߿�
           ""                           ;Ĭ����ɫ��ӡ����
           (strcat (getvar "DWGPREFIX") ;��ǰ�ĵ�·��
                   (strcat pdf-filename ;ͼ��
                           "_"
                           (substr (getvar "DWGNAME") 1 3)
                                        ;CAD�ĵ���ǰ3���ַ�
                           "_"
                           (substr (rtos (getvar "cdate") 2 6) 5 4)))
                                        ;�����pdf�ļ���:ͼ��-CAD�ļ���ǰ3λ-��������ʱ��
           "n"                          ;Ĭ���Ƿ񱣴��ҳ�����õ��޸�
           "y"                          ;�Ƿ������ӡ
           )                            ;  
  )


(defun get-biaotilan-attr-papername
       (paper-min paper-max / ss paper-name paper-num)
  ;;����ͼ����Χpaper-min paper-max
  ;;����ͼ����Χ�ڱ������������"ͼ��"-"ͼ��"��ֵ
  ;; ͼ�� Ϊ PDF�ļ����ļ�����ɲ���
  (setq ss (ssget "_W" paper-min paper-max '((2 . "������"))))
                                        ;ss�Ǳ�����ͼ��
                                        ;ȡ�ñ�����ͼ�������б�����ֵ��biaotilan-attr
  (setq paper-name (vlax-get-property   ;paper-name ͼ��
                     (nth 0             ;ͼ���Ǳ������ĵ�1������
                          (vlax-safeArray->list
                            (vlax-variant-value
                              (vla-getattributes
                                (vlax-ename->vla-object (ssname ss 0))))))
                     'TextString)
        paper-num  (vlax-get-property   ;paper-num ͼ��
                     (nth 3             ;ͼ���Ǳ������ĵ�4������
                          (vlax-safeArray->list
                            (vlax-variant-value
                              (vla-getattributes
                                (vlax-ename->vla-object (ssname ss 0))))))
                     'TextString))
  (if (/= paper-num "ͼ��")             ;������ض�ͼ��,���� ͼ��-ͼ��,���򵥶����� ͼ��
    (strcat paper-num "-" paper-name)
    paper-name))


;;;    (cond
;;;      ((or (= insert-name "paper-a4y") (= insert-name "a4y"))
;;;       (progn (setq paper-min       (car (get_minmax ss-name))
;;;                    paper-max       (cadr (get_minmax ss-name))
;;;                    draw-paper-name (get-biaotilan-attr-papername
;;;                                      paper-min
;;;                                      paper-max))
;;;              (plot "paper-a4y" scale paper-min paper-max draw-paper-name)
;;;              (command "_text"
;;;                       "s"
;;;                       (strcat "s-" (rtos scale))
;;;                       (mapcar '+ paper-min (list 0 (* -4 scale)))
;;;                       "0"
;;;                       (strcat "���� "
;;;                               (substr (rtos (getvar "cdate") 2 7) 1 15)
;;;                               " ��ӡΪPDF�ļ�"))))
;;;      ((or (= insert-name "paper-a4x") (= insert-name "a4x"))
;;;       (progn (setq paper-min       (car (get_minmax ss-name))
;;;                    paper-max       (cadr (get_minmax ss-name))
;;;                    draw-paper-name (get-biaotilan-attr-papername
;;;                                      paper-min
;;;                                      paper-max))
;;;              (plot "paper-a4x" scale paper-min paper-max draw-paper-name)
;;;              (command "_text"
;;;                       "s"
;;;                       (strcat "s-" (rtos scale))
;;;                       (mapcar '+ paper-min (list 0 (* -4 scale)))
;;;                       "0"
;;;                       (strcat "���� "
;;;                               (substr (rtos (getvar "cdate") 2 7) 1 15)
;;;                               " ��ӡΪPDF�ļ�"))))
;;;      ((or (= insert-name "paper-a3x") (= insert-name "a3x"))
;;;       (progn (setq paper-min       (car (get_minmax ss-name))
;;;                    paper-max       (cadr (get_minmax ss-name))
;;;                    draw-paper-name (get-biaotilan-attr-papername
;;;                                      paper-min
;;;                                      paper-max))
;;;              (plot "paper-a3x" scale paper-min paper-max draw-paper-name)
;;;              (command "_text"
;;;                       "s"
;;;                       (strcat "s-" (rtos scale))
;;;                       (mapcar '+ paper-min (list 0 (* -4 scale)))
;;;                       "0"
;;;                       (strcat "���� "
;;;                               (substr (rtos (getvar "cdate") 2 7) 1 15)
;;;                               " ��ӡΪPDF�ļ�")))))

