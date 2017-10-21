;;����������cnc����
;;2016-07-19��1�� ��ɼ��׹���
;;��4�� д�ӳ�����뵥�����������Ϊһ������
;;2016-07-26 V4.1 ����A���Ƿ��пף�д0005��0006�ӳ���
;;2016-07-26 V4.2 ������һ��bug�����ش����������ĸ��ӳ���ı�־���ء�

(defun make-axis-pair (dist-list / first-name zhou-pair reverse-dist-list)
                                        ;�ɱ�(A 20 30 40 50)��ʽ�����ɱ�
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

(defun A>y? (a y)                       ; �ж��������a��y�ĵڶ���Ԫ�ش�С
  (> (cdr a) (cdr y))
  )

(defun yz-drilling (ff p6l1)            ;yz������ӳ���
                                        ;ff �ļ�ָ��
                                        ;p6l1 Ϊ�棬���ڼ�ͷ���������ӳ����Ϊ O0003,����Ϊ O0002
  (if p6l1
    (write-line "O0003" ff)             ;0003�ӳ��������ڼ�ͷ������
    (progn (write-line "O0002" ff)      ;0002�ӳ��������ڼ�ͷ����
           (write-line "M89 P6 L1" ff)  ;�ӽ��ڼ�ͷ
           )
    )
  (write-line "M89 P2 L1" ff)           ;�ӽ����ͷ
  (write-line "G04 P200" ff)            ;��ͣ200����
  (write-line "G01 Y80 Z80 F6000" ff)   ;��ͷ���ٽ����� Y��76 Z��89��λ�� []ʵ����ֵ��Ҫ������ͷ���ȵ���
  (write-line "G01 Y85 Z85 F100" ff)    ;��ͷ��ף����ٽ���
  (write-line "G01 Y75 Z75 F6000" ff)   ;���꣬�����˳�
  (if (not p6l1)
    (write-line "M89 P6 L0" ff)         ;�Ƿ����ڼ�ͷ
    )                                   ;���ڼ�ͷ
  (write-line "M89 P2 L0" ff)           ;�����ͷ
  (write-line "G04 P1000" ff)           ;��ͣ
  (write-line "M99" ff)                 ;����������
  (write-line "%" ff)
  )                                     ;�ӳ������


(defun a-drilling (ff p6l1)             ;a������ӳ���
                                        ;ff �ļ�ָ��
                                        ;p6l1 Ϊ�棬���ڼ�ͷ���������ӳ����Ϊ O0006,����Ϊ O0005
  (if p6l1
    (write-line "O0006" ff)             ;O0006�ӳ��������ڼ�ͷ������
    (progn (write-line "O0005" ff)      ;0005�ӳ��������ڼ�ͷ����
           (write-line "M89 P6 L1" ff)  ;�ӽ��ڼ�ͷ
           )
    )
  (write-line "M89 P2 L1" ff)           ;�н����ͷ
  (write-line "G04 P200" ff)
  (write-line "G01 A35 F6000" ff)       ;������
  (write-line "G01 A40 F100" ff)        ;���
  (write-line "G01 A85 F6000" ff)       ;���±���
  (write-line "G01 A90 F100" ff)        ;���
  (write-line "G01 A30 F6000" ff)       ;�˻�
  (if (not p6l1)
    (write-line "M89 P6 L0" ff)         ;���ڼ�ͷ
    )
  (write-line "M89 P2 L0" ff)           ;�����ͷ
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
;;;;������
                                        ;�����������ʵ���������
  (setq det-x 18                        ;��е����� yz���ܲı߾���
        det-yz-a 112                    ;yz���A��ˮƽ����
        x-limit 120                     ;X����λ���룬С�ڴ��������ʱ�ڼ�ͷ������
        )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq point (getpoint "ָ������ԭ��")) ;�趨�ܲĵı�Ϊԭ��
  (command "ucs" point)
  (command "")
  (setq osmode (getvar "osmode"))       ;���󲶻�
  (setvar "osmode" 36)                  ;ֻ����Բ�ĺͽ���
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;����YZ��  
  (setq yz-point-list nil)              ;YZ�����λ�ô洢�б�
  (setq point         (getpoint "������YZ��Ҫ��׵ĵ�һ��Բ��λ��")
        yz-point-list (cons point nil)
        n             2
        )
  (while (setq point (getpoint point
                               (strcat "\n������YZ���"
                                       (rtos n 2)
                                       "��Ҫ��׵�Բ��λ�ã������밴ENT��:"
                                       )
                               )
               )
    (setq yz-point-list
           (cons point yz-point-list)
          n (1+ n)
          )
    )
  (setq yz-dist-list (mapcar 'car yz-point-list)) ;��ȡYZ�����Xֵ
  (setq yz (cons 'yz (mapcar '(lambda (x) (- x det-x)) yz-dist-list)))
                                        ;YZ��Xֵ�任����еԭ��
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;����A��
  (setq a-point-list nil                ;A�����λ�ô洢�б�
        point        (getpoint "������A��Ҫ��׵ĵ�һ��Բ��λ��(ȡ���밴ENT)")
        )
  (if point                             ;�����A�����
    (progn
      (setq a-point-list
             (cons point nil)
            n 2
            )
      (while (setq
               point (getpoint point
                               (strcat "\n������A���"
                                       (rtos n 2)
                                       "��Ҫ��׵�Բ��λ�ã������밴ENT��:"
                                       )
                               )
               )
        (setq a-point-list
               (cons point a-point-list)
              n (1+ n)
              )
        )
      (setq a-dist-list (mapcar 'car a-point-list))
                                        ;�����A�������ȡA�����xֵ
      (princ "\nA���ܱ߾��룺")
      (princ a-dist-list)
      (setq a
             (cons 'A (mapcar '(lambda (x) (- x det-x det-yz-a)) a-dist-list))
            )                           ;A��Xֵ�任����еԭ��
      )
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;A�ᴦ������
  (setvar "osmode" osmode)              ;�ָ�ԭ�ж��󲶻�
  (princ "\nYZ���ܱ߾��룺")
  (princ yz-dist-list)
  (princ "\n")
  (setq yz-a-x
         (vl-sort (append (make-axis-pair a) (make-axis-pair yz)) 'a>y?)
        )                               ;yz����A��ͳһ����,�����ǰ
  (princ yz-a-x)
  (setq p0002 nil                       ;O0002�ӳ����־λ,Ϊ����д
        p0003 nil                       ;O0003�ӳ����־λ
        p0005 nil                       ;O0005�ӳ����־λ
        p0006 nil                       ;O0006�ӳ����־λ
        )                               ;�ָ�Ĭ��ֵ
  (setq ffd (getfiled "д����׳����ļ�" "" "cnc" 1))
  (setq ff (open ffd "w"))              ;�ļ�ָ��
  (write-line "O0001" ff)               ;����ͷ
  (write-line "M89 P3 L1" ff)           ;����н�
  (write-line "G04 P500" ff)            ;��ͣ500����
  (while (car yz-a-x)
    (if (= 'A (caar yz-a-x))            ;�����A�����
      (progn (write-line (strcat "G00 X" (rtos (cdar yz-a-x) 2)) ff)
             (if (> (cdar yz-a-x) x-limit)
               (progn (write-line "M98 P0005" ff) ;����0005����ӳ���
                      (setq p0005 t)
                      )
               (progn (write-line "M98 P0006" ff) ;����0006����ӳ���
                      (setq p0006 t)
                      )
               )
             )
      (progn (write-line (strcat "G00 X" (rtos (cdar yz-a-x) 2)) ff)
                                        ;YZ�����
             (if (> (cdar yz-a-x) x-limit)
               (progn (write-line "M98 P0002" ff) ;����0002����ӳ���
                      (setq p0002 t)
                      )
               (progn (write-line "M98 P0003" ff) ;����0006����ӳ���
                      (setq p0003 t)
                      )
               )
             )
      )
    (setq yz-a-x (cdr yz-a-x))
    )
  (write-line "M89 P3 L0" ff)           ;�ɼ���
  (write-line "G04 P500" ff)            ;��ͣ0.5��
  (write-line "M30" ff)                 ;ѭ�����ؿ�ʼ
  (write-line "%" ff)                   ;����
  (if p0002                             ;дO0002�ӳ���
    (yz-drilling ff nil)
    )
  (if p0003                             ;дO0003�ӳ���
    (yz-drilling ff t)
    )
  (if p0005                             ;дO0005�ӳ���
    (a-drilling ff nil)
    )
  (if p0006
    (a-drilling ff t)
    )                                   ;дO0006�ӳ���
  (close ff)                            ;�ر�ָ��
  (princ)
  )