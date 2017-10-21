                                        ;��ȡA4�������򡢺���A3���ֺ��������������ԡ�ͼ����������ֵ����������
                                        ;ͨ��ѡ�񼯻�ȡ��ǰ������
                                        ;ͼ������Ҫȷ����һ˳��
                                        ;�༭��2014-08-12

(defun c:lname
       (/ layout paper rotation p1 p2 biaoti biaoti-pro-list lname)
  (VL-LOAD-COM)
  (setq layout   (vla-get-layout
                   (vla-get-PaperSpace
                     (vla-get-ActiveDocument (vlax-get-acad-object))))
                                        ;layout ��ǰ����
        paper    (vla-get-CanonicalMediaName layout) ;��ȡֽ�Ŵ�С
        rotation (vla-get-PlotRotation layout)) ;��ȡ���ַ���
  (cond ((and (= paper "A4") (or (= rotation 0) (= rotation 2)))
                                        ;A4����
         (setq p1 '(170 10)
               p2 '(120 30)))           ;ȷ��ssgetѡȡ������λ��
        ((and (= paper "A4") (or (= rotation 3) (= rotation 4)))
                                        ;A4����
         (setq p1 '(235 10)
               p2 '(255 12)))
        ((= paper "A3")                 ;A3
         (setq p1 '(320 10)
               p2 '(325 12)))           ;ȷ��ssgetѡȡ������λ��
        (t exit))
  (setq biaoti          (vlax-ename->vla-object (ssname (ssget "C" p1 p2) 0))
                                        ;ѡȡ����������ת��ΪActivex����
        biaoti-pro-list (vlax-safearray->list
                          (vlax-variant-value (vla-GetAttributes biaoti)))
                                        ;�����������б�
        lname           (vlax-get-property (nth 0 biaoti-pro-list) 'TextString))
                                        ;��ȡ��һ������ֵ��ͼ��
  (command "layout" "r" "" lname)) ;������
