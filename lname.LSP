                                        ;获取A4布局竖向、横向，A3布局横向标题栏块的属性“图名”，并赋值给布局名字
                                        ;通过选择集获取当前标题栏
                                        ;图名属性要确保第一顺序
                                        ;编辑：2014-08-12

(defun c:lname
       (/ layout paper rotation p1 p2 biaoti biaoti-pro-list lname)
  (VL-LOAD-COM)
  (setq layout   (vla-get-layout
                   (vla-get-PaperSpace
                     (vla-get-ActiveDocument (vlax-get-acad-object))))
                                        ;layout 当前布局
        paper    (vla-get-CanonicalMediaName layout) ;获取纸张大小
        rotation (vla-get-PlotRotation layout)) ;获取布局方向
  (cond ((and (= paper "A4") (or (= rotation 0) (= rotation 2)))
                                        ;A4竖向
         (setq p1 '(170 10)
               p2 '(120 30)))           ;确定ssget选取标题栏位置
        ((and (= paper "A4") (or (= rotation 3) (= rotation 4)))
                                        ;A4横向
         (setq p1 '(235 10)
               p2 '(255 12)))
        ((= paper "A3")                 ;A3
         (setq p1 '(320 10)
               p2 '(325 12)))           ;确定ssget选取标题栏位置
        (t exit))
  (setq biaoti          (vlax-ename->vla-object (ssname (ssget "C" p1 p2) 0))
                                        ;选取标题栏，并转换为Activex对象
        biaoti-pro-list (vlax-safearray->list
                          (vlax-variant-value (vla-GetAttributes biaoti)))
                                        ;标题栏属性列表
        lname           (vlax-get-property (nth 0 biaoti-pro-list) 'TextString))
                                        ;提取第一个属性值，图名
  (command "layout" "r" "" lname)) ;重命名

