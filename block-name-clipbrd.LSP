(defun c:bnc ()
  ;;ѡȡһ���飬�������Ƶ�������
  ;;ʹ����DosLib�Ŀ�
  (vl-load-com)
  ;(arxload "DOSLib19.arx") ;����DosLib �����CAD�汾ѡ����Ӧ�İ汾
  (dos_clipboard
    (vlax-get-property
      (vlax-ename->vla-object (car (entsel "ѡȡ�飬�������Ƶ�������: ")))
      'effectivename)
    )
  (princ))
  