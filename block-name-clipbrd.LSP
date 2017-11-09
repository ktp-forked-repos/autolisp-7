(defun c:bnc ()
  ;;选取一个块，块名复制到剪贴板
  ;;使用了DosLib的库
  (vl-load-com)
  ;(arxload "DOSLib19.arx") ;加载DosLib 需根据CAD版本选择相应的版本
  (dos_clipboard
    (vlax-get-property
      (vlax-ename->vla-object (car (entsel "选取块，块名复制到剪贴板: ")))
      'effectivename)
    )
  (princ))
  