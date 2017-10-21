(defun c:sweight ()
  ;;;;;;;;;;;;;;;;;
  (defun c_js ()
    (set_tile "c_w" (rtos (swc c-d c-tt c-l) 2 3))
    )
  ;;;;;;;;

  (defun r_js()
    (set_tile "r_w" (rtos (swr r-d1 r-d2 r-tt r-l) 2 3))
    )

  (defun rb_js()
    (set_tile "rb_w" (rtos (swrb rb-d1 rb-d2 rb-tt rb-l) 2 3))
    )

  
  ;;;;;;;;;;;;;;
  (setq id (load_dialog "steel.dcl"))
  (if (< id 0)
    (exit)
  )

  (if (not (new_dialog "steel" id))
    (exit)
  )
  ;;;;;;;;;;;
  (action_tile "c_d" "(setq c-d (atof $value))")
  (action_tile "c_tt" "(setq c-tt (atof $value))")
  (action_tile "c_l" "(setq c-l (atof $value))")
  (action_tile "c_js" "(c_js)")
  ;;;;;;;;;;;;
  (action_tile "r_d1" "(setq r-d1 (atof $value))")
  (action_tile "r_d2" "(setq r-d2 (atof $value))")
  (action_tile "r_tt" "(setq r-tt (atof $value))")
  (action_tile "r_l" "(setq r-l (atof $value))")
  (action_tile "r_js" "(r_js)")
  ;;;;;;;;;;;;
  (action_tile "rb_d1" "(setq rb-d1 (atof $value))")
  (action_tile "rb_d2" "(setq rb-d2 (atof $value))")
  (action_tile "rb_tt" "(setq rb-tt (atof $value))")
  (action_tile "rb_l" "(setq rb-l (atof $value))")
  (action_tile "rb_js" "(rb_js)")
  ;;;;;;;;;;;;;
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq std (start_dialog))
  (unload_dialog id)
  
)





(defun swc (d tt l)
  ;;计算热镀锌圆管重量kg
  ;;d外径mm, tt厚度mm, l长度m
  ;;镀锌系数1.03
  (* 0.02466
     tt
     (- d tt)
     l
     1.05
  )
)
(defun swr (d1 d2 tt l)
  ;;计算热镀锌矩管重量kg
  ;;d1边长mm, d2边长mm, tt厚度mm, l长度m
  ;;镀锌系数1.02
  (* 2
     tt
     (- (+ d1 d2) tt)
     l
     7.85e-3
     1.02
  )
)
(defun swrb (d1 d2 tt l)
  ;;计算黑矩管重量kg
  ;;d1边长mm, d2边长mm, tt厚度mm, l长度m
  (* 2
     tt
     (- (+ d1 d2) tt)
     l
     7.85e-3
  )
)