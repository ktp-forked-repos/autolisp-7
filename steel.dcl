steel:dialog{
	label="钢材重量计算";
	:row{
	:boxed_row{label="热镀锌圆管";
	:column{
		:edit_box{
			label="外径(mm):";
			key="c_d";
			edit_width=5;
			}
		:edit_box{
			label="壁厚(mm):";
			key="c_tt";
			edit_width=5;
			}
		:edit_box{
			label="长度(m):";
			key="c_l";
			edit_width=5;}
		:row{
		:text{label="质量(kg):";
		}
		:text{key="c_w";
		  	width=5;}}
		}
	
	:button{label="计算";
		key="c_js";}
	}

	:boxed_row{label="热镀锌矩管";
	:column{
		:edit_box{
			label="边长1(mm):";
			key="r_d1";
			edit_width=5;
			}
		:edit_box{
			label="边长2(mm):";
			key="r_d2";
			edit_width=5;
			}
		:edit_box{
			label="壁厚(mm):";
			key="r_tt";
			edit_width=5;
			}
		:edit_box{
			label="长度(m):";
			key="r_l";
			edit_width=5;}
		:row{
		:text{label="质量(kg):";
		}
		:text{key="r_w";width=5;}}
		}
	
	:button{label="计算";
		key="r_js";}
	}

	:boxed_row{label="矩管（黑件）";
	:column{
		:edit_box{
			label="边长1(mm):";
			key="rb_d1";
			edit_width=5;
			}
		:edit_box{
			label="边长2(mm):";
			key="rb_d2";
			edit_width=5;
			}
		:edit_box{
			label="壁厚(mm):";
			key="rb_tt";
			edit_width=5;
			}
		:edit_box{
			label="长度(m):";
			key="rb_l";
			edit_width=5;}
		:row{
		:text{label="质量(kg):";
		}
		:text{key="rb_w";width=5;}}
		}
	
	:button{label="计算";
		key="rb_js";}
	}
	}
ok_only;
}