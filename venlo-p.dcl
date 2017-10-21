venlo_plan:dialog{label="文洛式联栋温室基础平面图";
 	:column{
		:edit_box{key="kuadu";
			label="跨度规格表";
			width=20;
			//value="(10.8 5)";
			}
		:edit_box{key="kaijian";
			label="开间规格表";
			width=20;
			//value="(4 10)";
			}
                :edit_box{key="jiangao";
                          label="肩高(米）";
                          width=10;
                          //value="4";
                         } 	
 	}
 
	:column{
		:toggle{label="添加端立柱";
		key="duan_lizhu";
		value=1;
	
		}
		:toggle{label="添加边立柱（8m开间使用）";
		key="bian_lizhu";
		value=0;
	
		}
		}
	:boxed_column{label="左下定位点";
	
		:button{label="光标拾取<";
			key="pick";
			}
		:edit_box{label="X:";
			width=10;
			key=p0_x;}
		:edit_box{label="Y:";
			width=10;
			key=p0_y;}
		}
	

 	
 	ok_cancel;
}