venlo_plan:dialog{label="����ʽ�������һ���ƽ��ͼ";
 	:column{
		:edit_box{key="kuadu";
			label="��ȹ���";
			width=20;
			//value="(10.8 5)";
			}
		:edit_box{key="kaijian";
			label="�������";
			width=20;
			//value="(4 10)";
			}
                :edit_box{key="jiangao";
                          label="���(�ף�";
                          width=10;
                          //value="4";
                         } 	
 	}
 
	:column{
		:toggle{label="��Ӷ�����";
		key="duan_lizhu";
		value=1;
	
		}
		:toggle{label="��ӱ�������8m����ʹ�ã�";
		key="bian_lizhu";
		value=0;
	
		}
		}
	:boxed_column{label="���¶�λ��";
	
		:button{label="���ʰȡ<";
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