open Helpers
(*********************************************************************)

let r0 = `L_Reg 0;;
let r1 = `L_Reg 1;;
let r2 = `L_Reg 2;;
let r3 = `L_Reg 3;;
let r4 = `L_Reg 4;;
let r5 = `L_Reg 5;;

(*********************************************************************)

let create_to_s_method = 
	[| (*code for to_s method*)
		I_rd_glob (r0, Const.const_class_table);
		I_const (r1, Const.const_string);
		I_rd_tab (r2, r0, r1); (*get the method table for strings*)
			
		I_mk_tab r3; (*create the table representing the string*)
			
		I_const (r4, Const.const_class); (*for mapping in the methods table of String*)
		I_wr_tab (r3, r4, r1); (*map #vtable->method table for the string*)
			
		I_const (r4, Const.const_vtable); (*for mapping in the methods table of String*)
		I_wr_tab (r3, r4, r2); (*map #vtable->method table for the string*)
			
		I_const (r4, Const.const_contents); (*key for mapping in the value of Integer*)
		I_const (r5, (`L_Str "nil")); (*value for mapping in the value of String*)
		I_wr_tab (r3, r4, r5); (*map #contents->String value of str*)
		I_ret (r3)
	|] 
;;

let create_print_method = 
	[|
		I_rd_glob (r0, Const.const_class_table);
		I_const (r1, Const.const_string);
		I_rd_tab (r2, r0, r1); (*get the method table for strings*)
			
		I_mk_tab r3; (*create the table representing the string*)
			
		I_const (r4, Const.const_class); (*for mapping in the methods table of String*)
		I_wr_tab (r3, r4, r1); (*map #vtable->method table for the string*)
			
		I_const (r4, Const.const_vtable); (*for mapping in the methods table of String*)
		I_wr_tab (r3, r4, r2); (*map #vtable->method table for the string*)
			
		I_const (r4, Const.const_contents); (*key for mapping in the value of Integer*)
		I_const (r5, (`L_Str "nil")); (*value for mapping in the value of String*)
		I_wr_tab (r3, r4, r5); (*map #contents->String value of str*)
			
		I_const (r0,(`L_Str "nil"));
		I_const (r1,(`L_Id "print_string"));
		I_call (r1, 0, 0);
		I_ret r3 (*return the created string object*)
	|] 
;;

(*********************************************************************)

let create_bot_class vm_prog =
	
	Hashtbl.replace vm_prog "Bot___to_s" create_to_s_method;
	Hashtbl.replace vm_prog "Bot___print" create_print_method; 
		
	[|
		I_mk_tab r0; (*Create a table mapping method names to rubevm function names*)
		
		I_const (r1, Const.const_sup); (*key for mapping superclass*)
		I_const (r2, Const.const_obj); (*superclass name for mapping superclass*)
		I_wr_tab (r0, r1, r2); (*map #sup->superclass in method table*)
		
		I_const (r1, (`L_Str "to_s")); (*to_s method name*)
		I_const (r2, (`L_Id "Bot___to_s")); (*to_s function name*)
		I_wr_tab (r0, r1, r2); (*map to_s*)
		
		I_const (r1, (`L_Str "print")); (*print method name*)
		I_const (r2, (`L_Id "Bot___print")); (*print function name*)
		I_wr_tab (r0, r1, r2); (*map print*)
		
		I_rd_glob (r3, Const.const_class_table); (*gets the class table*)
		I_const (r4, (`L_Str "Bot")); (*loads Object=class name into register*)
		I_wr_tab (r3, r4, r0) (*Maps Object->tabl mapping method to function names*)
	|] 
;;

