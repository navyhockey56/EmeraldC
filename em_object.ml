open Instr
open Const 
open Helpers
(*********************************************************************)

let r0 = `L_Reg 0;;
let r1 = `L_Reg 1;;
let r2 = `L_Reg 2;;
let r3 = `L_Reg 3;;
let r4 = `L_Reg 4;;
let r5 = `L_Reg 5;;

(*********************************************************************)

let create_equal_method = 
	[| (*code for equals methods*) 
		I_eq (r0, r0, r1); (*checks pointer equality*)
		I_if_zero (r0, 12);
		
		I_rd_glob (r0, Const.const_class_table);
		I_const (r1, Const.const_int);
		I_rd_tab (r2, r0, r1); (*get the method table for integers*)
			
		I_mk_tab r3; (*create the table representing the integer*)
				
		I_const (r4, Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab (r3, r4, r1); (*map #class->Integer*)
				
		I_const (r4, Const.const_vtable); (*for mapping in the methods table of Integer*)
		I_wr_tab (r3, r4, r2); (*map #vtable->method table for the integer*)
				
		I_const (r4, Const.const_contents); (*key for mapping in the value of Integer*)
		I_const (r5, (`L_Int 1)); (*value for mapping in the value of Integer*)
		I_wr_tab (r3, r4, r5); (*map #contents->Int value of n*)
		I_ret r3; (*return the created integer*)
			
		I_rd_glob (r1, Const.const_null);
		I_ret r1
	|] 
;;

let create_to_s_method = 
	[|
		I_rd_glob (r0, Const.const_class_table);
		I_const (r1, Const.const_string);
		I_rd_tab (r2, r0, r1); (*get the method table for integers*)
				
		I_mk_tab r3; (*create the table representing the integer*)
				
		I_const (r4, Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab (r3, r4, r1); (*map #class->Integer*)
				
		I_const (r4, Const.const_vtable); (*for mapping in the methods table of Integer*)
		I_wr_tab (r3, r4, r2); (*map #vtable->method table for the integer*)
				
		I_const (r4, Const.const_contents); (*key for mapping in the value of Integer*)
		I_const (r5, (`L_Str "")); (*value for mapping in the value of Integer*)
		I_wr_tab (r3, r4, r5); (*map #contents->Int value of n*)
		I_ret r3; (*return the created integer*)
	|]
;;

let create_print_method = 
	[|
		I_rd_glob (r1, Const.const_null);
		I_ret r1
	|]
;;

(*********************************************************************)

let create_object_class vm_prog =
	
	Hashtbl.replace vm_prog "Object___equal" create_equal_method;
	Hashtbl.replace vm_prog "Object___to_s" create_to_s_method; 
	Hashtbl.replace vm_prog "Object___print" create_print_method;
	
	[|
		I_mk_tab r0; (*Create a table mapping method names to rubevm function names*)
		
		I_const (r1, (`L_Str "equal?"));  (*equals method name*)
		I_const (r2, (`L_Id "Object___equal")); (*equals function name*)
		I_wr_tab (r0, r1, r2); (*map equals*)
		
		I_const (r1, (`L_Str "to_s")); (*to_s method name*)
		I_const (r2, (`L_Id "Object___to_s")); (*to_s function name*)
		I_wr_tab (r0, r1, r2); (*map to_s*)
		
		I_const (r1, (`L_Str "print")); (*print method name*)
		I_const (r2, (`L_Id  "Object___print")); (*print function name*)
		I_wr_tab (r0, r1, r2); (*map print*)
		
		I_rd_glob (r3, Const.const_class_table); (*gets the class table*)
		I_const (r4, Const.const_obj); (*loads Object=class name into register*)
		I_wr_tab (r3, r4, r0) (*Maps Object->tabl mapping method to function names*)
	|] 
;;

