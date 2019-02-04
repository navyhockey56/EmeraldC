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
	[|
		I_const (r2, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r3, r0, r2); (*retrieve value of this integer*)
		I_rd_tab (r4, r1, r2); (*retrieve value of this integer*)
		
		I_eq (r1, r3, r4);
		I_if_zero (r1, 12);
		
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
		I_ret r0
	|]
;;

let create_print_method = 
	[| (*code for print method*) 
		I_const (r1, Const.const_contents);
		I_const (r2,(`L_Id "print_string"));
		I_rd_tab (r1, r0, r1); (*gets the string objects contents*)
		I_call (r2, 1, 1); (*call print_string on contents*)
		I_rd_glob (r1, Const.const_null);
		I_ret r1
	|]
;;

let create_plus_method = 
	[|
		I_const (r2, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r3, r0, r2); (*retrieve value of this integer*)
		I_rd_tab (r4, r1, r2); (*retrieve value of this integer*)
		
		I_const (r5, (`L_Id "concat"));
		I_call (r5, 3, 4);
		I_mov (r5, r3);
		
		I_rd_glob (r0, Const.const_class_table);
		I_const (r1, Const.const_string);
		I_rd_tab (r2, r0, r1); (*get the method table for integers*)
			
		I_mk_tab r3; (*create the table representing the integer*)
			
		I_const (r4, Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab (r3, r4, r1); (*map #class->Integer*)
			
		I_const (r4, Const.const_vtable); (*for mapping in the methods table of Integer*)
		I_wr_tab (r3, r4, r2); (*map #vtable->method table for the integer*)
			
		I_const (r4, Const.const_contents); (*key for mapping in the value of Integer*)
		I_wr_tab (r3, r4, r5); (*map #contents->Int value of n*)
		I_ret r3
	|]
;;

let create_length_method = 
	[|
		I_const (r2, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r3, r0, r2); (*retrieve value of this integer*)
		I_const (r5, (`L_Id "length"));
		I_call (r5, 3, 3);
		I_mov (r5, r3);
		
		I_rd_glob (r0, Const.const_class_table);
		I_const (r1, Const.const_int);
		I_rd_tab (r2, r0, r1); (*get the method table for integers*)
			
		I_mk_tab r3; (*create the table representing the integer*)
			
		I_const (r4, Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab (r3, r4, r1); (*map #class->Integer*)
			
		I_const (r4, Const.const_vtable); (*for mapping in the methods table of Integer*)
		I_wr_tab (r3, r4, r2); (*map #vtable->method table for the integer*)
			
		I_const (r4, Const.const_contents); (*key for mapping in the value of Integer*)
		I_wr_tab (r3, r4, r5); (*map #contents->Int value of n*)
		I_ret r3
	|]
;;

(*********************************************************************)

let create_string_class vm_prog = 
	
	Hashtbl.replace vm_prog "String___equal" create_equal_method;
	Hashtbl.replace vm_prog "String___to_s" create_to_s_method;
	Hashtbl.replace vm_prog "String___print" create_print_method;
	Hashtbl.replace vm_prog "String___plus" create_plus_method;
	Hashtbl.replace vm_prog "String___length" create_length_method;
	
	[|
		I_mk_tab r0; (*Create a table mapping method names to rubevm function names*)
		
		I_const (r1, Const.const_sup); (*key for mapping superclass*)
		I_const (r2, Const.const_obj); (*superclass name for mapping superclass*)
		I_wr_tab (r0, r1, r2); (*map #sup->superclass in method table*)
		
		I_const (r1, (`L_Str "equal?"));  (*equals method name*)
		I_const (r2, (`L_Id "String___equal")); (*equals function name*)
		I_wr_tab (r0, r1, r2); (*map equals*)
		
		I_const (r1, (`L_Str "to_s")); (*to_s method name*)
		I_const (r2, (`L_Id  "String___to_s")); (*to_s function name*)
		I_wr_tab (r0, r1, r2); (*map to_s*)
		
		I_const (r1, (`L_Str "print")); (*print method name*)
		I_const (r2, (`L_Id  "String___print")); (*print function name*)
		I_wr_tab (r0, r1, r2); (*map print*)
		
		I_const (r1, (`L_Str "+")); (*plus method name*)
		I_const (r2, (`L_Id  "String___plus")); (*plus function name*)
		I_wr_tab (r0, r1, r2); (*map plus*)
		
		I_const (r1, (`L_Str "length")); (*length method name*)
		I_const (r2, (`L_Id "String___length")); (*length function name*)
		I_wr_tab (r0, r1, r2); (*map length*)
		
		I_rd_glob (r3, (`L_Id "___ClassTable")); (*gets the class table*)
		I_const (r4, (`L_Str "String")); (*loads Object=class name into register*)
		I_wr_tab (r3, r4, r0) (*Maps Object->tabl mapping method to function names*)
	|]
;;

