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
let r6 = `L_Reg 6;;
let r7 = `L_Reg 7;;
let r8 = `L_Reg 8;;
let r9 = `L_Reg 9;;
let r10 = `L_Reg 10;;

(*********************************************************************)

let create_equal_method =
	(*
		TODO: You need to add a nil check in here!
	*)
	[|
		I_rd_glob (r2, Const.const_null);
		I_eq(r2, r1, r2);
		I_if_zero(r2, 1);
		I_ret r1;

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
	[| (*code for to_s method*)
		(*ASSUMPTIONS:
			`L_Reg 0 will contain 'self'
		*)
		I_const (r1, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r2, r0, r1); (*retrieve value of this integer*)

		I_const (r1, (`L_Id "to_s")); (*to_s to convert to a string*)
		I_call (r1, 2, 2); (*call to_s on the integer to convert into a string*)

		I_mov (r5, r2); (*return the string*)

		I_rd_glob (r0, Const.const_class_table);
		I_const (r1, Const.const_string);
		I_rd_tab (r2, r0, r1); (*get the method table for strings*)

		I_mk_tab r3; (*create the table representing the string*)

		I_const (r4, Const.const_class); (*for mapping in the methods table of String*)
		I_wr_tab (r3, r4, r1); (*map #vtable->method table for the string*)

		I_const (r4, Const.const_vtable); (*for mapping in the methods table of String*)
		I_wr_tab (r3, r4, r2); (*map #vtable->method table for the string*)

		I_const (r4, Const.const_contents); (*key for mapping in the value of Integer*)
		I_wr_tab (r3, r4, r5); (*map #contents->String value of str*)
		I_ret (r3)
	|]
;;

let create_print_method =
	[| (*code for print method*)
		I_const (r1, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r2, r0, r1); (*retrieve value of this integer*)

		I_const (r1, (`L_Id "print_int")); (*to_s to convert to a string*)
		I_call (r1, 2, 2); (*call to_s on the integer to convert into a string*)
		I_rd_glob (r1, Const.const_null);
		I_ret r1
	|]
;;

let create_plus_method =
	[|
		I_const (r2, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r3, r0, r2); (*retrieve value of this integer*)
		I_rd_tab (r4, r1, r2); (*retrieve value of this integer*)

		I_rd_glob (r5, Const.const_class_table);
		I_const (r6, Const.const_int);
		I_rd_tab (r7, r5, r6); (*get the method table for integers*)

		I_mk_tab r8; (*create the table representing the integer*)

		I_const (r9, Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab (r8, r9, r6); (*map #class->Integer*)

		I_const (r9, Const.const_vtable); (*for mapping in the methods table of Integer*)
		I_wr_tab (r8, r9, r7); (*map #vtable->method table for the integer*)

		I_const (r9, Const.const_contents); (*key for mapping in the value of Integer*)
		I_add (r10, r3, r4); (*value for mapping in the value of Integer*)
		I_wr_tab (r8, r9, r10); (*map #contents->Int value of n*)
		I_ret r8 (*return the created integer*)
	|]
;;

let create_minus_method =
	[|
		I_const (r2, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r3, r0, r2); (*retrieve value of this integer*)
		I_rd_tab (r4, r1, r2); (*retrieve value of this integer*)

		I_rd_glob (r5, Const.const_class_table);
		I_const (r6, Const.const_int);
		I_rd_tab (r7, r5, r6); (*get the method table for integers*)

		I_mk_tab r8; (*create the table representing the integer*)

		I_const (r9, Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab (r8, r9, r6); (*map #class->Integer*)

		I_const (r9, Const.const_vtable); (*for mapping in the methods table of Integer*)
		I_wr_tab (r8, r9, r7); (*map #vtable->method table for the integer*)

		I_const (r9, Const.const_contents); (*key for mapping in the value of Integer*)
		I_sub (r10, r3, r4); (*value for mapping in the value of Integer*)
		I_wr_tab (r8, r9, r10); (*map #contents->Int value of n*)
		I_ret r8 (*return the created integer*)
	|]
;;

let create_multiply_method =
	[|
		I_const (r2, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r3, r0, r2); (*retrieve value of this integer*)
		I_rd_tab (r4, r1, r2); (*retrieve value of this integer*)

		I_rd_glob (r5, Const.const_class_table);
		I_const (r6, Const.const_int);
		I_rd_tab (r7, r5, r6); (*get the method table for integers*)

		I_mk_tab r8; (*create the table representing the integer*)

		I_const (r9, Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab (r8, r9, r6); (*map #class->Integer*)

		I_const (r9, Const.const_vtable); (*for mapping in the methods table of Integer*)
		I_wr_tab (r8, r9, r7); (*map #vtable->method table for the integer*)

		I_const (r9, Const.const_contents); (*key for mapping in the value of Integer*)
		I_mul (r10, r3, r4); (*value for mapping in the value of Integer*)
		I_wr_tab (r8, r9, r10); (*map #contents->Int value of n*)
		I_ret r8 (*return the created integer*)
	|]
;;

let create_divide_method =
	[|
		I_const (r2, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r3, r0, r2); (*retrieve value of this integer*)
		I_rd_tab (r4, r1, r2); (*retrieve value of this integer*)

		I_rd_glob (r5, Const.const_class_table);
		I_const (r6, Const.const_int);
		I_rd_tab (r7, r5, r6); (*get the method table for integers*)

		I_mk_tab r8; (*create the table representing the integer*)

		I_const (r9, Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab (r8, r9, r6); (*map #class->Integer*)

		I_const (r9, Const.const_vtable); (*for mapping in the methods table of Integer*)
		I_wr_tab (r8, r9, r7); (*map #vtable->method table for the integer*)

		I_const (r9, Const.const_contents); (*key for mapping in the value of Integer*)
		I_div (r10, r3, r4); (*value for mapping in the value of Integer*)
		I_wr_tab (r8, r9, r10); (*map #contents->Int value of n*)
		I_ret r8 (*return the created integer*)
	|]
;;


(*********************************************************************)

let create_integer_class vm_prog =

	Hashtbl.replace vm_prog "Integer___equal?" create_equal_method;
	Hashtbl.replace vm_prog "Integer___to_s" create_to_s_method;
	Hashtbl.replace vm_prog "Integer___print" create_print_method;
	Hashtbl.replace vm_prog "Integer___plus" create_plus_method;
	Hashtbl.replace vm_prog "Integer___minus" create_minus_method;
	Hashtbl.replace vm_prog "Integer___multiply" create_multiply_method;
	Hashtbl.replace vm_prog "Integer___divide" create_divide_method;

	[|
		I_mk_tab r0; (*Create a table mapping method names to rubevm function names*)

		I_const (r1, Const.const_sup); (*key for mapping superclass*)
		I_const (r2, Const.const_obj); (*superclass name for mapping superclass*)
		I_wr_tab (r0, r1, r2); (*map #sup->superclass in method table*)

		I_const (r1, (`L_Str "equal?"));  (*equals method name*)
		I_const (r2, (`L_Id "Integer___equal?")); (*equals function name*)
		I_wr_tab (r0, r1, r2); (*map equals*)

		I_const (r1, (`L_Str "to_s")); (*to_s method name*)
		I_const (r2, (`L_Id "Integer___to_s")); (*to_s function name*)
		I_wr_tab (r0, r1, r2); (*map to_s*)

		I_const (r1, (`L_Str "print")); (*print method name*)
		I_const (r2, (`L_Id "Integer___print")); (*print function name*)
		I_wr_tab (r0, r1, r2); (*map print*)

		I_const (r1, (`L_Str "+")); (*plus method name*)
		I_const (r2, (`L_Id "Integer___plus")); (*plus function name*)
		I_wr_tab (r0, r1, r2); (*map plus*)

		I_const (r1, (`L_Str "-")); (*minus method name*)
		I_const (r2, (`L_Id "Integer___minus")); (*minus function name*)
		I_wr_tab (r0, r1, r2); (*map minus*)

		I_const (r1, (`L_Str "*")); (*multiply method name*)
		I_const (r2, (`L_Id "Integer___multiply")); (*multiply function name*)
		I_wr_tab (r0, r1, r2); (*map multiply*)

		I_const (r1, (`L_Str "/")); (*divide method name*)
		I_const (r2, (`L_Id "Integer___divide")); (*divide function name*)
		I_wr_tab (r0, r1, r2); (*map divide*)

		I_rd_glob (r3, Const.const_class_table); (*gets the class table*)
		I_const (r4, Const.const_int); (*loads Object=class name into register*)
		I_wr_tab (r3, r4, r0) (*Maps Object->tabl mapping method to function names*)
	|]

;;


