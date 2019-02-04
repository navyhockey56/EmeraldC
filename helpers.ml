open Ast
open Const

let next_location_func flag = 
	let x = ref (-1) in 
	fun () -> (x := !x + 1; !x)
;;

let read_class_table class_name start_register = 
	let r0 = `L_Reg start_register in 
	let r1 = `L_Reg (start_register + 1) in 
	let r2 = `L_Reg (start_register + 2) in 
	[|
		I_rd_glob (r0, Const.const_class_table);
		I_const (r1, Const.const_int);
		I_rd_tab (r2, r0, r1);
	|]
;;

let create_integer_obj n new_reg = 
	let r0 = new_reg () in 
	let r1 = new_reg () in 
	let r2 = new_reg () in 
	let r3 = new_reg () in 
	let r4 = new_reg () in 
	let r5 = new_reg () in 
	
	let instructions = [|
		I_rd_glob ((`L_Reg r0), (`L_Id const_class_table));
		I_const ((`L_Reg r1), (`L_Str const_int));
		I_rd_tab ((`L_Reg r2), (`L_Reg r0), (`L_Reg r1)); (*get the method table for integers*)
			
		I_mk_tab (`L_Reg r3); (*create the table representing the integer*)
			
		I_const ((`L_Reg r4), (`L_Str const_class)); (*for mapping in the class name of Integer*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r1)); (*map #class->Integer*)
			
		I_const ((`L_Reg r4), (`L_Str const_vtable)); (*for mapping in the methods table of Integer*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r2)); (*map #vtable->method table for the integer*)
			
		I_const ((`L_Reg r4), (`L_Str const_contents)); (*key for mapping in the value of Integer*)
		I_const ((`L_Reg r5), (`L_Int n)); (*value for mapping in the value of Integer*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r5)); (*map #contents->Int value of n*)
		I_ret (`L_Reg r3) (*return the created integer*)
	|] in 
	
	(*Return the register containing the integer's table, and the corresponding instructions*)
	((`L_Reg r3), instructions)
;;