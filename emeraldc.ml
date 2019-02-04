open Instr
open Ast 

open Const 
open Helpers

open Em_object
open Em_string
open Em_integer
open Em_map
open Em_bot

(*********************************************************************)

let retrieve_return instructions = 
	match (Array.get instructions ((Array.length instructions) - 1)) with 
	| I_ret reg -> (reg, (Array.sub instructions 0 ((Array.length instructions) - 1)))
	| _ -> failwith "return was not the last instruction"
;;

let create_new_object class_name new_reg = 
	let r0 = new_reg () in 
	let r1 = new_reg () in 
	let r2 = new_reg () in 
	let r3 = new_reg () in 
	let r4 = new_reg () in 
	
	[|
		I_rd_glob ((`L_Reg r0), Const.const_class_table);
		I_const ((`L_Reg r1), (`L_Str class_name));
		I_rd_tab ((`L_Reg r2), (`L_Reg r0), (`L_Reg r1)); (*get the method table for strings*)
			
		I_mk_tab (`L_Reg r3); (*create the table representing the string*)
			
		I_const ((`L_Reg r4), Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r1)); (*map #class->Integer*)
			
		I_const ((`L_Reg r4), Const.const_vtable); (*for mapping in the methods table of String*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r2)); (*map #vtable->method table for the string*)
		
		I_ret (`L_Reg r3)
	|]
;;

let create_new_object_with_contents class_name new_reg contents =
	let instructions = create_new_object class_name new_reg in 
	let return_reg, instructions = retrieve_return instructions in 

	let r1 = new_reg () in 
	let r2 = new_reg () in  

	Array.append instructions 
	[|
		I_const ((`L_Reg r1), Const.const_contents);
		I_const ((`L_Reg r2), contents);
		I_wr_tab (return_reg, (`L_Reg r1), (`L_Reg r2)); 
		I_ret return_reg
	|]

;;

(*  
	Create the instructions for an integer.
*)
let compile_eint n new_reg =
	create_new_object_with_contents Const.class_name_int new_reg (`L_Int n)
;;

(*
	Create instructions for nil
*)
let compile_enil new_reg = 
	let r0 = new_reg () in 
	[|
		I_rd_glob ((`L_Reg r0), Const.const_null);
		I_ret (`L_Reg r0)
	|]
;;

(*
	Create instructions for self object.
	If the self object is not mapped in the local environment, 
	then return the null object
*)
let compile_eself new_reg local_env = 
	if (Hashtbl.mem local_env const_self) then (* self is mapped *)
		[| I_ret (Hashtbl.find local_env const_self) |]
	else (* self is not mapped *)
		compile_enil new_reg
;;

(*
	Create instructions for a raw string
*)
let compile_estring str new_reg = 
	create_new_object_with_contents Const.class_name_string new_reg (`L_Str str)
;;

(*
	Create instructions for a variable read  
*)
let compile_elocrd id new_reg local_env = 
	if (Hashtbl.mem local_env id) then (* the variable is bound *)
		[| I_ret (Hashtbl.find local_env id) |]
	else (* the variable is not bound, return nil *)
		compile_enil new_reg
;;

(*
	Create instructions for a field read
*)
let compile_eflrd id new_reg local_env = 
	let r0 = `L_Reg (new_reg ()) in 
	let r1 = `L_Reg (new_reg ()) in 
	let self = Hashtbl.find local_env const_self in
	[|
		I_const (r0, (`L_Str id));
		I_has_tab (r1, self, r0);
		I_if_zero (r1, 2);
			
		I_rd_tab (r1, self, r0);
		I_jmp 1;
			
		I_rd_glob (r1, Const.const_null);
		I_ret r1; 
	|]
;;

(*
	Create instructions for a new map object
*)
let compile_new_map new_reg = 
	let r0 = new_reg () in 
	let r1 = new_reg () in 
	let r2 = new_reg () in 
	let r3 = new_reg () in 
	let r4 = new_reg () in 
	let r5 = new_reg () in 
		
	[|
		I_rd_glob ((`L_Reg r0), Const.const_class_table);
		I_const ((`L_Reg r1), Const.const_map);
		I_rd_tab ((`L_Reg r2), (`L_Reg r0), (`L_Reg r1)); (*get the method table for integers*)
			
		I_mk_tab (`L_Reg r3); (*create the table representing the integer*)
			
		I_const ((`L_Reg r4), Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r1)); (*map #class->Integer*)
			
		I_const ((`L_Reg r4), Const.const_vtable); (*for mapping in the methods table of Integer*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r2)); (*map #vtable->method table for the integer*)
			
		I_const ((`L_Reg r4), Const.const_contents); (*key for mapping in the value of Integer*)
		I_mk_tab (`L_Reg r5); (*value for mapping in the value of Integer*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r5)); (*map #contents->Int value of n*)

		I_ret (`L_Reg r3) (*return the created integer*)
	|]
;;

(*
	Throws an error if someone tries to create a new Bot Object.
*)
let compile_new_bot () = 
	failwith "Access forbidden - Cannot instratiate a new Bot Object."
;;

let rec compile_exp exp new_reg local_env = 
	match exp with  
  | EInt n -> 
  	compile_eint n new_reg
		
  | ENil -> 
		compile_enil new_reg
  
  | ESelf -> 
		compile_eself new_reg local_env
  
  | EString str -> 
		compile_estring str new_reg
  
  | ELocRd id (*string*) -> 
  	compile_elocrd id new_reg local_env
  
  | ELocWr (id, exp) (*string * expr*) ->  (* Write a local variable *) 
		compile_elocwr id exp new_reg local_env
		
  | EFldRd id (*string*) -> 
		compile_eflrd id new_reg local_env
			
  | EFldWr (id, exp) (*string * expr*) ->  
  	compile_eflwr id exp new_reg local_env
  
  | EIf (exp1, exp2, exp3) (*expr * expr * expr*) ->  
		compile_eif exp1 exp2 exp3 new_reg local_env
		
  | EWhile (exp1, exp2) (*expr * expr*) ->
		compile_ewhile exp1 exp2 new_reg local_env
		
  | ESeq (exp1, exp2) (*expr * expr*) -> 
  	compile_eseq exp1 exp2 new_reg local_env
	
  | ENew "String" -> 	
  	compile_estring "" new_reg
		
  | ENew "Integer" -> 	
		compile_eint 0 new_reg
		
	| ENew "Map" -> 	
		compile_new_map new_reg
		
  | ENew "Bot" -> 
  	compile_new_bot ()
  
  | ENew class_name (*string*) -> 
  	create_new_object class_name new_reg
		
  | EInstanceOf (obj, class_name) (*expr * string*) -> 
		compile_einstanceof obj class_name new_reg local_env
		
  | EInvoke (obj, meth_name, arg_lst) (*expr * string * (expr list)*) -> 
		compile_einvoke obj meth_name arg_lst new_reg local_env


and compile_elocwr id exp new_reg local_env = 
	let code_of_exp = compile_exp exp new_reg local_env in
		
	if (Hashtbl.mem local_env id) then (* This variable already exists  *)
		let reg_of_obj, code_of_exp = retrieve_return code_of_exp in
		Array.append code_of_exp 
		[|
			I_mov ((Hashtbl.find local_env id), reg_of_obj); 
			I_ret (Hashtbl.find local_env id)
		|]
	else ( (* This is a new variable *)
		(*get the register of the returned object*)
		let reg_of_obj, _ = retrieve_return code_of_exp in
		(* map the variable name to the objects register *)
		Hashtbl.replace local_env id reg_of_obj;
		(*return the generated code*)
		code_of_exp 
	)

and compile_eflwr id exp new_reg local_env = 
	let code_of_exp = compile_exp exp new_reg local_env in 
	let exp_obj, code_of_exp = retrieve_return code_of_exp in 
		
	let r0 = `L_Reg (new_reg ()) in 
	let self = Hashtbl.find local_env const_self in
	
	Array.append code_of_exp 
	[|
		I_const (r0, (`L_Str id));
		I_wr_tab (self, r0, exp_obj);
		I_ret exp_obj
	|]

and compile_eif exp1 exp2 exp3 new_reg local_env = 
	let code_of_exp1 = compile_exp exp1 new_reg local_env in 
	let exp_obj1, code_of_exp1 = retrieve_return code_of_exp1 in 
		
	let code_of_exp2 = compile_exp exp2 new_reg local_env in 
	let exp_obj2, code_of_exp2 = retrieve_return code_of_exp2 in 
		
	let code_of_exp3 = compile_exp exp3 new_reg local_env in 
	let exp_obj3, code_of_exp3 = retrieve_return code_of_exp3 in 

	let r0 = `L_Reg (new_reg ()) in 
	let r1 = `L_Reg (new_reg ()) in 
	let r2 = `L_Reg (new_reg ()) in 
	let r3 = `L_Reg (new_reg ()) in 
	let r5 = `L_Reg (new_reg ()) in 
		
	let instructions1 = [|
		I_const (r0, Const.const_class);
		I_rd_tab (r1, exp_obj1, r0);
		I_const (r2, Const.const_bot);
			
		I_eq (r3, r1, r2);
		I_if_zero (r3, ((Array.length code_of_exp3) + 2));
	|] in  
		
	let instructions = Array.append code_of_exp1 instructions1 in 
		
	let instructions = Array.append instructions code_of_exp3 in 
	let instructions1 = [|
		I_mov (r5, exp_obj3);
		I_jmp ((Array.length code_of_exp2) + 1) 
	|] in 
		
	let instructions = Array.append instructions instructions1 in 
	let instructions = Array.append instructions code_of_exp2 in 
	let instructions1 = [|
		I_mov (r5, exp_obj2);
		I_ret r5
	|] in 
	Array.append instructions instructions1

and compile_ewhile exp1 exp2 new_reg local_env =
	let code_of_exp1 = compile_exp exp1 new_reg local_env in 
	let exp_obj1, code_of_exp1 = retrieve_return code_of_exp1 in 
		
	let code_of_exp2 = compile_exp exp2 new_reg local_env in 
	let exp_obj2, code_of_exp2 = retrieve_return code_of_exp2 in 
		
	let r0 = `L_Reg (new_reg ()) in 
	let r1 = `L_Reg (new_reg ()) in 
	let r2 = `L_Reg (new_reg ()) in 
	let r3 = `L_Reg (new_reg ()) in 
	let r4 = `L_Reg (new_reg ()) in 
		
	let instructions = [|
		I_const (r0, Const.const_class);
		I_rd_tab (r1, exp_obj1, r0);
		I_const (r2, Const.const_bot);
			
		I_eq (r3, r1, r2);
		I_const (r4, `L_Int 1);
		I_sub (r3, r3, r4);
		I_if_zero (r3, ((Array.length code_of_exp2) + 1));
	|] in  
		
	let instructions = Array.append code_of_exp1 instructions in 
	let instructions = Array.append instructions code_of_exp2 in 
	let instructions = Array.append instructions [|I_jmp ((-1)*((Array.length code_of_exp2) + (Array.length code_of_exp1) + 8))|] in 
		
	let instructions1 = [|
		I_ret exp_obj1
	|] in 
	Array.append instructions instructions1

and compile_eseq exp1 exp2 new_reg local_env = 
	let code_of_exp1 = compile_exp exp1 new_reg local_env in (*generate code for exp1*)
	let _, code_of_exp1 = retrieve_return code_of_exp1 in  (*remove the return statement*)
	let code_of_exp2 = compile_exp exp2 new_reg local_env in (*generate the code for exp2*)
	Array.append code_of_exp1 code_of_exp2 (*create a single array with code for both exps*)

and compile_einstanceof obj class_name new_reg local_env = 
	let code_of_exp = compile_exp obj new_reg local_env in (*generate code for exp1*)
	let reg_exp, instructions = retrieve_return code_of_exp in  (*remove the return statement*)
		
	let r0 = new_reg () in 
	let r1 = new_reg () in 
	let r2 = new_reg () in 
	let r3 = new_reg () in 
	let r4 = new_reg () in 
	let r5 = new_reg () in 

	Array.append instructions [|
		I_const ((`L_Reg r0), (`L_Str class_name));
		I_const ((`L_Reg r1), Const.const_class);
		I_rd_tab ((`L_Reg r1), reg_exp, (`L_Reg r1));
		
		I_eq ((`L_Reg r0), (`L_Reg r1), (`L_Reg r0));
		I_if_zero ((`L_Reg r0), 13);
			
		I_rd_glob ((`L_Reg r0), Const.const_class_table);
		I_const ((`L_Reg r1), Const.const_int);
		I_rd_tab ((`L_Reg r2), (`L_Reg r0), (`L_Reg r1)); (*get the method table for integers*)
			
		I_mk_tab (`L_Reg r3); (*create the table representing the integer*)
			
		I_const ((`L_Reg r4), Const.const_class); (*for mapping in the class name of Integer*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r1)); (*map #class->Integer*)
			
		I_const ((`L_Reg r4), Const.const_vtable); (*for mapping in the methods table of Integer*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r2)); (*map #vtable->method table for the integer*)
			
		I_const ((`L_Reg r4), Const.const_contents); (*key for mapping in the value of Integer*)
		I_const ((`L_Reg r5), (`L_Int 1)); (*value for mapping in the value of Integer*)
		I_wr_tab ((`L_Reg r3), (`L_Reg r4), (`L_Reg r5)); (*map #contents->Int value of n*)
		I_mov ((`L_Reg r0), (`L_Reg r3)); (*return the created integer*)
		I_jmp 1; 
			
		I_rd_glob ((`L_Reg r0), Const.const_null);
		I_ret (`L_Reg r0)	
	|] 

and compile_einvoke obj meth_name arg_lst new_reg local_env = 
	let instructions = compile_exp obj new_reg local_env in 
	let obj_reg, instructions = retrieve_return instructions in 
	(*
		for each expr in arg_lst
			generate code of expr
			get the return register, add to list
			append the created code to a list
		for each register in list 
			move the register into a fresh one so that returns are placed in sequential registers
		call the function
	*)
	let register_list, instructions = List.fold_left (fun (register_list,instructions) expr -> 
		let code_of_exp = compile_exp expr new_reg local_env in 
		let reg_of_obj, code_of_exp = retrieve_return code_of_exp in 
		let instructions = Array.append instructions code_of_exp in 
		let register_list = register_list@[reg_of_obj] in 
		(register_list,instructions) 
	) ([], instructions) arg_lst in 
			
	let starting_position_of_arguments = new_reg () in 
	let instructions = Array.append instructions [|I_mov ((`L_Reg starting_position_of_arguments), obj_reg)|] in 
		
	let instructions = List.fold_left (fun instructions reg -> 
		let new_reg_pos = new_reg () in 
		Array.append instructions [|I_mov ((`L_Reg new_reg_pos), reg)|]
	) instructions register_list in 
		
	let val_r0 = new_reg () in
	let r0 = `L_Reg val_r0 in 
	let r1 = `L_Reg (new_reg ()) in
	let r3 = `L_Reg (new_reg ()) in
	let r4 = `L_Reg (new_reg ()) in
	let r5 = `L_Reg (new_reg ()) in
	let r6 = `L_Reg (new_reg ()) in
		
	Array.append instructions [|
		I_const (r0, Const.const_vtable);
		I_const (r1, (`L_Str meth_name));
		I_const (r5, Const.const_sup);
		I_rd_glob (r6, Const.const_class_table);
					
		I_rd_tab (r3, (`L_Reg starting_position_of_arguments), r0); (*get the method table*) 
			
		I_has_tab(r4, r3, r1); (*checks if the method exists. *JUMP BACK TO HERE* *)
		I_if_zero(r4, 3); (*branch to checking superclass*)
		
		(*the method belonds to the current class*)
		I_rd_tab (r4, r3, r1); (*get the function name*)
		I_call (r4, starting_position_of_arguments, (val_r0 - 1)); (*call the function, result will be in start_pos*)
		I_jmp 7; (*jump to the return statement*)
			
		I_has_tab (r4, r3, r5); (*check for superclass - object has no superclass -> method DNE*)
		I_if_zero (r4, 3); (*jumps to halt instruction for method DNE.*)
			
		(*load the superclasses class table into r3 and jump back to *JUMP BACK TO HERE*
		thus searching for the method_name through each superclass until it is found, or
		the search reaches Object class and still hasn't found the method name, thus signaling it DNE*)
		I_rd_tab (r4, r3, r5); (*get the superclass name*)
		I_rd_tab(r3, r6, r4); (*get the superclass method table*)
		I_jmp (-10); (*jump back to *JUMP BACK TO HERE**)
			
		(*the method DNE, signal a halt*)
		I_const (r0, (`L_Str "No such method"));
		I_halt r0;
			
		I_ret (`L_Reg starting_position_of_arguments) (*return the result of the call*)
	|] 

;;

   
let set_up_local_env arg_lst = 
	let local_env = Hashtbl.create 17 in 
	Hashtbl.replace local_env const_self (`L_Reg 0);
	let local_env, _ = List.fold_left (fun (local_env, pos) id -> Hashtbl.replace local_env id (`L_Reg pos); (local_env, pos + 1)) 
	(local_env, 1) arg_lst in 
	local_env
;;
	
let rec call_reg new_reg n = 
	if n = (-1) then 
		new_reg 
	else (
		let _ = new_reg () 
		in call_reg new_reg (n - 1)
	)
;;

let rec create_class_aux name (vm_prog:prog) = 
	function 
	| [] -> [||]

	| h::t ->  
		let {meth_name = m_name; meth_args = args_lst; meth_body = exp} = h in 
		
		let next_reg = Helpers.next_location_func () in 
		let next_reg = call_reg next_reg (List.length args_lst) in 
		let local_env = set_up_local_env args_lst in 
		let code_of_exp = compile_exp exp next_reg local_env in (*generate code for exp*)
		let method_name = String.concat "" [name;"___"; m_name] in (*create name for method*)
		Hashtbl.replace vm_prog method_name code_of_exp; (*add the code to the vm_program*)
	
		let instrucs = [|
			I_const ((`L_Reg 1), (`L_Str m_name)); (*load the method name*)
			I_const ((`L_Reg 2), (`L_Id  method_name)); (*load the created function name in vm*)
			I_wr_tab ((`L_Reg 0), (`L_Reg 1), (`L_Reg 2)); (*map the method name to function name*)
		|] in 
		Array.append instrucs (create_class_aux name vm_prog t)
;;
		
		
let create_class cls (vm_prog:prog) = 
	let {cls_name=name; cls_super=sup; cls_meths=method_lst} = cls in (*extract info from cls*)
		
		let instructions = [|	
			I_mk_tab (`L_Reg 0); (*create the method table*)
			
			I_const ((`L_Reg 1), Const.const_sup); (*key for mapping superclass*)
			I_const ((`L_Reg 2), (`L_Str sup)); (*superclass name for mapping superclass*)
			I_wr_tab ((`L_Reg 0), (`L_Reg 1), (`L_Reg 2)); (*map #sup->superclass in method table*)
			
			I_rd_glob ((`L_Reg 3), Const.const_class_table); (*Get the classtable*)
			I_const ((`L_Reg 2), (`L_Str name)); (*load the class name for mapping method table in classtable*)
			I_wr_tab ((`L_Reg 3), (`L_Reg 2), (`L_Reg 0)); (*map method table into the classtable*)
		|] in 
		
		let add_methods_to_class_table = create_class_aux name vm_prog method_lst in 
		Array.append instructions add_methods_to_class_table
;;

let rec create_other_classes (vm_prog:prog) class_list = match class_list with 
	| [] -> [||]
	| h::t -> Array.append (create_class h vm_prog) (create_other_classes vm_prog t)
;;


let built_in_classes vm_prog = 
	let instructions1 = Em_object.create_object_class vm_prog in 
	let instructions2 = Em_string.create_string_class vm_prog in 
	let instructions3 = Em_integer.create_integer_class vm_prog in 
	let instructions4 = Em_map.create_map_class vm_prog in 
	let instructions5 = Em_bot.create_bot_class vm_prog in 
	Array.append (Array.append instructions1 instructions2) (Array.append instructions3 (Array.append instructions4 instructions5))
;;

(*
	There is only one 'Null' (much like there is only one empty set). 
	As such, the single 'Null' wil lbe stored as a global variable. 
*)
let create_null_instructions = 
 [|
		I_mk_tab (`L_Reg 0); (* create table to represent null object *)
		I_const ((`L_Reg 1), Const.const_vtable); (* for mapping methods of null *)
		I_rd_glob ((`L_Reg 2), Const.const_class_table); (* gets the classtable *)
		I_const ((`L_Reg 3), (`L_Str "Bot")); (* loads key for classtable *)
		I_rd_tab ((`L_Reg 2), (`L_Reg 2), (`L_Reg 3)); (* gets the bot/null class' method table out of the classtable *)
		I_wr_tab ((`L_Reg 0), (`L_Reg 1), (`L_Reg 2)); (* stores bot's method table into the null objects table *)
		I_const ((`L_Reg 1), (`L_Str "#class")); (* for mapping in the class name *)
		I_wr_tab ((`L_Reg 0), (`L_Reg 1), (`L_Reg 3)); (* stores bot's classname into the null objects table *)
		I_wr_glob ((`L_Id "___NULL"), (`L_Reg 0)) (* store the null object as a global variable *)
	|]
;;

(*
	There is only one 'Object' (much like there is only one empty set). 
	As such, the single 'Object' wil lbe stored as a global variable. 
*)
let create_obj_instructions = 
	[|
		I_mk_tab (`L_Reg 0); (* create table to represent object *)
		I_const ((`L_Reg 1), Const.const_vtable); (*for mapping methods of object*)
		I_rd_glob ((`L_Reg 2), Const.const_class_table); (*gets the classtable*)
		I_const ((`L_Reg 3), (`L_Str "Object")); (*loads key for classtable*)
		I_rd_tab ((`L_Reg 2), (`L_Reg 2), (`L_Reg 3)); (*gets the object class' method table out of the classtable*)
		I_wr_tab ((`L_Reg 0), (`L_Reg 1), (`L_Reg 2)); (*stores objects's method table into the objects table*)
		I_const ((`L_Reg 1), (`L_Str "#class")); (* for mapping in the class name *)
		I_wr_tab ((`L_Reg 0), (`L_Reg 1), (`L_Reg 3)); (*stores object's classname into the objects table*)
	|] 
;;

let create_my_iter = 
	[|
		I_mov ((`L_Reg 4), (`L_Reg 2));
		I_mov ((`L_Reg 5), (`L_Reg 1));
		I_mov ((`L_Reg 6), (`L_Reg 0));
		I_mov ((`L_Reg 0), (`L_Reg 4));
		I_mov ((`L_Reg 2), (`L_Reg 5));
		I_mov ((`L_Reg 1), (`L_Reg 6));
	
		I_const ((`L_Reg 3), Const.const_vtable);
		I_rd_tab ((`L_Reg 4), (`L_Reg 0), (`L_Reg 3));
		
		I_const ((`L_Reg 5), (`L_Str "move")); (*load to_s method name*)
		
		(*check if the object has a to_s method and branch accordingly*)
		I_has_tab ((`L_Reg 6), (`L_Reg 4), (`L_Reg 5));
		I_if_zero ((`L_Reg 6), 3);
		
		(*the object did have a to_s method*)
		I_rd_tab ((`L_Reg 6), (`L_Reg 4), (`L_Reg 5)); (*get the function name from the method table*)
		I_call ((`L_Reg 6), 0, 2); (*call the to_s function*)
		I_ret (`L_Reg 0); (*return the result*)
		
		(*the object did not have a to_s method*)
		I_const ((`L_Reg 7), Const.const_sup);
		I_rd_tab ((`L_Reg 6), (`L_Reg 4), (`L_Reg 7)); (*get the superclass name*)
		I_rd_glob ((`L_Reg 8), Const.const_class_table); (*get the classtable*)
		I_rd_tab ((`L_Reg 4), (`L_Reg 8), (`L_Reg 7)); (*store the superclasses class table in reg2*)
		I_jmp (-10) (*go back to check for to_s method*)
	|] 
;;

let create_call_to_s returned_obj = 
	[|
		I_mov ((`L_Reg 1), returned_obj); (*make sure the object isnt overwritten*)
		
		(*gets the method table from the object*)
		I_const ((`L_Reg 2), Const.const_vtable);
		I_rd_tab ((`L_Reg 3), (`L_Reg 1), (`L_Reg 2));
		
		I_const ((`L_Reg 4), (`L_Str "to_s")); (*load to_s method name*)
		
		(*check if the object has a to_s method and branch accordingly*)
		I_has_tab ((`L_Reg 5), (`L_Reg 3), (`L_Reg 4));
		I_if_zero ((`L_Reg 5), 5);
		
		(*the object did have a to_s method*)
		I_rd_tab ((`L_Reg 5), (`L_Reg 3), (`L_Reg 4)); (*get the function name from the method table*)
		I_call ((`L_Reg 5), 1, 1); (*call the to_s function*)
		I_const ((`L_Reg 4), Const.const_contents);
		I_rd_tab ((`L_Reg 1), (`L_Reg 1), (`L_Reg 4));
		I_ret (`L_Reg 1); (*return the result*)
		
		(*the object did not have a to_s method*)
		I_const ((`L_Reg 6), Const.const_sup);
		I_rd_tab ((`L_Reg 5), (`L_Reg 3), (`L_Reg 6)); (*get the superclass name*)
		I_rd_glob ((`L_Reg 5), Const.const_class_table); (*get the classtable*)
		I_rd_tab ((`L_Reg 3), (`L_Reg 7), (`L_Reg 5)); (*store the superclasses class table in reg2*)
		I_jmp (-12) (*go back to check for to_s method*)
	|]
;;

let create_class_table = 
	(*Create class table: maps class names to their method tables
	and stores it on the heap as a global variable accessed through the id "#ClassTable"*)
	let class_table = (`L_Reg 0) in (*register to hold the class table pointer*)
	[|
		I_mk_tab class_table; (*create the class table*)
		I_wr_glob (Const.const_class_table, class_table) (*mapping #ClassTable->class_table in the heap*)
	|] 
;;

let create_class_instructions vm_prog class_list = 
	let built_in_instr = built_in_classes vm_prog in 
	let user_def_instr = create_other_classes vm_prog class_list in 
	let main_instructions = Array.append user_def_instr built_in_instr in 
	
	(*Append the instructions for creating the null value onto the end of main*)
	let main_instructions = Array.append main_instructions create_null_instructions in 
	
	(*Append the instructions for creating the null value onto the end of main*)
	 Array.append main_instructions create_obj_instructions
;;


let create_expression_instr expr = 
	
	(* Set up the env for compilation of the expression *)
	let local_env = (Hashtbl.create 17) in 
	Hashtbl.replace local_env Const.const_self (`L_Reg 0);
	
	(* 
		Create a new new_reg maker, then increatement it
		by one so that you don't overwrite the value of `self` in
		the local env. 
	*)
	let new_reg = Helpers.next_location_func () in 
	let _ = new_reg () in 
	
	(* 
		Set up the instructions for the main expression of the program.
		Extract the return value from the main instruction so that
		it can be passed to it's to_s method, thus printing it out
		at the termination of the program.
	*)
	let expression_instructions = compile_exp expr new_reg local_env in 
	let returned_obj, expression_instructions = retrieve_return expression_instructions in 
			
	Array.append expression_instructions (create_call_to_s returned_obj)
;;

let create_main_instructions p vm_prog =
	(*extract data from p*)
	let {prog_clss=class_list; prog_main=exp} = p in 

	(* Set up the class table *)
	let main_instructions = create_class_table in 
	
	(* Set up the classes *)
	let main_instructions = Array.append main_instructions (create_class_instructions vm_prog class_list) in
	
	(* Set up the global Null object *)
	let main_instructions = Array.append main_instructions create_null_instructions in 
	
	(* Set up the global Object object*)
	let main_instructions = Array.append main_instructions create_obj_instructions in 

	(* Add what looks like a hack to iter *)
	Hashtbl.replace vm_prog "my_iter" create_my_iter; 
	
	(* Set up the expression of the main expression *)		
	Array.append main_instructions (create_expression_instr exp)
;;


let compile_prog (p:rube_prog):prog =
	
	(* Create the empty EmeraldVM program*)
	let vm_prog = Hashtbl.create 17 in 
	
	(*Map the main function to the instructions that have been generated*)
	Hashtbl.replace vm_prog "main" (create_main_instructions p vm_prog);

	(*Return the rubevm program*)
	vm_prog 
;;


