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

let create_find_method =
	[|
		I_const (r2, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r3, r0, r2); (*retrieve value of this integer*)

		I_has_tab (r4, r3, r1);
		I_if_zero (r4, 2);

		I_rd_tab (r4, r3, r1);
		I_ret r4;

		I_const (r2, (`L_Str "Key not in Map Object"));
		I_halt r2
	|]
;;

let create_insert_method =
	[|
		I_const (r3, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r4, r0, r3); (*retrieve value of this integer*)
		I_wr_tab (r4, r1, r2);
		I_rd_glob (r1, Const.const_null);
		I_ret r1
	|]
;;

let create_has_key_method =
	[|
		I_const (r2, Const.const_contents); (*for getting value of int object*)
		I_rd_tab (r3, r0, r2); (*retrieve value of this integer*)

		I_has_tab (r4, r3, r1);
		I_if_zero (r4, 12);

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

let create_iter_method =
  (*
  	EmeraldVM Args:
	    r0 - self
	    r1 - should be the object defining the call method
		
		Call the built-in 'iter' function to run over the
		table stored in contents, and to repeatedly invoke
		the my_iter function (in order to re-order the inputs,
		see my_iter below for more info).
  *)

  [|
    (* retrieve the table of contained within this map *)
		I_const (r2, Const.const_contents);
		I_rd_tab (r3, r0, r2);

		I_const (r4, `L_Id "my_iter");

    (* copy the map object as input to iter *)
    I_mov (r5, r1);
    (*
			iter(contents of map, my_iter, callable object)
    *)
		I_const (r2, (`L_Id "iter"));
		I_call (r2, 3, 5);

		I_rd_glob (r1, Const.const_null);
		I_ret r1
	|]
;;

(*********************************************************************)

let my_iter = 
	(*
		EmeraldVM Args:
			r0 - key
			r1 - value
			r2 - callable object

		This method will be repeatadly called by the built-in, emeraldvm 'iter' function, with
		the initial call to the built-in iter coming from the Map.iter method. The purpose
		of this method is to re-order the input from prior to invoking the 'call' method...
	
		Explanation:
		The iter function takes arguments as (table, function_to_call, input), and then
		invokes function_to_call(r0=key, r1=value, r2=input)... The compilation of Emerald
		depends on the fact that the 'self' object is contained in r0. Thus, all arguments
		to a method invokation are placed in r1, r2, .., rN. This convention is incopatible
		with iter becuase iter places the key into r0 (thus overwriting the self object).

		To overcome this issue, Map.iter invokes iter(Map.contents, my_iter, Callable_Object),
		and then each call to the my_iter function invokes Callable_Object(key, value). 

		Note: You could probably also overcome the issue by overwriting the built in functions
		for :start_iter and :iter within EmeraldVM to automatically perform the re-order. (but that
		would be one of the hackiest hacks ever)
	*)
	[|
		(*
			Map (r0, r1, r2) -> (r1, r2, r0)
		*)
		I_mov (r3, r0);
		I_mov (r0, r2);
		I_mov (r2, r1);
		I_mov (r1, r3);

		(* TODO: get superclass method if not exist *)
		(* Get the call method from the callable object *)
		I_const (r3, Const.const_vtable);
		I_rd_tab (r3, r0, r3);
		I_const (r4, (`L_Str "call"));
    I_rd_tab (r3, r3, r4);

    I_call (r3, 0, 2);
    I_ret r0
	|]
;;

(*********************************************************************)

let create_map_class vm_prog =

	Hashtbl.replace vm_prog "Map___find" create_find_method;
	Hashtbl.replace vm_prog "Map___insert" create_insert_method;
	Hashtbl.replace vm_prog "Map___has_key" create_has_key_method;
	Hashtbl.replace vm_prog "Map___iter" create_iter_method;

	[|
		I_mk_tab r0; (*Create a table mapping method names to rubevm function names*)

		I_const (r1, Const.const_sup); (*key for mapping superclass*)
		I_const (r2, (`L_Str "Object")); (*superclass name for mapping superclass*)
		I_wr_tab (r0, r1, r2); (*map #sup->superclass in method table*)

		I_const (r1, (`L_Str "has"));  (*equals method name*)
		I_const (r2, (`L_Id "Map___has_key")); (*equals function name*)
		I_wr_tab (r0, r1, r2); (*map equals*)

		I_const (r1, (`L_Str "insert")); (*to_s method name*)
		I_const (r2, (`L_Id "Map___insert")); (*to_s function name*)
		I_wr_tab (r0, r1, r2); (*map to_s*)

		I_const (r1, (`L_Str "find")); (*print method name*)
		I_const (r2, (`L_Id "Map___find")); (*print function name*)
		I_wr_tab (r0, r1, r2); (*map print*)

		I_const (r1, (`L_Str "iter")); (*plus method name*)
		I_const (r2, (`L_Id "Map___iter")); (*plus function name*)
		I_wr_tab (r0, r1, r2); (*map plus*)

		I_rd_glob (r3, Const.const_class_table); (*gets the class table*)
		I_const (r4, Const.const_map); (*loads Object=class name into register*)
		I_wr_tab (r3, r4, r0) (*Maps Object->tabl mapping method to function names*)
	|]

;;

