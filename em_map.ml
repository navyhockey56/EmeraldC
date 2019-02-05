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
    r0 - self
    r1 - should be the object defining the call method

    There will be a method 'call' on r1 that we need to send
    to the iter method. We also need to pass the Map represented
    by r0 (self).

   	the object's method will be in its method table under 'call'
    which will return the of the actual method

    You now appear to be running into a problem with the value reference being
    nil... Not sure what's going on here

		After further investigation, its clear that the v is a table (so it should be an object),
		but it doesn't have a #vtable

		Ugh there's nothing wrong with the way you're passing arguments to functions, so
		you must be misunderstanding the structure of the contents table

		OKAY! I think I have the issue figured out, the solution not so much...
		The problem is that r0 corresponds to the key object, and r1 corresponds
		to the value object. But the program right now thinks that r0 corresponds
		to self, etc.

		If iter's parameters were (input, key, value)
		instead of (key, value, input)
		then we would be able to call this correctly

		okay if you do have a my_iter method, you can call iter,
		and with iter, you can pass the third argument as the object
		that has 'call'. From my_iter, you can then call the 'call' 
		method on the object with the given key/value
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

let my_iter = 
	(*
		r0 - key
		r1 - value
		r2 - callable object
	*)
	[|
		(*
			Map (r0, r1, r2) -> (r1, r2, r0)
		*)
		I_mov (r3, r0);
		I_mov (r0, r2);
		I_mov (r2, r1);
		I_mov (r1, r3);

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

