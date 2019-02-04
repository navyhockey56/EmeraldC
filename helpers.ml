open Instr
open Const 

let next_location_func flag = 
	let x = ref (-1) in 
	fun () -> (x := !x + 1; !x)
;;

let extract_string = function 
	| `L_String s -> s
;;