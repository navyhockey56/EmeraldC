open Instr

let const_class_table = `L_Id "__ClassTable";;
let const_null = `L_Id "___NULL";;

let const_class = `L_Str "#class";;
let const_vtable = `L_Str "#vtable";;
let const_contents = `L_Str "#contents";;
let const_sup = `L_Str "#sup";;

let class_name_obj = "Object";;
let class_name_int = "Integer";;
let class_name_string = "String";;
let class_name_map = "Map";;
let class_name_bot = "Bot";;

let const_self = "self";;

let const_obj = `L_Str class_name_obj;;
let const_int = `L_Str class_name_int;;
let const_string = `L_Str class_name_string;;
let const_map = `L_Str class_name_map;;
let const_bot = `L_Str class_name_bot;;
