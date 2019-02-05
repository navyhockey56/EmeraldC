open Ast
open Disassembler
open Emeraldc

(*********************************************************************)

let rec output_expr o = function
  | EInt i -> Printf.fprintf o "%d" i
  | ENil -> Printf.fprintf o "nil"
  | ESelf -> Printf.fprintf o "self"
  | EString s -> Printf.fprintf o "\"%s\"" s
  | ELocRd x -> output_string o x
  | ELocWr (x, e) ->
      Printf.fprintf o "%s = (%a)" x output_expr e
  | EFldRd x -> output_string o x
  | EFldWr (x, e) ->
      Printf.fprintf o "%s = (%a)" x output_expr e
  | EIf (e1, e2, e3) ->
      Printf.fprintf o "if %a then %a else %a end" output_expr e1
	output_expr e2 output_expr e3
  | EWhile (e1, e2) ->
     Printf.fprintf o "while %a do %a end" output_expr e1 output_expr e2
  | ESeq (e1, e2) -> Printf.fprintf o "%a; %a" output_expr e1 output_expr e2
  | ENew x ->
      Printf.fprintf o "new %s" x
  | EInstanceOf (e, s) -> Printf.fprintf o "%a instanceof %s" output_expr e s 
  | EInvoke (e, m, es) ->
      Printf.fprintf o "%a.%s(%a)" output_expr e m output_exprs es

and output_exprs o = function
    [] -> ()
  | [e] -> output_expr o e
  | e::es -> Printf.fprintf o "%a, %a" output_expr e output_exprs es

and output_arg o = function
    s -> Printf.fprintf o "%s" s

and output_args o = function
  | [] -> ()
  | [a] -> output_arg o a
  | a::aa -> Printf.fprintf o "%a, %a" output_arg a output_args aa

and output_locals o = function
  | [] -> ()
  | [l] -> output_arg o l
  | l::ls -> Printf.fprintf o "%a\n%a" output_arg l output_locals ls

and output_meth o ({meth_name=name; meth_args=args; meth_body=body}:meth) =
  Printf.fprintf o "  def %s(%a)\n %a\n  end\n" name output_args args output_expr body

and output_meths o = function
    [] -> ()
  | [m] -> Printf.fprintf o "%a" output_meth m
  | m::ms -> Printf.fprintf o "%a\n%a" output_meth m output_meths ms

and output_cls o ({cls_name=name; cls_super=super; cls_meths=meths}:cls) =
  Printf.fprintf o "class %s < %s\n %a\nend\n" name super output_meths meths

and output_clss o = function
    [] -> ()
  | [c] -> Printf.fprintf o "%a" output_cls c
  | c::cs -> Printf.fprintf o "%a\n%a" output_cls c output_clss cs

and print_program ({prog_clss=clss; prog_main=main}:rube_prog) = match clss with
  | [] -> Printf.printf "%a\n" output_expr main
  | _ -> Printf.printf "%a\n%a\n" output_clss clss output_expr main
;;


let rec determine_file_name file_name = 
  let length = String.length file_name in 
  match (String.index_from_opt file_name 0 '/') with 
    | None -> String.sub file_name 0 (length - 3)
    | Some i -> 
      let file_name = String.sub file_name (i + 1) (length - i - 1) in 
      determine_file_name file_name 
;;

let parse_file name =
  let chan = open_in name in
  let lexbuf = Lexing.from_channel chan in
  let (p:rube_prog) = Parser.main Lexer.token lexbuf in
  close_in chan;
  p
;;

let main () =
  
  (* Parse the input file *)
  let input_file_name = Sys.argv.(1) in 
  let p = parse_file input_file_name in

  (* Compile the program *)
  let (p':Instr.prog) = Emeraldc.compile_prog p in

  (* Write the program to the output file *)
  let output_file_name = (determine_file_name input_file_name) ^ ".evm" in 
  let out_chan = open_out output_file_name in
  disassemble out_chan p'
;;

main ()

