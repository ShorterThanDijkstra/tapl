open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core

let process_input input =
  try
    let input' =
      if String.ends_with ~suffix:";" input then input else input ^ ";"
    in
    let lexbuf = Lexing.from_string input' in
    let cmds = Parser.toplevel Lexer.main lexbuf in
    let f = fun cmd -> match cmd with Eval (fi, t) -> true | _ -> false in
    let cmd = List.find_opt f cmds in
    match cmd with Some (Eval (fi, t)) -> Some t | _ -> None
  with Parsing.Parse_error -> None

let rec loop () =
  print_string "> ";
  print_flush ();
  let input = read_line () in
  if input = "exit" || input = "quit" then exit 0
  else
    let res = process_input input in
    match res with
    | Some t ->
        eval t |> printtm;
        print_newline ();
        loop ()
    | None ->
        print_endline "Syntax error.";
        loop ()

let () = loop ()
