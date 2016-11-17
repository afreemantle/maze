type action = Ast | Help | Error (*LLVM_IR | Compile*)

let help_string = (
    "Usage: ./maze [option] <source file>\n" ^
    "\n Valid Options: \n"^
    "\t -h: Help \n" ^
    "\t -c: Compile \n" ^ 
    "\t -l: Generate LLVM without checking \n" ^
    "\t -a: Print Abstract Syntax tree \n" 
)

let invalid_arg_string = ("Invalid Arguments")

let check_option = function
     "-h" -> Help, ""
    | _ as s -> Error, ""

let check_action = function  


let _ =  
    let action, filename  = 
        if Array.length Sys.argv = 1 then Help, "" 
        else if Array.length Sys.argv = 2 then check_option (Sys.argv.(1))
        else if Array.l
        else Error, ""
        List.assoc Sys.argv.(1) [ ("-a", Ast); 
                                  ("-h", Help) ] 
    (* 
    else Help in 
    let lexbuf = Lexing.from_channel stdin in 
    let ast = Parser.program Scanner.token lexbuf in 
    match action with 
        Ast -> print_string (Ast.string_of_program ast)
        | Help -> print_string help_string
*)

    in
    match action with 
        Help -> print_string help_string 
      | Ast -> print_string invalid_arg_string




    
