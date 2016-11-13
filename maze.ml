type action = AST | Help (*LLVM_IR | Compile*)

let help_string = (
    "Usage: maze [option] <source files>\n" ^
    "\t -h: Help \n" ^
    "\t -c: Compile \n" ^ 
    "\t -l: Generate LLVM without checking" ^
    "\t -a: Print Abstract Syntax tree" 
)

let _ =  
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-a", AST); 
                                  ("-h", Help) ] 
                                 (*
                                  ("-l", LLVM_IR)  Generate LLVM, don't check 
                                  ("-c", Compile) Generate and check LLVM IR 
                                 *) 
    else Help in 
    let lexbuf = Lexing.from_channel stdin in 
    let ast = Parser.program Scanner.token lexbuf in 
    match action with 
        Ast -> print_string (Ast.string_of_program ast)
        | Help -> print_string help_string




    
