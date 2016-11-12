type action = AST | LLVM_IR | Compile | Help

let get-action = function 
      "-a" -> AST (* Print the AST*) 
    | "-l" -> LLVM_IR (* Generate LLVM, don't check*) 
    | "-c" -> Compile (*Generate and check LLVM IR *)
    | "-h" -> Help (*The help function*) 

    

    
