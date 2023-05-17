

signature INTERPRETER =
sig
    val interpret : string * string -> string FunctionStack.Stack * string array * (string, int) HashTable.hash_table * StackDataTypes.STACKNODE FunctionStack.Stack * int * (DataTypes.ProcDef list) *string
end

structure Interpreter :> INTERPRETER =
struct
fun interpret(inputfile,outputfile) = 
    let 
        val ProgramAST = rationalPL0.compile inputfile 
        val DataTypes.Program(block) = ProgramAST
        val DataTypes.Block(declarationseq, commandlist) = block
        val DataTypes.DeclarationSeq(vardecls, procdecls) = declarationseq
        val (memory,memorytable,scopetable,num_var) = Rational_Eval.create_memory(vardecls)
        val ControlStack = create_stack (List.rev(cmdproc commandlist ), FunctionStack.create_stack)
        val ValueStack = FunctionStack.create_stack
        val ProcedureStack  = [] @ procdecls
    in 
        (TextIO.openOut outputfile;Rational_Eval.execute(ValueStack, memory,memorytable,ControlStack,num_var, ProcedureStack,outputfile))
    end
end