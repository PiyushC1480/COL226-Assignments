fun writeToFile (str : string, filename : string) =
let
  val outstream = TextIO.openAppend filename
in
  TextIO.output(outstream, str);
  TextIO.flushOut outstream;
  TextIO.closeOut outstream
end;

exception UnDeclaredVariableException ; 
exception OperationNotFound;
exception ProcedureNotDeclared;
val string_list = [#"1",#"2",#"3",#"4",#"5",#"6",#"7",#"8",#"9",#"0",#"."]
fun DFA(a:string):bool =
let 
    fun correct(x:string,i:int,bracopen:bool, bracclose:bool):bool = 
        let 
            val i_th = String.sub(x,i)
            fun search(lst: char list, str: char):bool =
                let
                    fun search_helper(lst: char list, str: char):bool =
                        case lst of
                            [] => false
                        | x::xs => if x = str then true else search_helper(xs, str)
                in
                    search_helper(lst, str)
                    end
        in 
        if i = String.size(x)-1 andalso i_th = #")" andalso bracopen then true
        else if i = String.size(x)-1 then false
        else if i_th = #"(" andalso i = String.size(x) then false
        else if  i_th = #"(" andalso String.sub(x,i+1) = #")" then false
        else if i_th = #"(" then correct(x,i+1,true,bracclose)
        else if i_th = #")" then false
        else if search(string_list,i_th) then correct(x,i+1,bracopen,bracclose)
        else false
        end
in
    if String.sub(a,0) = #"+" orelse  String.sub(a,0) = #"-" then correct(String.substring(a,1,String.size(a)-1),0,false,false)
    else false
end
fun get_input(msg : string)=
    (print (msg);
    let
        val in_str = valOf(TextIO.inputLine TextIO.stdIn)
    in
        String.substring (in_str, 0, (String.size in_str) -1)
    end)

fun get_var_input(variable) =
    let 
        val x = get_input("Enter value for variable  "^variable ^":")
    in
        x
    end
(*stack operations*)
fun create_stack(commands, stack) = 
    if null commands then stack
    else create_stack (tl commands, FunctionStack.push((hd commands), stack))

fun push_lst(l,s) = 
    if (null l) then 
        s
    else 
        push_lst(tl l, FunctionStack.push(hd l,s))

fun get_var(declarations) = 
    let 
        val DataTypes.VarDecls(variables,types) = declarations
    in
        (variables,types)
    end

fun get_proc(procdef) = 
    let 
        val DataTypes.ProcDef(n,block) = procdef
    in
        (n,block)
    end

fun init_addr(variables , memorytable,index)=   
    if (null variables) then ()
    else (HashTable.insert memorytable ( hd variables, index); init_addr(tl variables, memorytable,index+1))

fun init_scope(variables, scopetable, scope) = 
    if (null variables) then ()
    else (HashTable.insert scopetable(hd variables,scope) ; init_scope((tl variables), scopetable, scope))

fun init_table(declarations, memorytable,scopetable,scope,index) =  
    if (null declarations) then index
    else 
        let 
            val temp = get_var(hd declarations)
            val num_var = length (#1 temp)
        in 
            init_addr(#1 temp , memorytable,index);
            init_scope(#1 temp, scopetable,scope);
            init_table(tl declarations, memorytable, scopetable,scope,index + num_var)
        end 


(*put modified values of a variable in the hashmap*)

(*memory operatoins*)
fun init_array(size:int) = Array.array(size,"")

fun getVariableIdx( variableName, memorytable  ) = 
    HashTable.lookup memorytable variableName 

fun getVariableValue( variableName, memorytable, memory ) = 
    let 
        val idxOfVariable = getVariableIdx( variableName, memorytable)
    in 
        Array.sub( memory, idxOfVariable )
    end 

fun setVariableValue( variableName, memorytable, memory, value) = 
    let 
        val idxOfVariable = getVariableIdx( variableName, memorytable ) 
    in 
        Array.update( memory, idxOfVariable, value)
    end 

(* got ast from the parser now converting it into a stack *)
fun cmdproc (commands: DataTypes.Command list) = 
    if null commands  then 
        []
    else 
        let
          val command = hd commands 
          val ans  = cmdproc(tl commands)
        in
          case command of DataTypes.AssignmentCmd(x,y) => StackDataTypes.ASSIGNMENTCMD(x,y) :: ans
                            | DataTypes.ReadCmd(x) => StackDataTypes.READCMD(x) ::ans
                            | DataTypes.PrintCmd(x) => StackDataTypes.PRINTCMD(x) :: ans
                            | DataTypes.CallCmd(x) => StackDataTypes.CALLCMD(x) :: ans
                            | DataTypes.ConditionalCmd(x,y,z) => StackDataTypes.ITECMD(x,y,z) :: ans
                            | DataTypes.WhileCmd(x,y) => StackDataTypes.WHILECMD(x,y) :: ans
                            
        end

(*just for printing the stack which is produced*)
fun exprproc(expr) =
    case expr of DataTypes.RatAdd(x,y) => List.concat[  exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR(".+.") ]]
                |  DataTypes.RatSub(x,y) => List.concat [ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR(".-.") ]]
                |  DataTypes.RatMul(x,y) => List.concat [ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR(".*.") ]]
                |  DataTypes.RatDiv(x,y) => List.concat [ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("./.") ]]
                

                | DataTypes.BigInt(x) =>  [StackDataTypes.INTNUMERAL(x)]
                | DataTypes.RationalNum(x) =>  [StackDataTypes.RATNUMERAL(x)]

                | DataTypes.RatMake(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("make_rat") ]]
                | DataTypes.RatRat(x) => List.concat[ exprproc(x) , [StackDataTypes.OPERATOR("rat") ]]
                | DataTypes.RatShow(x) => List.concat[ exprproc(x) ,  [StackDataTypes.OPERATOR("show_rational") ]]
                | DataTypes.RatShowDec(x) => List.concat[ exprproc(x) , [StackDataTypes.OPERATOR("show_decimal") ]]
                | DataTypes.RatFromDec(x) => List.concat[ exprproc(x) , [StackDataTypes.OPERATOR("from_decimal") ]]
                | DataTypes.RatToDec(x) => List.concat[ exprproc(x) , [StackDataTypes.OPERATOR("to_decimal") ]]
                | DataTypes.RatInv(x) => List.concat[ exprproc(x) , [StackDataTypes.OPERATOR("inverse") ]]

                |  DataTypes.IntAdd(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("+") ]]
                |  DataTypes.IntSub(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("-") ]]
                |  DataTypes.IntMul(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("*") ]]
                |  DataTypes.IntDiv(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("/") ]]
                |  DataTypes.IntMod(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("%") ]]
                |  DataTypes.Neg(x) => List.concat[ exprproc(x) , [StackDataTypes.OPERATOR(".~.") ]]
                |  DataTypes.IntNeg(x) => List.concat[ exprproc(x) , [StackDataTypes.OPERATOR("~") ]]

                | DataTypes.BoolTrue => [StackDataTypes.BOOLEAN("tt")]
                | DataTypes.BoolFalse => [StackDataTypes.BOOLEAN("ff")]
                | DataTypes.BoolNot(x) => List.concat[ exprproc(x) , [StackDataTypes.OPERATOR("!") ]]
                |  DataTypes.BoolAnd(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("andalso") ]]
                |  DataTypes.BoolOr(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("orelse") ]]
                |  DataTypes.BoolEqual(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("=") ]]
                |  DataTypes.BoolNotEqual(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("<>") ]]
                |  DataTypes.BoolLessThan(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("<") ]]
                |  DataTypes.BoolLessThanOrEqual(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR("<=") ]]
                |  DataTypes.BoolGreaterThan(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR(">") ]]
                |  DataTypes.BoolGreaterThanOrEqual(x,y) => List.concat[ exprproc(x) , exprproc(y), [StackDataTypes.OPERATOR(">=") ]]
                
                |  DataTypes.Identifier(x) =>  [StackDataTypes.VARIABLE(x)]

fun bool_str(str) = if str="tt" then "tt" else "ff"
(*evaluator function*)
(*takes the variable name and then processes it *)
(* x is string and y is string value of the operation which is to be performed*)
fun evaluator(x,y,opr:string)= 
    if (opr = ".+.") then
        let 
            val a = Rational.fromDecimal(x)
            val b = Rational.fromDecimal(y)
        in 
            Rational.toDecimal(Rational.add(a,b))
        end
    else if (opr = ".-.") then
        let 
            val a = Rational.fromDecimal(x)
            val b = Rational.fromDecimal(y)
        in 
            Rational.toDecimal(Rational.subtract(a,b))
        end
    else if (opr = ".*.") then
        let 
            val a = Rational.fromDecimal(x)
            val b = Rational.fromDecimal(y)
        in 
            Rational.toDecimal(Rational.multiply(a,b))
        end
    else if (opr = "./.") then
        let 
            val a = Rational.fromDecimal(x)
            val b = Rational.fromDecimal(y)
        in 
            Rational.toDecimal(valOf(Rational.divide(a,b)))
        end
    else if (opr = "+") then
        let 
            val a = BigInt.fromString(x)
            val b = BigInt.fromString(y)
        in 
            BigInt.toString(BigInt.add(a,b))
        end
    else if (opr = "-") then
        let 
            val a = BigInt.fromString(x)
            val b = BigInt.fromString(y)
        in 
            BigInt.toString(BigInt.subtract(a,b))
        end
    else if (opr = "*") then
        let 
            val a = BigInt.fromString(x)
            val b = BigInt.fromString(y)
        in 
            BigInt.toString(BigInt.multiply(a,b))
        end
    else if (opr = "/") then
        let 
            val a = BigInt.fromString(x)
            val b = BigInt.fromString(y)
        in 
            BigInt.toString(BigInt.quotient(a,b))
        end
    else if (opr = "%") then
        let 
            val a = BigInt.fromString(x)
            val b = BigInt.fromString(y)
        in 
            BigInt.toString(BigInt.remainder(a,b))
        end
    else if (opr = "!") then
        let 
            val n = bool_str(x)
        in 
            if n="tt" then "ff" else "tt"
        end
    else if (opr = "~") then
        let 
            val a = BigInt.fromString(x)
        in 
            BigInt.toString(BigInt.negate(a))
        end
    else if (opr = ".~.") then
        let 
            val a = Rational.fromDecimal(x)
        in 
            Rational.toDecimal(Rational.neg(a))
        end
    else if (opr = "andalso") then
        if x = "tt" andalso y = "tt" then "tt" else "ff"
    else if (opr = "orelse") then
        if x = "ff" andalso y = "ff" then "ff" else "tt"
    (*check for x and y whther theu are string or not.*)
    else if (opr = "=") then
        if (x = "tt" andalso y="ff") orelse (x = "ff" andalso y="tt")  then "ff"
        else if (x = "tt" andalso y="tt") orelse (x = "ff" andalso y="ff") then "tt"
        else if (DFA(x) andalso DFA(y)) then
            let 
                val a = Rational.fromDecimal(x)
                val b = Rational.fromDecimal(y)
                val ans = Rational.equal(a,b)
            in 
                if ans then "tt" else "ff"
            end
        else 
            let 
                val a = BigInt.fromString(x)
                val b = BigInt.fromString(y)
                val ans = BigInt.equal(a,b)
            in 
                if ans then "tt" else "ff"
            end

    else if (opr = "<>") then
        if (x = "tt" andalso y="ff") orelse (x = "ff" andalso y="tt")  then "tt"
        else if (x = "tt" andalso y="tt") orelse (x = "ff" andalso y="ff") then "ff"
        else if (DFA(x) andalso DFA(y)) then
            let 
                val a = Rational.fromDecimal(x)
                val b = Rational.fromDecimal(y)
                val ans = Rational.equal(a,b)
            in 
                if ans then "ff" else "tt"
            end
        else 
            let 
                val a = BigInt.fromString(x)
                val b = BigInt.fromString(y)
                val ans = BigInt.equal(a,b)
            in 
                if ans then "ff" else "tt"
            end

    else if (opr = "<") then
        if (DFA(x) andalso DFA(y)) then
            let 
                val a = Rational.fromDecimal(x)
                val b = Rational.fromDecimal(y)
                val ans = Rational.less(a,b)
            in 
                if ans then "tt" else "ff"
            end
        else 
            let 
                val a = BigInt.fromString(x)
                val b = BigInt.fromString(y)
                val ans = BigInt.less(a,b)
            in 
                if ans then "tt" else "ff"
            end
    else if (opr = "<=") then
        if (DFA(x) andalso DFA(y)) then
            let 
                val a = Rational.fromDecimal(x)
                val b = Rational.fromDecimal(y)
                val ans1 = Rational.equal(a,b)
                val ans2  =Rational.less(a,b)
            in 
                if (ans1 orelse ans2) then "tt" else "ff"
            end
        else 
            let 
                val a = BigInt.fromString(x)
                val b = BigInt.fromString(y)
                val ans = BigInt.lessEq(a,b)
            in 
                if ans then "tt" else "ff"
            end
    else if (opr = ">") then
        if (DFA(x) andalso DFA(y)) then
            let 
                val a = Rational.fromDecimal(x)
                val b = Rational.fromDecimal(y)
                val ans1 = Rational.equal(a,b)
                val ans2 = Rational.less(a,b)
            in 
                if ans1 orelse ans2 then "ff" else "tt"
            end
        else 
            let 
                val a = BigInt.fromString(x)
                val b = BigInt.fromString(y)
                val ans1 = BigInt.less(a,b)
                val ans2 = BigInt.equal(a,b)
            in 
                if ans1 orelse ans2 then "ff" else "tt"
            end
    else if (opr = ">=") then
        if (DFA(x) andalso DFA(y)) then
            let 
                val a = Rational.fromDecimal(x)
                val b = Rational.fromDecimal(y)
                val ans = Rational.less(a,b)
            in 
                if ans then "ff" else "tt"
            end
        else 
            let 
                val a = BigInt.fromString(x)
                val b = BigInt.fromString(y)
                val ans = BigInt.greaterEq(a,b)
            in 
                if ans then "tt" else "ff"
            end
    else if (opr = "make_rat") then 
        let 
            val a = BigInt.fromString(x)
            val b = BigInt.fromString(y)
            val n = valOf(Rational.make_rat(a,b))
        in 
            Rational.toDecimal(n)
        end
    else if (opr = "rat") then 
        let 
            val a = BigInt.fromString(x)
            val n = Rational.rat(a)
        in 
            Rational.toDecimal(n)
        end
    else if (opr = "inverse") then 
        Rational.toDecimal(valOf(Rational.inverse(Rational.fromDecimal(x))))
    else if (opr = "show_rational") then 
        let 
            val a = Rational.fromDecimal(x)
            val n = Rational.showRat(a)
        in 
            n
        end
    else if (opr = "show_decimal") then 
        let 
            val a = Rational.fromDecimal(x)
            val n = Rational.showDecimal(a)
        in 
            n
        end
    else if (opr = "from_decimal") then 
        x
    else if (opr = "to_decimal") then 
        let 
            val a = Rational.fromDecimal(x)
        in 
            Rational.toDecimal(a)
        end
    else raise OperationNotFound


fun push_cmd( commands, stack ) = 
    if( null commands ) then 
        stack 
    else
        push_cmd( tl commands, FunctionStack.push( hd commands, stack))

fun push_expr(expression,stack) = 
    let 
        val nodes = List.rev(exprproc(expression))
    in 
        push_lst(nodes,stack)
    end


(*print the putput or see the output of the parser at each step them impelememorytable all the to string operations*)
(*----------------AST to stack for processing ----------------------*)                                                         
fun value_stack_to_str(stack) = (FunctionStack.toString Int.toString stack)

fun expr_to_str(element) = case element of StackDataTypes.VARIABLE(x) => x
                            | StackDataTypes.RATNUMERAL(x) => Rational.toDecimal(x)
                            | StackDataTypes.INTNUMERAL(x) => BigInt.toString(x)
                            | StackDataTypes.BOOLEAN(x) => x
                            | StackDataTypes.OPERATOR(x) =>x
                            | StackDataTypes.READCMD(x) => ""
                            | StackDataTypes.PRINTCMD(x) => ""
                            | StackDataTypes.PRINTCMD_ => ""
                            | StackDataTypes.CALLCMD(x) => ""
                            | StackDataTypes.CALLCMD_ => ""
                            | StackDataTypes.ASSIGNMENTCMD(x) =>""
                            | StackDataTypes.ASSIGNMENTCMD_(x) => ""
                            | StackDataTypes.ITECMD(x,y,z) => ""
                            | StackDataTypes.ITECMD_(x,y) => ""
                            | StackDataTypes.WHILECMD(x,y) =>""
                            | StackDataTypes.WHILECMD_(x,y) => ""



fun exprlis_to_str(list) = 
    if null list then ""
    else
        let 
            val temp = if ( null (tl list )) then "" else "."
        in
            ( expr_to_str( hd list))^temp^(exprlis_to_str( tl list))
        end


fun expr_to_str(expr) = 
    let 
        val nodeList = exprproc( expr )
    in 
        exprlis_to_str( nodeList )
    end 



fun cmd_to_str(cmd) = 
     case cmd of DataTypes.AssignmentCmd(x , y ) =>  x^"."^expr_to_str( y)^ ".ASSIGN"
                | DataTypes.ReadCmd(x) => x ^"."^"READ"
                | DataTypes.PrintCmd(x) => expr_to_str(x) ^  ".PRINT"
                |  DataTypes.CallCmd(x) => x ^  ".PRINT"
                | DataTypes.ConditionalCmd(x,y,z) => expr_to_str(x)^"."^cmdseq_to_str(y )^"."^cmdseq_to_str(z)^".ITE"
                | DataTypes.WhileCmd(x,y) => expr_to_str(x) ^ "." ^ cmdseq_to_str( y  ) ^ ".WHILE"  

and cmdseq_to_str (cmds) = 
    if (null cmds) then ""
    else 
        let 
            val joinWith = if (null(tl cmds)) then "" else "."
        in  
            cmd_to_str(hd cmds) ^ joinWith ^ cmdseq_to_str(tl cmds)
        end


fun stk_nd_to_str(  node ) = case node of StackDataTypes.VARIABLE(x) => x
                                             | StackDataTypes.INTNUMERAL(x) => x 
                                             | StackDataTypes.RATNUMERAL(x) => Rational.toDecimal(x)
                                             | StackDataTypes.BOOLEAN(x) => BigInt.toString(x)
                                             | StackDataTypes.OPERATOR(x) => x 
                                             | StackDataTypes.READCMD(x) => x^"/READ"
                                             | StackDataTypes.PRINTCMD(x) => expr_to_str(x)^"/PRINT"
                                             | StackDataTypes.PRINTCMD_ => "PRINT"
                                             | StackDataTypes.CALLCMD(x) => x^"/CALL"
                                             | StackDataTypes.CALLCMD_ => "CALL"
                                             | StackDataTypes.ASSIGNMENTCMD(x,y) => x^"("^expr_to_str(y)^"/ASSIGN"
                                             | StackDataTypes.ITECMD(x,y,z) => expr_to_str(x)^"("^cmdseq_to_str(y )^")"^cmdseq_to_str(z)^"/ITE"
                                             | StackDataTypes.WHILECMD(x,y) => expr_to_str(x)^"("^cmdseq_to_str(y )^"/"^"WHILE"
                                             | StackDataTypes.ITECMD_(x,y) => "cmdseq" ^ "(" ^ "cmdseq" ^ "/" ^ "ITE"
                                             | StackDataTypes.ASSIGNMENTCMD_(x) => x^"/"^"ASSIGN" 
                                             | StackDataTypes.WHILECMD_(x,y) => "partial-while"    

fun control_stack_to_str( cs ) = ( FunctionStack.toString stk_nd_to_str cs ) 
fun printControlStack( cs) =print((FunctionStack.toString stk_nd_to_str cs)^"\n")                                    
fun search_procedure(n,procdecls) =
    if null procdecls then raise ProcedureNotDeclared
    else 
        let
            val temp = get_proc(hd procdecls)
        in
            if (n =  (#1 temp)) then (#2 temp)
            else search_procedure(n, tl procdecls)
        end
fun mem_to_str(memory,answer, index, num_var) =
    if (index = num_var) then 
        print(answer^"\n")
    else 
        let
            val temp = Array.sub(memory,index)
            val joinWith = if( answer = "") then "" else "-"
        in 
            mem_to_str(memory , answer ^ joinWith ^ temp , index + 1 , num_var)
        end

signature RATIONAL_EVAL = 
sig
    (*executor ffuncitons*)
    val rules : string FunctionStack.Stack * string array * (string, int) HashTable.hash_table * StackDataTypes.STACKNODE FunctionStack.Stack * int * (DataTypes.ProcDef list)  *string
                -> string FunctionStack.Stack * string array * (string, int) HashTable.hash_table * StackDataTypes.STACKNODE FunctionStack.Stack * int * (DataTypes.ProcDef list)  *string

    val execute :  string FunctionStack.Stack * string array * (string, int) HashTable.hash_table * StackDataTypes.STACKNODE FunctionStack.Stack *int * (DataTypes.ProcDef list) *string
                    -> string FunctionStack.Stack * string array * (string, int) HashTable.hash_table * StackDataTypes.STACKNODE FunctionStack.Stack*int * (DataTypes.ProcDef list) *string
    val create_memory :  DataTypes.VarDecls list -> string array * (string , int) HashTable.hash_table * (string, int) HashTable.hash_table * int


end

structure Rational_Eval :> RATIONAL_EVAL = 
struct
fun create_memory (declarations) = 
    let 
        val memory = init_array(1000)
        val memorytable : (string,int) HashTable.hash_table = HashTable.mkTable (HashString.hashString,op=)(42, Fail "not found")
        val scopetable : (string, int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found")
        val num_var = init_table(declarations,memorytable ,scopetable,0, 0)
    in
        (memory,memorytable,scopetable,num_var)
    end

fun rules(ValueStack, memory, memorytable,   ControlStack ,num_var, ProcedureStack ,outputfile) = 
    if FunctionStack.is_empty ControlStack then 
        (ValueStack , memory , memorytable , ControlStack,num_var, ProcedureStack, outputfile)
    else
        (*process*)
        let 
            val top = FunctionStack.top ControlStack

        in 
            case top of StackDataTypes. INTNUMERAL(x) => (FunctionStack.push(BigInt.toString(x),ValueStack), memory , memorytable , FunctionStack.pop ControlStack, num_var, ProcedureStack, outputfile)
                        | StackDataTypes.RATNUMERAL(x) => (FunctionStack.push(Rational.toDecimal(x),ValueStack),memory,memorytable,FunctionStack.pop ControlStack, num_var, ProcedureStack, outputfile)
                        | StackDataTypes.BOOLEAN(x) => (FunctionStack.push(bool_str(x),ValueStack), memory,memorytable,FunctionStack.pop ControlStack, num_var,ProcedureStack,outputfile)
                        | StackDataTypes.VARIABLE(x) => (FunctionStack.push(getVariableValue(x,memorytable,memory) ,ValueStack),memory,memorytable,FunctionStack.pop ControlStack, num_var,ProcedureStack,outputfile)
                        | StackDataTypes.ASSIGNMENTCMD(x,y) => let 
                                                                    val a = FunctionStack.pop ControlStack
                                                                    val b = FunctionStack.push (StackDataTypes.ASSIGNMENTCMD_(x),a)
                                                                    val c = push_expr(y,b)
                                                                in  
                                                                    (ValueStack,memory,memorytable,c, num_var,ProcedureStack,outputfile)
                                                                end

                        | StackDataTypes.ASSIGNMENTCMD_(x) => let 
                                                                val SOME (a,b) = FunctionStack.poptop(ValueStack)
                                                                val temp = setVariableValue(x,memorytable,memory,a)
                                                            in 
                                                                (b, memory, memorytable, FunctionStack.pop(ControlStack), num_var,ProcedureStack,outputfile)
                                                            end
                                                        
                        | StackDataTypes.PRINTCMD(x) => let 
                                                            val a = FunctionStack.pop ControlStack
                                                            val b = FunctionStack.push (StackDataTypes.PRINTCMD_,a)
                                                            val c = push_expr(x,b)
                                                        in 
                                                            (ValueStack,memory,memorytable,c, num_var,ProcedureStack,outputfile)
                                                        end
                        | StackDataTypes.PRINTCMD_ =>   let
                                                            val SOME(topValue, remainingValueStack) = FunctionStack.poptop(ValueStack)
                                                        in
                                                            (writeToFile(topValue^"\n",outputfile) ; (remainingValueStack,memory,memorytable,FunctionStack.pop(ControlStack), num_var,ProcedureStack,outputfile))
                                                        end
                        | StackDataTypes.CALLCMD(x) => let
                                                            val a = FunctionStack.pop ControlStack
                                                            val b = search_procedure(x,ProcedureStack)
                                                            val DataTypes.Block(declarationseq, commandlist) = b
                                                            val DataTypes.DeclarationSeq(vardecls, procdecls) = declarationseq
                                                            val n_prc_stack = if (procdecls =[]) then ProcedureStack else ProcedureStack @ procdecls;
                                                            val new_ctrl_stack = create_stack (List.rev(cmdproc commandlist ), FunctionStack.create_stack)
                                                            val new_ctrl_stack_list = FunctionStack.stack2list new_ctrl_stack
                                                            val scopetable : (string, int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found")
                                                        in 
                                                            let
                                                                val nn_cmd_stack = FunctionStack.push (StackDataTypes.CALLCMD_,a)
                                                                val cmd_stack = push_lst(List.rev new_ctrl_stack_list, nn_cmd_stack)
                                                                val num_var = init_table(vardecls,memorytable,scopetable,1, num_var)
                                                               
                                                            
                                                            in
                                                                (ValueStack,memory,memorytable,cmd_stack,num_var, n_prc_stack,outputfile)
                                                            end
                                                        end

                        | StackDataTypes.CALLCMD_ => let
                                                        val a = FunctionStack.pop ControlStack
                                                        val del_proc_stk =  ProcedureStack
                                                    in 
                                                        (ValueStack,memory,memorytable,a,num_var,del_proc_stk,outputfile)
                                                    end


                        | StackDataTypes.READCMD(x) => let 
                                                            val a = get_var_input(x)
                                                            val temp = setVariableValue(x,memorytable,memory,a)
                                                        in
                                                            (ValueStack,memory,memorytable,FunctionStack.pop ControlStack, num_var, ProcedureStack,outputfile)
                                                        end

                        | StackDataTypes.ITECMD(x,y,z) => let 
                                                            val a = FunctionStack.pop ControlStack
                                                            val b = FunctionStack.push(StackDataTypes.ITECMD_(y,z),a)
                                                            val c = push_expr(x,b)
                                                        in
                                                            (ValueStack,memory,memorytable,c, num_var, ProcedureStack,outputfile)
                                                        end
                        | StackDataTypes.ITECMD_(y,z) => let 
                                                            val SOME(condition, remainingValueStack)  = FunctionStack.poptop(ValueStack)
                                                            val remainingControlStack = FunctionStack.pop ControlStack
                                                        in  
                                                            if (condition= "tt") then
                                                                ((remainingValueStack,memory,memorytable,push_cmd (List.rev (cmdproc y) , remainingControlStack ), num_var,ProcedureStack,outputfile))
                                                            else
                                                                ((remainingValueStack,memory,memorytable,push_cmd (List.rev (cmdproc z) , remainingControlStack ), num_var, ProcedureStack,outputfile))
                                                        end 

                        | StackDataTypes.WHILECMD(x,y) => let 
                                                            val a  = FunctionStack.pop ControlStack
                                                            val b =  FunctionStack.push (StackDataTypes.WHILECMD_(x,y),a)
                                                            val c = push_expr(x,b)
                                                        in
                                                            (ValueStack,memory,memorytable,c,num_var,ProcedureStack,outputfile)
                                                        end    

                        | StackDataTypes.WHILECMD_(x,y) => let 
                                                                val SOME(condition,remainingValueStack) = FunctionStack.poptop(ValueStack)
                                                                val remainingControlStack = FunctionStack.pop ControlStack
                                                                val temp = FunctionStack.push (StackDataTypes.WHILECMD(x,y),remainingControlStack)
                                                                val updatedControlStack = push_cmd(List.rev(cmdproc y),temp)
                                                            in
                                                                if (condition = "tt") then
                                                                    (remainingValueStack,memory,memorytable,updatedControlStack, num_var,ProcedureStack,outputfile)
                                                                else 
                                                                    (remainingValueStack,memory,memorytable,remainingControlStack, num_var,ProcedureStack,outputfile)
                                                            end
                        | StackDataTypes.OPERATOR(x) => if (x = "!" orelse x = ".~." orelse x = "~" orelse x = "rat" orelse x = "show_rat" orelse x ="from_decimal" orelse x = "to_deciaml" orelse x = "show_decimal" orelse x="inverse" ) then
                                                            let 
                                                                val SOME (a,b) = FunctionStack.poptop(ValueStack)
                                                                val answer  = evaluator(a,"0",x)(*put the evaluator function here.*)
                                                            in  
                                                                (FunctionStack.push(answer,b),memory,memorytable,FunctionStack.pop ControlStack,num_var,ProcedureStack,outputfile)
                                                            end
                                                        else
                                                            let 
                                                                val SOME (a,b) = FunctionStack.poptop(ValueStack)
                                                                val SOME (c,d) = FunctionStack.poptop(b)
                                                                val answer  = evaluator(c,a,x) (*put the evaluator functiopn here*)
                                                            in 
                                                                (FunctionStack.push(answer,d),memory,memorytable,FunctionStack.pop ControlStack, num_var,ProcedureStack,outputfile)
                                                            end 
        end
fun execute(ValueStack, memory, memorytable , ControlStack , num_var,ProcedureStack,outputfile) = 
    if (FunctionStack.is_empty ControlStack) then (ValueStack, memory,memorytable,ControlStack,num_var,ProcedureStack,outputfile)
    else 
        let 
            val temp  = rules (ValueStack, memory, memorytable , ControlStack,num_var,ProcedureStack,outputfile)
        in
            execute ( #1 temp , #2 temp , #3 temp , #4 temp, #5 temp, #6 temp, #7 temp)
        end

end
