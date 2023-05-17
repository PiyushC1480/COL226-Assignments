structure DataTypes =
struct
    datatype ProgramAST = Program of Block

    and Block = Block of DeclarationSeq * Command list

    and DeclarationSeq = DeclarationSeq of VarDecls list * ProcDef list

    and VarDecls = VarDecls of (string list * Type)

    and Type = Rational | Bigint | Bool 
    
    and ProcDecls = ProcDecls of ProcDef list 

    and ProcDef = ProcDef of string * Block

    and Command = AssignmentCmd of string * Expression
                 | CallCmd of string
                 | ReadCmd of string
                 | PrintCmd of Expression
                 | ConditionalCmd of Expression * Command list * Command list
                 | WhileCmd of Expression * Command list
    and Expression =  RatAdd of Expression * Expression
                      | RatSub of Expression * Expression
                      | RatMul of Expression * Expression
                      | RatDiv of Expression * Expression
                      | RatInv of Expression
                      | Neg of Expression
                      | IntNeg of Expression
                      | RatMake of Expression * Expression
                      | RatRat of Expression
                      | RatShow of Expression
                      | RatShowDec of Expression
                      | RatFromDec of Expression
                      | RatToDec of Expression
                      | IntAdd of Expression * Expression
                      | IntSub of Expression * Expression
                      | IntMul of Expression * Expression
                      | IntDiv of Expression * Expression
                      | IntMod of Expression * Expression
                      | BoolTrue
                        | BoolFalse
                        | BoolNot of Expression
                        | BoolAnd of Expression * Expression
                        | BoolOr of Expression * Expression
                        | BoolEqual of Expression * Expression
                        | BoolNotEqual of Expression * Expression
                        | BoolLessThan of Expression * Expression
                        | BoolLessThanOrEqual of Expression * Expression
                        | BoolGreaterThan of Expression * Expression
                        | BoolGreaterThanOrEqual of Expression * Expression
                        | Identifier of string
                        | BigInt of BigInt.bigint
                        | RationalNum of Rational.rational
end;
