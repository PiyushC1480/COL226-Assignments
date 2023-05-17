structure StackDataTypes = 
struct
datatype STACKNODE =  VARIABLE of string 
                    | RATNUMERAL of Rational.rational
                    | INTNUMERAL of BigInt.bigint 
                    | BOOLEAN of string
                    | OPERATOR of string  
                    | READCMD of string 
                    | PRINTCMD of DataTypes.Expression
                    | CALLCMD of string
                    | PRINTCMD_  (* partially completed write command with value to be written on top of value stack*)
                    | CALLCMD_
                    | ASSIGNMENTCMD of string * DataTypes.Expression 
                    | ITECMD of DataTypes.Expression * ( DataTypes.Command list ) * ( DataTypes.Command list )
                    | WHILECMD of DataTypes.Expression * ( DataTypes.Command list ) 
                    | ASSIGNMENTCMD_ of string (* partial set command string => variable name *)
                    | ITECMD_ of ( DataTypes.Command list ) * ( DataTypes.Command list ) (* partially completed if then else command with value of boolean on top of value stack *)
                    | WHILECMD_ of ( DataTypes.Expression )  * ( DataTypes.Command list ) (* partially completed while command with value of expression on top of value stack   *)
                    | PROCEDUREDEC of string * DataTypes.Block
end ; 
