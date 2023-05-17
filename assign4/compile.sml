structure rationalPL0 :
sig val compile : string -> DataTypes.ProgramAST
end =
struct
exception RatError;
fun compile (fileName) =
let 
val inStream =  TextIO.openIn fileName;
val grab : int -> string = fn
    n => if TextIO.endOfStream inStream
         then ""
         else TextIO.inputN (inStream,n);
val printError : string * int * int -> unit = fn
    (msg,line,col) =>
     print (fileName^"["^Int.toString line^":"
           ^Int.toString col^"] "^msg^"\n");
val (tree,rem) = rationalPL0Parser.parse
             (15,
             (rationalPL0Parser.makeLexer grab),
             printError,
             ())
    handle rationalPL0Parser.ParseError => raise RatError;
(* Close the source program file *)
val _ = TextIO.closeIn inStream;
in tree
end end;