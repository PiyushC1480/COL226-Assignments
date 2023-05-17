structure Tokens = Tokens

type pos = int (*type of the position in the tokens*)
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0 
val eof = fn () => Tokens.EOF (!pos,!pos);


(*keywords declaratoin here . *)
(* dont think i need a keyword in here . if not running then see. *)
%%

%header (functor rationalPL0LexFun(structure Tokens: rationalPL0_TOKENS));
digit=[0-9];
alpha=[A-Za-z];
ws = [\ \t];
comment = [ -~];
%%
\n       => ( pos:= (!pos) + 1; lex()); 
{ws}+    => (lex());
"(*"({comment}|\n)*"*)" => (pos := !pos + size yytext ; continue());
".+."    => (Tokens.RATPLUS(!pos,!pos));
".*."      => (Tokens.RATTIMES(!pos,!pos));
".-."      => (Tokens.RATSUB(!pos,!pos));
"./."      => (Tokens.RATDIV(!pos,!pos));

"+"      => (Tokens.INTPLUS(!pos,!pos));
"*"      => (Tokens.INTTIMES(!pos,!pos));
"-"      => (Tokens.INTSUB(!pos,!pos));
"/"      => (Tokens.INTDIV(!pos,!pos));
"%"      => (Tokens.INTMOD(!pos,!pos));

"="      => (Tokens.EQUAL(!pos,!pos));
"<>"     => (Tokens.NOTEQUAL(!pos,!pos));
"<"       => (Tokens.LESS(!pos,!pos));
">"       => (Tokens.GREATER(!pos,!pos));
"<="      => (Tokens.LESSEQ(!pos,!pos));
">="      => (Tokens.GREATEREQ(!pos,!pos));

":="      => (Tokens.ASSIGN(!pos,!pos));

"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
"}"      => (Tokens.RBRACE(!pos,!pos));
"{"       => (Tokens.LBRACE(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));


"!"      => (Tokens.NOT(!pos,!pos)) ;
"&&"     => ( Tokens.ANDALSO(!pos,!pos)) ; 
"||"     => (Tokens.ORELSE(!pos,!pos)) ;
".~."      => (Tokens.NEGATE(!pos,!pos)) ;
"~"    =>(Tokens.INTNEGATE(!pos,!pos)) ;

"andalso" => (Tokens.ANDALSO(!pos,!pos)) ; 
"orelse" => (Tokens.ORELSE(!pos,!pos));
"procedure"  =>  (Tokens.PROCEDURE(!pos,!pos)) ; 
"var"       => (Tokens.VAR(!pos,!pos)) ; 
"rational"  => (Tokens.RATIONAL(!pos,!pos)) ;
"integer"  => (Tokens.INTEGER(!pos,!pos)) ;
"boolean"  => (Tokens.BOOL(!pos,!pos)) ;
"if"       => (Tokens.IF(!pos,!pos)) ;
"then"     => (Tokens.THEN(!pos,!pos)) ;
"else" => (Tokens.ELSE(!pos,!pos)) ; 
"fi"       =>(Tokens.FI(!pos,!pos)) ;
"while"  => (Tokens.WHILE(!pos,!pos)) ;
"do"     => (Tokens.DO(!pos,!pos));
"od"     => (Tokens.OD(!pos,!pos));
"print"   => (Tokens.PRINT(!pos,!pos));
"read"    =>(Tokens.READ(!pos,!pos));
"call"     =>(Tokens.CALL(!pos,!pos));
"inverse"     =>(Tokens.INVERSE(!pos,!pos));
"rat"       => (Tokens.RAT(!pos,!pos));
"make_rat" => (Tokens.MAKERAT(!pos,!pos));
"showRat"   => (Tokens.SHOWRAT(!pos,!pos));
"showDecimal" => (Tokens.SHOWDEC(!pos,!pos));
"fromDecimal" => (Tokens.FROMDEC(!pos,!pos));
"toDecimal" => (Tokens.TODEC(!pos,!pos));
","      => (Tokens.COMMA(!pos,!pos));
"tt"    => (Tokens.TRUE(!pos,!pos));
"ff"     => (Tokens.FALSE(!pos,!pos));

{digit}+ => (Tokens.NUM(BigInt.fromString(yytext), !pos ,!pos));
["+""-"]?{digit}*"."{digit}*"("{digit}+")" => (Tokens.DECIMAL(Rational.fromDecimal(yytext),!pos,!pos));
{alpha}({alpha} | {digit})* => ( Tokens.IDENTIFIER( yytext ,!pos , !pos )) ; 


