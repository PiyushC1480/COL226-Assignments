functor rationalPL0LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : rationalPL0_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes;
(* declare a hashmap to store the declared variables *)
val typeTable : (string, Type) HashTable.hash_table  = 
	HashTable.mkTable (HashString.hashString, op=)(42, Fail "identifier not found");

exception VariableRedeclarationException of string ; 
exception TypeMisMatchException ; 
exception UnDeclaredVariableException ; 

fun insertInTable( idList : string list , idType : Type) = 
        if ( null idList ) then () else ( if isSome (HashTable.find typeTable (hd idList)) then () else 
                HashTable.insert typeTable ( hd idList , idType ) ; insertInTable( tl idList , idType) 
                ) ; 

fun checkIntOfID( id: string ) = 
        if( isSome (HashTable.find typeTable  id)) 
        then
            case ( HashTable.lookup typeTable id ) of Bigint => () | Rational => ( raise TypeMisMatchException)  |  Bool => ( raise TypeMisMatchException) 
                                                      
        else
                raise UnDeclaredVariableException ; 
fun checkBoolOfID( id: string ) = 
        if( isSome (HashTable.find typeTable  id)) 
        then
            case ( HashTable.lookup typeTable id ) of Bigint => ( raise TypeMisMatchException) | Rational => ( raise TypeMisMatchException)  | Bool => () 
                                                      
        else
                raise UnDeclaredVariableException ; 
fun checkRatOfID( id: string ) = 
        if( isSome (HashTable.find typeTable  id)) 
        then
            case ( HashTable.lookup typeTable id ) of Bigint => ( raise TypeMisMatchException) | Rational => ()  | Bool => ( raise TypeMisMatchException) 
                                                      
        else
                raise UnDeclaredVariableException ;
				
fun getType ( id : string ) = 
         if( isSome (HashTable.find typeTable  id)) 
         then 
                HashTable.lookup typeTable id 
         else 
                raise UnDeclaredVariableException ; 


fun checkBool ( typeToBeChecked : Type ) = 
        if ( typeToBeChecked = Bool ) then true else false ; 
fun checkInt ( typeToBeChecked : Type ) = 
        if ( typeToBeChecked = Bigint ) then true else false ; 
fun checkRat ( typeToBeChecked : Type ) = 
        if ( typeToBeChecked = Rational ) then true else false ; 
fun checkSameType ( A : Type , B : Type ) = (A = B ) ; 



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\057\000\004\000\056\000\009\000\055\000\021\000\054\000\
\\025\000\053\000\028\000\052\000\029\000\051\000\045\000\050\000\
\\046\000\049\000\047\000\048\000\048\000\047\000\049\000\046\000\
\\051\000\045\000\053\000\044\000\054\000\043\000\055\000\042\000\
\\056\000\041\000\000\000\
\\001\000\002\000\165\000\005\000\165\000\006\000\080\000\007\000\165\000\
\\008\000\078\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\072\000\015\000\071\000\
\\016\000\070\000\017\000\069\000\018\000\068\000\019\000\067\000\
\\022\000\165\000\026\000\066\000\027\000\065\000\036\000\165\000\
\\040\000\165\000\050\000\165\000\000\000\
\\001\000\002\000\166\000\005\000\166\000\006\000\080\000\007\000\166\000\
\\008\000\078\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\072\000\015\000\071\000\
\\016\000\070\000\017\000\069\000\018\000\068\000\019\000\067\000\
\\022\000\166\000\026\000\066\000\027\000\065\000\036\000\166\000\
\\040\000\166\000\050\000\166\000\000\000\
\\001\000\002\000\167\000\005\000\167\000\006\000\167\000\007\000\167\000\
\\008\000\167\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\072\000\015\000\071\000\
\\016\000\070\000\017\000\069\000\018\000\068\000\019\000\067\000\
\\022\000\167\000\026\000\066\000\027\000\065\000\036\000\167\000\
\\040\000\167\000\050\000\167\000\000\000\
\\001\000\002\000\168\000\005\000\168\000\006\000\168\000\007\000\168\000\
\\008\000\168\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\072\000\015\000\071\000\
\\016\000\070\000\017\000\069\000\018\000\068\000\019\000\067\000\
\\022\000\168\000\026\000\066\000\027\000\065\000\036\000\168\000\
\\040\000\168\000\050\000\168\000\000\000\
\\001\000\002\000\169\000\005\000\169\000\006\000\169\000\007\000\169\000\
\\008\000\169\000\009\000\169\000\010\000\169\000\011\000\169\000\
\\012\000\169\000\013\000\169\000\014\000\169\000\015\000\169\000\
\\016\000\169\000\017\000\169\000\018\000\169\000\019\000\169\000\
\\022\000\169\000\026\000\169\000\027\000\169\000\036\000\169\000\
\\040\000\169\000\050\000\169\000\000\000\
\\001\000\002\000\170\000\005\000\170\000\006\000\170\000\007\000\170\000\
\\008\000\170\000\009\000\170\000\010\000\170\000\011\000\170\000\
\\012\000\170\000\013\000\170\000\014\000\170\000\015\000\170\000\
\\016\000\170\000\017\000\170\000\018\000\170\000\019\000\170\000\
\\022\000\170\000\026\000\170\000\027\000\170\000\036\000\170\000\
\\040\000\170\000\050\000\170\000\000\000\
\\001\000\002\000\171\000\005\000\081\000\006\000\080\000\007\000\079\000\
\\008\000\078\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\072\000\015\000\071\000\
\\016\000\070\000\017\000\069\000\018\000\068\000\019\000\067\000\
\\022\000\171\000\026\000\066\000\027\000\065\000\036\000\171\000\
\\040\000\171\000\050\000\171\000\000\000\
\\001\000\002\000\180\000\005\000\180\000\006\000\180\000\007\000\180\000\
\\008\000\180\000\009\000\180\000\010\000\076\000\011\000\180\000\
\\012\000\074\000\013\000\073\000\014\000\180\000\015\000\180\000\
\\016\000\180\000\017\000\180\000\018\000\180\000\019\000\180\000\
\\022\000\180\000\026\000\180\000\027\000\180\000\036\000\180\000\
\\040\000\180\000\050\000\180\000\000\000\
\\001\000\002\000\181\000\005\000\181\000\006\000\181\000\007\000\181\000\
\\008\000\181\000\009\000\181\000\010\000\076\000\011\000\181\000\
\\012\000\074\000\013\000\073\000\014\000\181\000\015\000\181\000\
\\016\000\181\000\017\000\181\000\018\000\181\000\019\000\181\000\
\\022\000\181\000\026\000\181\000\027\000\181\000\036\000\181\000\
\\040\000\181\000\050\000\181\000\000\000\
\\001\000\002\000\182\000\005\000\182\000\006\000\182\000\007\000\182\000\
\\008\000\182\000\009\000\182\000\010\000\182\000\011\000\182\000\
\\012\000\182\000\013\000\182\000\014\000\182\000\015\000\182\000\
\\016\000\182\000\017\000\182\000\018\000\182\000\019\000\182\000\
\\022\000\182\000\026\000\182\000\027\000\182\000\036\000\182\000\
\\040\000\182\000\050\000\182\000\000\000\
\\001\000\002\000\183\000\005\000\183\000\006\000\183\000\007\000\183\000\
\\008\000\183\000\009\000\183\000\010\000\183\000\011\000\183\000\
\\012\000\183\000\013\000\183\000\014\000\183\000\015\000\183\000\
\\016\000\183\000\017\000\183\000\018\000\183\000\019\000\183\000\
\\022\000\183\000\026\000\183\000\027\000\183\000\036\000\183\000\
\\040\000\183\000\050\000\183\000\000\000\
\\001\000\002\000\184\000\005\000\184\000\006\000\184\000\007\000\184\000\
\\008\000\184\000\009\000\184\000\010\000\184\000\011\000\184\000\
\\012\000\184\000\013\000\184\000\014\000\184\000\015\000\184\000\
\\016\000\184\000\017\000\184\000\018\000\184\000\019\000\184\000\
\\022\000\184\000\026\000\184\000\027\000\184\000\036\000\184\000\
\\040\000\184\000\050\000\184\000\000\000\
\\001\000\002\000\185\000\005\000\185\000\006\000\185\000\007\000\185\000\
\\008\000\185\000\009\000\185\000\010\000\076\000\011\000\185\000\
\\012\000\074\000\013\000\073\000\014\000\185\000\015\000\185\000\
\\016\000\185\000\017\000\185\000\018\000\185\000\019\000\185\000\
\\022\000\185\000\026\000\185\000\027\000\185\000\036\000\185\000\
\\040\000\185\000\050\000\185\000\000\000\
\\001\000\002\000\187\000\005\000\187\000\006\000\187\000\007\000\187\000\
\\008\000\187\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\072\000\015\000\071\000\
\\016\000\070\000\017\000\069\000\018\000\068\000\019\000\067\000\
\\022\000\187\000\026\000\187\000\027\000\187\000\036\000\187\000\
\\040\000\187\000\050\000\187\000\000\000\
\\001\000\002\000\188\000\005\000\188\000\006\000\188\000\007\000\188\000\
\\008\000\188\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\072\000\015\000\071\000\
\\016\000\070\000\017\000\069\000\018\000\068\000\019\000\067\000\
\\022\000\188\000\026\000\188\000\027\000\188\000\036\000\188\000\
\\040\000\188\000\050\000\188\000\000\000\
\\001\000\002\000\189\000\005\000\189\000\006\000\189\000\007\000\189\000\
\\008\000\189\000\009\000\189\000\010\000\189\000\011\000\189\000\
\\012\000\189\000\013\000\189\000\014\000\189\000\015\000\189\000\
\\016\000\189\000\017\000\189\000\018\000\189\000\019\000\189\000\
\\022\000\189\000\026\000\189\000\027\000\189\000\036\000\189\000\
\\040\000\189\000\050\000\189\000\000\000\
\\001\000\002\000\190\000\005\000\190\000\006\000\190\000\007\000\190\000\
\\008\000\190\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\190\000\015\000\190\000\
\\016\000\070\000\017\000\069\000\018\000\068\000\019\000\067\000\
\\022\000\190\000\026\000\190\000\027\000\190\000\036\000\190\000\
\\040\000\190\000\050\000\190\000\000\000\
\\001\000\002\000\191\000\005\000\191\000\006\000\191\000\007\000\191\000\
\\008\000\191\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\191\000\015\000\191\000\
\\016\000\070\000\017\000\069\000\018\000\068\000\019\000\067\000\
\\022\000\191\000\026\000\191\000\027\000\191\000\036\000\191\000\
\\040\000\191\000\050\000\191\000\000\000\
\\001\000\002\000\192\000\005\000\192\000\006\000\192\000\007\000\192\000\
\\008\000\192\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\192\000\015\000\192\000\
\\016\000\192\000\017\000\192\000\018\000\192\000\019\000\192\000\
\\022\000\192\000\026\000\192\000\027\000\192\000\036\000\192\000\
\\040\000\192\000\050\000\192\000\000\000\
\\001\000\002\000\193\000\005\000\193\000\006\000\193\000\007\000\193\000\
\\008\000\193\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\193\000\015\000\193\000\
\\016\000\193\000\017\000\193\000\018\000\193\000\019\000\193\000\
\\022\000\193\000\026\000\193\000\027\000\193\000\036\000\193\000\
\\040\000\193\000\050\000\193\000\000\000\
\\001\000\002\000\194\000\005\000\194\000\006\000\194\000\007\000\194\000\
\\008\000\194\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\194\000\015\000\194\000\
\\016\000\194\000\017\000\194\000\018\000\194\000\019\000\194\000\
\\022\000\194\000\026\000\194\000\027\000\194\000\036\000\194\000\
\\040\000\194\000\050\000\194\000\000\000\
\\001\000\002\000\195\000\005\000\195\000\006\000\195\000\007\000\195\000\
\\008\000\195\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\195\000\015\000\195\000\
\\016\000\195\000\017\000\195\000\018\000\195\000\019\000\195\000\
\\022\000\195\000\026\000\195\000\027\000\195\000\036\000\195\000\
\\040\000\195\000\050\000\195\000\000\000\
\\001\000\002\000\020\000\000\000\
\\001\000\002\000\030\000\000\000\
\\001\000\002\000\061\000\000\000\
\\001\000\002\000\095\000\005\000\081\000\006\000\080\000\007\000\079\000\
\\008\000\078\000\009\000\077\000\010\000\076\000\011\000\075\000\
\\012\000\074\000\013\000\073\000\014\000\072\000\015\000\071\000\
\\016\000\070\000\017\000\069\000\018\000\068\000\019\000\067\000\
\\026\000\066\000\027\000\065\000\000\000\
\\001\000\002\000\124\000\000\000\
\\001\000\002\000\125\000\000\000\
\\001\000\002\000\134\000\000\000\
\\001\000\002\000\139\000\000\000\
\\001\000\003\000\000\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\022\000\097\000\
\\026\000\066\000\027\000\065\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\022\000\122\000\
\\026\000\066\000\027\000\065\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\022\000\127\000\
\\026\000\066\000\027\000\065\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\022\000\128\000\
\\026\000\066\000\027\000\065\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\022\000\129\000\
\\026\000\066\000\027\000\065\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\022\000\130\000\
\\026\000\066\000\027\000\065\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\022\000\131\000\
\\026\000\066\000\027\000\065\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\022\000\137\000\
\\026\000\066\000\027\000\065\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\026\000\066\000\
\\027\000\065\000\036\000\094\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\026\000\066\000\
\\027\000\065\000\040\000\064\000\000\000\
\\001\000\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\
\\009\000\077\000\010\000\076\000\011\000\075\000\012\000\074\000\
\\013\000\073\000\014\000\072\000\015\000\071\000\016\000\070\000\
\\017\000\069\000\018\000\068\000\019\000\067\000\026\000\066\000\
\\027\000\065\000\050\000\132\000\000\000\
\\001\000\020\000\036\000\000\000\
\\001\000\021\000\038\000\000\000\
\\001\000\021\000\039\000\000\000\
\\001\000\021\000\083\000\000\000\
\\001\000\021\000\084\000\000\000\
\\001\000\021\000\085\000\000\000\
\\001\000\021\000\086\000\000\000\
\\001\000\021\000\087\000\000\000\
\\001\000\021\000\088\000\000\000\
\\001\000\022\000\096\000\000\000\
\\001\000\024\000\035\000\000\000\
\\001\000\037\000\133\000\000\000\
\\001\000\038\000\138\000\000\000\
\\001\000\041\000\126\000\000\000\
\\001\000\051\000\011\000\000\000\
\\001\000\051\000\021\000\000\000\
\\001\000\051\000\031\000\000\000\
\\001\000\051\000\037\000\000\000\
\\001\000\051\000\062\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\032\000\010\000\033\000\009\000\034\000\008\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\050\000\019\000\000\000\
\\151\000\000\000\
\\152\000\030\000\015\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\023\000\017\000\000\000\
\\156\000\000\000\
\\157\000\035\000\029\000\039\000\028\000\042\000\027\000\043\000\026\000\
\\044\000\025\000\051\000\024\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\186\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\"
val actionRowNumbers =
"\065\000\057\000\065\000\073\000\
\\076\000\062\000\070\000\069\000\
\\068\000\071\000\066\000\023\000\
\\064\000\058\000\063\000\078\000\
\\024\000\059\000\073\000\065\000\
\\078\000\053\000\043\000\060\000\
\\044\000\045\000\000\000\000\000\
\\067\000\071\000\074\000\075\000\
\\079\000\077\000\000\000\025\000\
\\061\000\000\000\041\000\000\000\
\\046\000\096\000\095\000\087\000\
\\047\000\048\000\049\000\050\000\
\\051\000\000\000\000\000\000\000\
\\000\000\000\000\086\000\094\000\
\\040\000\072\000\026\000\081\000\
\\052\000\032\000\076\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\005\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\007\000\
\\006\000\016\000\033\000\013\000\
\\076\000\080\000\027\000\028\000\
\\056\000\015\000\014\000\022\000\
\\020\000\021\000\019\000\018\000\
\\017\000\012\000\011\000\009\000\
\\010\000\008\000\004\000\002\000\
\\003\000\001\000\034\000\035\000\
\\036\000\037\000\038\000\042\000\
\\097\000\054\000\082\000\083\000\
\\029\000\089\000\093\000\092\000\
\\091\000\090\000\000\000\076\000\
\\085\000\039\000\055\000\088\000\
\\030\000\084\000\031\000"
val gotoT =
"\
\\001\000\138\000\002\000\005\000\003\000\004\000\007\000\003\000\
\\008\000\002\000\012\000\001\000\000\000\
\\000\000\
\\007\000\010\000\008\000\002\000\012\000\001\000\000\000\
\\010\000\012\000\011\000\011\000\000\000\
\\004\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\021\000\006\000\020\000\000\000\
\\000\000\
\\000\000\
\\010\000\030\000\011\000\011\000\000\000\
\\002\000\031\000\003\000\004\000\007\000\003\000\008\000\002\000\
\\012\000\001\000\000\000\
\\005\000\032\000\006\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\038\000\000\000\
\\013\000\056\000\000\000\
\\000\000\
\\009\000\057\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\058\000\000\000\
\\000\000\
\\000\000\
\\013\000\061\000\000\000\
\\000\000\
\\013\000\080\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\087\000\000\000\
\\013\000\088\000\000\000\
\\013\000\089\000\000\000\
\\013\000\090\000\000\000\
\\013\000\091\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\096\000\000\000\
\\013\000\097\000\000\000\
\\013\000\098\000\000\000\
\\013\000\099\000\000\000\
\\013\000\100\000\000\000\
\\013\000\101\000\000\000\
\\013\000\102\000\000\000\
\\013\000\103\000\000\000\
\\013\000\104\000\000\000\
\\013\000\105\000\000\000\
\\013\000\106\000\000\000\
\\013\000\107\000\000\000\
\\013\000\108\000\000\000\
\\013\000\109\000\000\000\
\\013\000\110\000\000\000\
\\013\000\111\000\000\000\
\\013\000\112\000\000\000\
\\013\000\113\000\000\000\
\\000\000\
\\013\000\114\000\000\000\
\\013\000\115\000\000\000\
\\013\000\116\000\000\000\
\\013\000\117\000\000\000\
\\013\000\118\000\000\000\
\\013\000\119\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\121\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\133\000\000\000\
\\004\000\134\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 139
val numrules = 59
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | BOOLEAN of unit ->  (string) | IDENTIFIER of unit ->  (string)
 | DECIMAL of unit ->  (Rational.rational)
 | NUM of unit ->  (BigInt.bigint)
 | expr of unit ->  (Type*Expression) | typedec of unit ->  (Type)
 | procdef of unit ->  (ProcDef)
 | procdecls of unit ->  (ProcDef list)
 | identlist of unit ->  (string list) | vardec of unit ->  (VarDecls)
 | vardeclis of unit ->  (VarDecls list) | cmd of unit ->  (Command)
 | ncmd of unit ->  (Command list) | cmdseq of unit ->  (Command list)
 | decseq of unit ->  (DeclarationSeq) | block of unit ->  (Block)
 | start of unit ->  (ProgramAST)
end
type svalue = MlyValue.svalue
type result = ProgramAST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 52) => true | (T 53) => true | (T 54) => true | (T 55) => true
 | (T 29) => true | (T 30) => true | (T 31) => true | (T 32) => true
 | (T 33) => true | (T 34) => true | (T 35) => true | (T 36) => true
 | (T 37) => true | (T 38) => true | (T 39) => true | (T 40) => true
 | (T 41) => true | (T 42) => true | (T 43) => true | (T 44) => true
 | (T 45) => true | (T 46) => true | (T 47) => true | (T 48) => true
 | (T 49) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 4))::
(nil
,nil
 $$ (T 5))::
(nil
,nil
 $$ (T 7))::
(nil
,nil
 $$ (T 6))::
nil
val noShift = 
fn (T 2) => true | _ => false
val showTerminal =
fn (T 0) => "NUM"
  | (T 1) => "SEMI"
  | (T 2) => "EOF"
  | (T 3) => "DECIMAL"
  | (T 4) => "RATPLUS"
  | (T 5) => "RATTIMES"
  | (T 6) => "RATSUB"
  | (T 7) => "RATDIV"
  | (T 8) => "INTPLUS"
  | (T 9) => "INTTIMES"
  | (T 10) => "INTSUB"
  | (T 11) => "INTDIV"
  | (T 12) => "INTMOD"
  | (T 13) => "EQUAL"
  | (T 14) => "NOTEQUAL"
  | (T 15) => "LESS"
  | (T 16) => "GREATER"
  | (T 17) => "LESSEQ"
  | (T 18) => "GREATEREQ"
  | (T 19) => "ASSIGN"
  | (T 20) => "LPAREN"
  | (T 21) => "RPAREN"
  | (T 22) => "LBRACE"
  | (T 23) => "RBRACE"
  | (T 24) => "NOT"
  | (T 25) => "ANDALSO"
  | (T 26) => "ORELSE"
  | (T 27) => "NEGATE"
  | (T 28) => "INTNEGATE"
  | (T 29) => "PROCEDURE"
  | (T 30) => "VAR"
  | (T 31) => "RATIONAL"
  | (T 32) => "INTEGER"
  | (T 33) => "BOOL"
  | (T 34) => "IF"
  | (T 35) => "THEN"
  | (T 36) => "ELSE"
  | (T 37) => "FI"
  | (T 38) => "WHILE"
  | (T 39) => "DO"
  | (T 40) => "OD"
  | (T 41) => "PRINT"
  | (T 42) => "READ"
  | (T 43) => "CALL"
  | (T 44) => "MAKERAT"
  | (T 45) => "SHOWRAT"
  | (T 46) => "SHOWDEC"
  | (T 47) => "FROMDEC"
  | (T 48) => "TODEC"
  | (T 49) => "COMMA"
  | (T 50) => "IDENTIFIER"
  | (T 51) => "BOOLEAN"
  | (T 52) => "TRUE"
  | (T 53) => "FALSE"
  | (T 54) => "RAT"
  | (T 55) => "INVERSE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 55) $$ (T 54) $$ (T 53) $$ (T 52) $$ (T 49) $$ (T 48) $$ (T 47)
 $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40)
 $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.block block1, block1left, block1right)) :: 
rest671)) => let val  result = MlyValue.start (fn _ => let val  (block
 as block1) = block1 ()
 in (HashTable.clear typeTable ; Program(block))
end)
 in ( LrTable.NT 0, ( result, block1left, block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.cmdseq cmdseq1, _, cmdseq1right)) :: ( _, ( 
MlyValue.decseq decseq1, decseq1left, _)) :: rest671)) => let val  
result = MlyValue.block (fn _ => let val  (decseq as decseq1) = 
decseq1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in ((Block(decseq,cmdseq) ))
end)
 in ( LrTable.NT 1, ( result, decseq1left, cmdseq1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.procdecls procdecls1, _, procdecls1right))
 :: ( _, ( MlyValue.vardeclis vardeclis1, vardeclis1left, _)) :: 
rest671)) => let val  result = MlyValue.decseq (fn _ => let val  (
vardeclis as vardeclis1) = vardeclis1 ()
 val  (procdecls as procdecls1) = procdecls1 ()
 in ((DeclarationSeq(vardeclis,procdecls)))
end)
 in ( LrTable.NT 2, ( result, vardeclis1left, procdecls1right), 
rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.vardeclis (fn _ => (
([])))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.vardeclis vardeclis1, _, vardeclis1right))
 :: ( _, ( MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) =>
 let val  result = MlyValue.vardeclis (fn _ => let val  (vardec as 
vardec1) = vardec1 ()
 val  (vardeclis as vardeclis1) = vardeclis1 ()
 in ((vardec :: vardeclis))
end)
 in ( LrTable.NT 6, ( result, vardec1left, vardeclis1right), rest671)

end
|  ( 5, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.identlist 
identlist1, _, _)) :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _))
 :: ( _, ( MlyValue.typedec typedec1, typedec1left, _)) :: rest671))
 => let val  result = MlyValue.vardec (fn _ => let val  (typedec as 
typedec1) = typedec1 ()
 val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (identlist as identlist1) = identlist1 ()
 in (
(insertInTable( IDENTIFIER :: identlist  , typedec) ;VarDecls(IDENTIFIER :: identlist,typedec))
)
end)
 in ( LrTable.NT 7, ( result, typedec1left, SEMI1right), rest671)
end
|  ( 6, ( ( _, ( _, RATIONAL1left, RATIONAL1right)) :: rest671)) =>
 let val  result = MlyValue.typedec (fn _ => ((Rational)))
 in ( LrTable.NT 11, ( result, RATIONAL1left, RATIONAL1right), rest671
)
end
|  ( 7, ( ( _, ( _, INTEGER1left, INTEGER1right)) :: rest671)) => let
 val  result = MlyValue.typedec (fn _ => ((Bigint)))
 in ( LrTable.NT 11, ( result, INTEGER1left, INTEGER1right), rest671)

end
|  ( 8, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.typedec (fn _ => ((Bool)))
 in ( LrTable.NT 11, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.identlist (fn _ => (
([])))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.identlist identlist1, _, identlist1right))
 :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, 
COMMA1left, _)) :: rest671)) => let val  result = MlyValue.identlist
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (identlist as identlist1) = identlist1 ()
 in ((IDENTIFIER :: identlist))
end)
 in ( LrTable.NT 8, ( result, COMMA1left, identlist1right), rest671)

end
|  ( 11, ( rest671)) => let val  result = MlyValue.procdecls (fn _ =>
 (([])))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.procdecls procdecls1, _, procdecls1right))
 :: _ :: ( _, ( MlyValue.procdef procdef1, procdef1left, _)) :: 
rest671)) => let val  result = MlyValue.procdecls (fn _ => let val  (
procdef as procdef1) = procdef1 ()
 val  (procdecls as procdecls1) = procdecls1 ()
 in (( procdef:: procdecls ))
end)
 in ( LrTable.NT 9, ( result, procdef1left, procdecls1right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.block block1, _, block1right)) :: ( _, ( 
MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, PROCEDURE1left, _
)) :: rest671)) => let val  result = MlyValue.procdef (fn _ => let
 val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (block as block1) = block1 ()
 in ((ProcDef(IDENTIFIER,block)))
end)
 in ( LrTable.NT 10, ( result, PROCEDURE1left, block1right), rest671)

end
|  ( 14, ( rest671)) => let val  result = MlyValue.cmdseq (fn _ => (
([])))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 15, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ncmd ncmd1,
 _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result
 = MlyValue.cmdseq (fn _ => let val  (ncmd as ncmd1) = ncmd1 ()
 in ((ncmd))
end)
 in ( LrTable.NT 3, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 16, ( rest671)) => let val  result = MlyValue.ncmd (fn _ => (([])
))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ncmd ncmd1, _, ncmd1right)) :: ( _, ( 
MlyValue.cmd cmd1, cmd1left, _)) :: rest671)) => let val  result = 
MlyValue.ncmd (fn _ => let val  (cmd as cmd1) = cmd1 ()
 val  (ncmd as ncmd1) = ncmd1 ()
 in ((cmd ::ncmd ))
end)
 in ( LrTable.NT 4, ( result, cmd1left, ncmd1right), rest671)
end
|  ( 18, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.expr expr1, _,
 _)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left,
 _)) :: rest671)) => let val  result = MlyValue.cmd (fn _ => let val 
 (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (expr as expr1) = expr1 ()
 in (
(  if (checkSameType( getType IDENTIFIER,  #1 expr)) then () else raise TypeMisMatchException ; AssignmentCmd(IDENTIFIER,#2 expr) )
)
end)
 in ( LrTable.NT 5, ( result, IDENTIFIER1left, SEMI1right), rest671)

end
|  ( 19, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.IDENTIFIER 
IDENTIFIER1, _, _)) :: ( _, ( _, CALL1left, _)) :: rest671)) => let
 val  result = MlyValue.cmd (fn _ => let val  (IDENTIFIER as 
IDENTIFIER1) = IDENTIFIER1 ()
 in ((CallCmd(IDENTIFIER)))
end)
 in ( LrTable.NT 5, ( result, CALL1left, SEMI1right), rest671)
end
|  ( 20, ( ( _, ( _, _, SEMI1right)) :: _ :: ( _, ( 
MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: _ :: ( _, ( _, READ1left, _
)) :: rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (
IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in ((ReadCmd(IDENTIFIER)))
end)
 in ( LrTable.NT 5, ( result, READ1left, SEMI1right), rest671)
end
|  ( 21, ( ( _, ( _, _, SEMI1right)) :: _ :: ( _, ( MlyValue.expr 
expr1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) => let
 val  result = MlyValue.cmd (fn _ => let val  (expr as expr1) = expr1
 ()
 in ((PrintCmd(#2 expr)))
end)
 in ( LrTable.NT 5, ( result, PRINT1left, SEMI1right), rest671)
end
|  ( 22, ( ( _, ( _, _, SEMI1right)) :: _ :: ( _, ( MlyValue.cmdseq 
cmdseq2, _, _)) :: _ :: ( _, ( MlyValue.cmdseq cmdseq1, _, _)) :: _ ::
 ( _, ( MlyValue.expr expr1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (expr
 as expr1) = expr1 ()
 val  cmdseq1 = cmdseq1 ()
 val  cmdseq2 = cmdseq2 ()
 in (
(if( checkBool(#1 expr)) then () else raise TypeMisMatchException ;ConditionalCmd(#2 expr, cmdseq1, cmdseq2 ) )
)
end)
 in ( LrTable.NT 5, ( result, IF1left, SEMI1right), rest671)
end
|  ( 23, ( ( _, ( _, _, SEMI1right)) :: _ :: ( _, ( MlyValue.cmdseq 
cmdseq1, _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _)) :: ( _, ( _
, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.cmd (fn _
 => let val  (expr as expr1) = expr1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (
( if( checkBool(#1 expr)) then () else raise TypeMisMatchException ; WhileCmd(#2 expr,cmdseq))
)
end)
 in ( LrTable.NT 5, ( result, WHILE1left, SEMI1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkRat(#1 expr1) andalso checkRat( #1 expr2) then () else raise TypeMisMatchException ;(Rational, RatAdd(#2 expr1 , #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
(if checkRat(#1 expr1) andalso checkRat( #1 expr2) then () else raise TypeMisMatchException ;(Rational, RatSub(#2 expr1 , #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkRat(#1 expr1) andalso checkRat( #1 expr2) then () else raise TypeMisMatchException ;(Rational,RatMul(#2 expr1 , #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkRat(#1 expr1) andalso checkRat( #1 expr2) then () else raise TypeMisMatchException ;(Rational,RatDiv(#2 expr1 , #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
INVERSE1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  (expr as expr1) = expr1 ()
 in (
( if checkRat(#1 expr) then () else  raise TypeMisMatchException ; (Rational ,RatInv(#2 expr)) )
)
end)
 in ( LrTable.NT 12, ( result, INVERSE1left, expr1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn _
 => let val  (expr as expr1) = expr1 ()
 in (
(if checkRat(#1 expr) then () else  raise TypeMisMatchException ; (Rational , Neg(#2 expr)) )
)
end)
 in ( LrTable.NT 12, ( result, NEGATE1left, expr1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
INTNEGATE1left, _)) :: rest671)) => let val  result = MlyValue.expr
 (fn _ => let val  (expr as expr1) = expr1 ()
 in (
(if checkInt(#1 expr) then () else  raise TypeMisMatchException ; (Bigint , IntNeg(#2 expr)) )
)
end)
 in ( LrTable.NT 12, ( result, INTNEGATE1left, expr1right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.DECIMAL DECIMAL1, DECIMAL1left, 
DECIMAL1right)) :: rest671)) => let val  result = MlyValue.expr (fn _
 => let val  (DECIMAL as DECIMAL1) = DECIMAL1 ()
 in (( (Rational, RationalNum(DECIMAL)) ))
end)
 in ( LrTable.NT 12, ( result, DECIMAL1left, DECIMAL1right), rest671)

end
|  ( 32, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in (( getType IDENTIFIER  , Identifier(IDENTIFIER) ))
end)
 in ( LrTable.NT 12, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 33, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr2,
 _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _)) :: _ :: ( _, ( _, 
MAKERAT1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkInt( #1 expr1) andalso checkInt(#1 expr2) then () else raise TypeMisMatchException ; (Rational,RatMake(#2 expr1, #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, MAKERAT1left, RPAREN1right), rest671)

end
|  ( 34, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: _ :: ( _, ( _, RAT1left, _)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (
( if checkInt(#1 expr) then () else raise TypeMisMatchException ; (Rational, RatRat(#2 expr)) )
)
end)
 in ( LrTable.NT 12, ( result, RAT1left, RPAREN1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: _ :: ( _, ( _, SHOWRAT1left, _)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (
(if checkRat(#1 expr) then () else  raise TypeMisMatchException ; (Rational , RatShow(#2 expr)) )
)
end)
 in ( LrTable.NT 12, ( result, SHOWRAT1left, RPAREN1right), rest671)

end
|  ( 36, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: _ :: ( _, ( _, SHOWDEC1left, _)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (
(if checkRat(#1 expr) then () else  raise TypeMisMatchException ; (Rational , RatShowDec(#2 expr)) )
)
end)
 in ( LrTable.NT 12, ( result, SHOWDEC1left, RPAREN1right), rest671)

end
|  ( 37, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: _ :: ( _, ( _, FROMDEC1left, _)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (
(if checkRat(#1 expr) then () else  raise TypeMisMatchException ; (Rational , RatFromDec(#2 expr)) )
)
end)
 in ( LrTable.NT 12, ( result, FROMDEC1left, RPAREN1right), rest671)

end
|  ( 38, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: _ :: ( _, ( _, TODEC1left, _)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (
(if checkRat(#1 expr) then () else  raise TypeMisMatchException ; (Rational , RatToDec(#2 expr)) )
)
end)
 in ( LrTable.NT 12, ( result, TODEC1left, RPAREN1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkInt( #1 expr1) andalso checkInt(#1 expr2) then () else raise TypeMisMatchException ;(Bigint , IntAdd(#2 expr1, #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkInt( #1 expr1) andalso checkInt(#1 expr2) then () else raise TypeMisMatchException ;(Bigint ,IntSub(#2 expr1 , #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkInt( #1 expr1) andalso checkInt(#1 expr2) then () else raise TypeMisMatchException ;(Bigint ,IntMul(#2 expr1 , #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkInt( #1 expr1) andalso checkInt(#1 expr2) then () else raise TypeMisMatchException ;(Bigint ,IntDiv(#2 expr1 , #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkInt( #1 expr1) andalso checkInt(#1 expr2) then () else raise TypeMisMatchException ;(Bigint ,IntMod(#2 expr1 , #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
INTPLUS1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  (expr as expr1) = expr1 ()
 in (expr)
end)
 in ( LrTable.NT 12, ( result, INTPLUS1left, expr1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.expr (fn _ => let val  (NUM as NUM1) =
 NUM1 ()
 in (( Bigint , BigInt(NUM)))
end)
 in ( LrTable.NT 12, ( result, NUM1left, NUM1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
(  if checkBool(#1 expr1) andalso checkBool(#1 expr2) then () else raise TypeMisMatchException ;(Bool ,BoolAnd(#2 expr1, #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkBool(#1 expr1) andalso checkBool(#1 expr2) then () else raise TypeMisMatchException ;(Bool , BoolOr(#2 expr1, #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn _ =>
 let val  (expr as expr1) = expr1 ()
 in (
( if checkBool (#1 expr) then () else raise TypeMisMatchException ; (Bool ,BoolNot(#2 expr)) )
)
end)
 in ( LrTable.NT 12, ( result, NOT1left, expr1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
( if checkSameType( #1 expr1 , #1 expr2) then () else raise TypeMisMatchException ;(Bool , BoolEqual(#2 expr1, #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
(  if checkSameType( #1 expr1 , #1 expr2) then () else raise TypeMisMatchException ;(Bool , BoolNotEqual(#2 expr1, #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
(  if checkSameType( #1 expr1 , #1 expr2) then () else raise TypeMisMatchException ;(Bool , BoolLessThan(#2 expr1, #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
(  if checkSameType( #1 expr1 , #1 expr2) then () else raise TypeMisMatchException ;(Bool , BoolLessThanOrEqual(#2 expr1, #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
(  if checkSameType( #1 expr1 , #1 expr2) then () else raise TypeMisMatchException ;(Bool , BoolGreaterThan(#2 expr1, #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
(  if checkSameType( #1 expr1 , #1 expr2) then () else raise TypeMisMatchException ;(Bool , BoolGreaterThanOrEqual(#2 expr1, #2 expr2)) )
)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr2right), rest671)
end
|  ( 55, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => ((Bool , BoolTrue)))
 in ( LrTable.NT 12, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 56, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.expr (fn _ => ((Bool ,BoolFalse)))
 in ( LrTable.NT 12, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 57, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (expr)
end)
 in ( LrTable.NT 12, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 58, ( ( _, ( MlyValue.expr expr1, expr1left, expr1right)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  (expr
 as expr1) = expr1 ()
 in (expr)
end)
 in ( LrTable.NT 12, ( result, expr1left, expr1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : rationalPL0_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun DECIMAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.DECIMAL (fn () => i),p1,p2))
fun RATPLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun RATTIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RATSUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RATDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun INTPLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun INTTIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun INTSUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun INTDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun INTMOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun NOTEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATEREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDALSO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ORELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun INTNEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RATIONAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun INTEGER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOWRAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOWDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun FROMDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun TODEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
fun BOOLEAN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.BOOLEAN (fn () => i),p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(
ParserData.MlyValue.VOID,p1,p2))
fun RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 54,(
ParserData.MlyValue.VOID,p1,p2))
fun INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 55,(
ParserData.MlyValue.VOID,p1,p2))
end
end
