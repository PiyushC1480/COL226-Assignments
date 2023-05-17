signature STACK =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create_stack : 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val is_empty: 'a Stack -> bool 
    val poptop : 'a Stack -> ('a * 'a Stack) option
    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int  
    val toString: ('a -> string) -> 'a Stack -> string 
    val stack2list: 'a Stack -> 'a list (* Convert a list into a stack *)
end

structure FunctionStack :> STACK = 
struct
    datatype 'a Stack = Nil_Stack | CONS of 'a * 'a Stack   
    exception EmptyStack
    exception Error of string

    val create_stack = Nil_Stack
    val push = fn(x,y) => CONS(x,y)
    val pop = fn(x) => case x of Nil_Stack => raise EmptyStack  
                        | CONS(topElement,rest) => rest

    val top = fn x => case x of Nil_Stack => raise EmptyStack
                        | CONS(topElement,rest) =>topElement
    fun poptop(x) = case x of Nil_Stack => NONE | CONS(topElement,rest ) => SOME(topElement,rest)
    val is_empty = fn x => case x of Nil_Stack => true 
                        | CONS(topElement,rest_) => false

    fun depth(x) = case x of Nil_Stack => 0 
                    |  CONS(topElement,rest) => 1 + depth(rest)

    fun nth(x,n) = 
        if( n < 0  orelse n>= depth(x) ) then 
            raise Error( "Invalid n provided") 
        else
            if( n  =  0 ) then  
                top(x)
            else 
                nth( pop(x) , n-1)
        
    fun drop( x , n ) = 
        if( n < 0  orelse n > depth(x) ) then 
            raise Error("Invalid argument for drop")
        else 
            if( n = 0 ) then 
                x
            else 
                drop( pop(x) , n-1 )
        
    fun toString f x  = 
        case x of Nil_Stack => "" 
            | CONS(topElement,rest) => let
                                            val delimiter = if ( is_empty (rest)) then "" else "."
                                        in
                                            f(topElement) ^  delimiter  ^  ( toString f rest ) 
                                        end 
    fun stack2list s = 
        case s of Nil_Stack => [] 
                 | CONS(x,y) => x :: stack2list y 
end
