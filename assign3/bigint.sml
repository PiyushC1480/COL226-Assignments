signature BIGINT =
sig
	type bigint
  	val stripZeroes : bigint -> bigint
  	val negate : bigint -> bigint
  	val abs : bigint -> bigint
	val add : bigint * bigint -> bigint
	val equal : bigint * bigint -> bool
	val less : bigint * bigint -> bool
	val greater : bigint * bigint -> bool
	val lessEq : bigint * bigint -> bool
	val greaterEq : bigint * bigint -> bool
	val subtract : bigint * bigint -> bigint
	val multiply : bigint * bigint -> bigint
	val quotient : bigint * bigint -> bigint 
	val remainder : bigint*bigint ->bigint 
	val gcd : bigint * bigint -> bigint
	val lcm : bigint * bigint -> bigint
	val toString : bigint -> string
	val fromString: string -> bigint
	val one : bigint
	val zero :  bigint
	val empty : bigint
	(* val modulo : bigint * bigint -> bigint option
	val pow : bigint * int -> bigint
	val isEven : bigint -> bool
	val isOdd : bigint -> bool
	val compare : bigint * bigint -> order
	 *)
	end

structure BigInt: BIGINT =
struct
	type bigint = string (*string representation of a bigint*)
	
	(* Internal Helper functions *)

fun addZeroes(str: bigint, i: int): bigint =
		let
			val zeroes = String.implode(List.tabulate(i, fn _ => #"0"))
		in
			zeroes ^ str
		end
fun addZeroes2(str: bigint, i: int): bigint =
		let
			val zeroes = String.implode(List.tabulate(i, fn _ => #"0"))
		in
			str^zeroes
		end
fun rev(str: bigint) : bigint = String.rev(str)
fun charToInt c =
		let
			val digitCode = ord(c)
			val zeroCode = ord(#"0")
		in
			digitCode - zeroCode
		end


	(* Signature functions *)
	fun stripZeroes(str: bigint): bigint =
		if size str = 0 orelse str = "0" then
			""
		else if String.sub(str,0) = #"0" then
			stripZeroes(String.substring(str, 1, size str - 1))
		else
			str

	fun negate(str : bigint) : bigint = if String.sub(str,0) = #"-" then String.substring(str,1,String.size(str)-1) else "-" ^ str

	fun abs(str: bigint) : bigint  =
	if str ="" then "" else
		if String.sub(str,0) = #"-" orelse String.sub(str,0) = #"+" then String.substring(str,1,String.size(str)-1) else str
	
	
	fun equal(a:bigint , b : bigint) : bool = 
		let 
			fun eqrecurse(x:bigint,y:bigint,i:int): bool = 
				if i = String.size(x) then true
				else if String.sub(x,i) <> String.sub(y,i) then false
				else eqrecurse(x,y,i+1)
		in
			if String.size(a) <> String.size(b) then false
			else eqrecurse(a,b,0)
		end

	fun less(a:bigint,b : bigint) :bool = 
		let
			fun lessrecurse(x:string,y:string,i:int): bool = 
				if i = String.size(x) then false
				else if String.sub(x,i) < String.sub(y,i) then true
				else lessrecurse(x,y,i+1)
		in
			let 
				val m = stripZeroes(abs(a))
				val n = stripZeroes(abs(b))
			in
				if String.size(n) < String.size(m) then false
				else if String.size(m) < String.size(n) then true
				else lessrecurse(m,n,0)
			end
		end
	fun greater(a:bigint,b : bigint) :bool = 
		let
			fun grtrecurse(x:string,y:string,i:int): bool = 
				if i = String.size(x) then true
                else if String.sub(x,i) > String.sub(y,i) then true
				else if String.sub(x,i) < String.sub(y,i) then false
				else grtrecurse(x,y,i+1)
		in
			let 
				val m = stripZeroes(abs(a))
				val n = stripZeroes(abs(b))
			in
			if String.size(n) < String.size(m) then true
			else if String.size(m) < String.size(n) then false
			else grtrecurse(m,n,0)
			end
		end

	fun lessEq(a:bigint,b : bigint) :bool = 
		let
			fun lessEqrecurse(x:string,y:string,i:int): bool = 
				if i = String.size(x) then true
				else if String.sub(x,i) < String.sub(y,i) then true
				else if String.sub(x,i) > String.sub(y,i) then false
				else lessEqrecurse(x,y,i+1)
		in
			let
			  val m = stripZeroes(abs(a))
			  val n = stripZeroes(abs(b))
			in
			if String.size(n) < String.size(m) then false
			else if String.size(m) < String.size(n) then true
			else lessEqrecurse(m,n,0)
			end
		end

	fun greaterEq(a:bigint,b : bigint) :bool = 
		let
			fun grtEqrecurse(x:string,y:string,i:int): bool = 
				if i = String.size(x) then true
				else if String.sub(x,i) > String.sub(y,i) then true
        		else if String.sub(x,i) < String.sub(y,i) then false
				else grtEqrecurse(x,y,i+1)
		in
			let 
				val m = stripZeroes(abs(a))
				val n = stripZeroes(abs(b))
			in
			if String.size(n) < String.size(m) then true
			else if String.size(m) < String.size(n) then false
			else grtEqrecurse(a,b,0)
			end
		end
	fun add(a : bigint , b: bigint): string = 
		let
			fun adder(x : bigint, y: bigint, i :int , carry : int, ans: bigint) :bigint = 
				if i  = String.size(x) andalso carry = 0 then String.rev(ans)
				else if i  = String.size(x) andalso carry = 1 then String.rev(ans^"1")
				else
					let 
						val s = charToInt(String.sub(x,i)) + charToInt(String.sub(y,i)) + carry
					in 
						if s >=10 then 
							let 
								val c = Int.toString(s-10);
							in 
								adder(x,y,i+1,1,ans^c)
							end
						else 
							adder(x,y,i+1,0,ans^(Int.toString(s)))
					end
		in
			if String.sub(a,0) = #"-" then
				if String.sub(b,0) = #"-"  then
					let 	
						val c=  abs(a)
						val d = abs(b) 
					in 
						if String.size(c) > String.size(d) then
							let 
								val d1 = addZeroes(d,(String.size(c)-String.size(d)))
							in 
								"-" ^ adder(String.rev(c),String.rev(d1),0,0,"")
							end
						else if String.size(c) < String.size(d) then
							let 
								val c1 = addZeroes(c,(String.size(d)-String.size(c)))
							in 
								"-" ^ adder(String.rev(c1),String.rev(d),0,0,"")
							end
						else 
							"-" ^ adder(String.rev(c),String.rev(d),0,0,"")
					end
				else 
					subtract(b,abs(a))
					(* string.subtract(b,abs(a)) *)
			
			else
				if String.sub(b,0) = #"-" then 
					subtract(a,abs(b))
					(* string.subtract(a,abs(b)) *)
				else
					let 	
						val c=  abs(a)
						val d = abs(b)
					in 
						if String.size(c) > String.size(d) then
							let 
								val d1 = addZeroes(d,(String.size(c)-String.size(d)))
							in 
								adder(String.rev(c),String.rev(d1),0,0,"")
							end
						else if String.size(c) < String.size(d) then
							let 
								val c1 = addZeroes(c,(String.size(d)-String.size(c)))
							in 
								adder(String.rev(c1),String.rev(d),0,0,"")
							end
						else 
							adder(String.rev(c),String.rev(d),0,0,"")
					end
		end
		
	and subtract(a:bigint, b:bigint):bigint = 
		let 
			fun subtractor(x:bigint , y : bigint , i : int , carry : int, ans : bigint): bigint = 
				if  i = String.size(x) then String.rev(ans)
				else
					let
						val s = charToInt(String.sub(x,i)) - charToInt(String.sub(y,i)) + carry
					in
						if s < 0  then 
							let
								val ns = Int.toString(10 + s)
							in 
								subtractor(x,y,i+1,~1,ans^ns)
							end
						else 
							subtractor(x,y,i+1,0,ans^Int.toString(s))
					end
		in 
			if String.sub(a,0) = #"-" then
				if String.sub(b,0) = #"-"  then
					let 	
						val c = abs(a)
						val d = abs(b) 
					in 
						if String.size(c) > String.size(d) then
							let 
								val d1 = addZeroes(d,(String.size(c)-String.size(d)))
							in 
								"-"^subtractor(String.rev(c),String.rev(d1),0,0,"")
							end
						else if String.size(c) < String.size(d) then
							let 
								val c1 = addZeroes(c,(String.size(d)-String.size(c)))
							in 
								subtractor(String.rev(d),String.rev(c1),0,0,"")
							end	
						else 
							if greaterEq(c,d) then "-" ^subtractor(String.rev(c),String.rev(d),0,0,"")
							else subtractor(String.rev(d),String.rev(c),0,0,"")
					end
				else 
					add(a,negate(b))
			else
				if String.sub(b,0) = #"-" then 
					add(a,abs(b))
				else
					let 	
						val c = abs(a)
						val d = abs(b) 
					in 
						if String.size(c) > String.size(d) then
							let 
								val d1 = addZeroes(d,(String.size(c)-String.size(d)))
							in 
								subtractor(String.rev(c),String.rev(d1),0,0,"")
							end
						else if String.size(c) < String.size(d) then
							let 
								val c1 = addZeroes(c,(String.size(d)-String.size(c)))
							in 
								"-"^subtractor(String.rev(d),String.rev(c1),0,0,"")
							end	
						else 
							if greaterEq(c,d) then subtractor(String.rev(c),String.rev(d),0,0,"")
							else "-"^subtractor(String.rev(d),String.rev(c),0,0,"")
					end
			end
	fun multiply(a:bigint,b:bigint):bigint= 
		let
			fun multiplier(x:bigint,y:bigint,i:int,ans:bigint):bigint =
				let 
					fun sigdtmul(p:bigint,q:bigint,j:int,carry:int,tempans:bigint):bigint=
						if j = String.size(p) andalso carry = 0 then tempans
            			else if j = String.size(p) andalso carry <> 0 then Int.toString(carry)^tempans
						else
							let
								val k = charToInt(String.sub(p,j))*charToInt(String.sub(q,i)) + carry
							in 
								if k>=10 then
									let
										val ca =  k div 10
                    					val nk = k mod 10
									in 
										sigdtmul(p,q,j+1,ca,Int.toString(nk)^tempans)
									end
								else
									sigdtmul(p,q,j+1,0,Int.toString(k)^tempans)
							end
				in 
					if i = String.size(y) then ans
					else
						let 
							val h = sigdtmul(x,y,0,0,"")
						in
							if i = 0 then 
							  	multiplier(x,y,i+1,h)
              				else
                				multiplier(x,y,i+1,add(ans,addZeroes2(h,i)))
						end
				end
		in 
			multiplier(String.rev(a),String.rev(b),0,"")
		end
fun gusser(divisor:bigint,rem:bigint):bigint = 
    let
        fun recur (divs:bigint,r:bigint,i:int):bigint = 
            let 
                val m = multiply(divs,Int.toString(i))
            in 
                if equal(m,rem) then Int.toString(i)
                else if greater(m,rem) then Int.toString(i-1)
                else recur(divs,r,i+1)
            end
    in
        recur(divisor,rem,0)
    end

	fun divider(x:bigint,y:bigint,answer:bool):bigint = 
		let 
			fun divi(divident:bigint,divisor:bigint,i:int , quo:bigint,rem:bigint,car:int,to_return:bool):bigint = 
				if i =String.size(divident) then 
					if greaterEq(rem,divisor) then 
						let 
							val g = gusser(divisor,rem)
						in 
							let 
								val nrem = subtract(rem,multiply(divisor,g))
							in
								divi(divident,divisor,i,quo^g,stripZeroes(nrem),0,to_return)
							end
						end
					else
					if to_return then 
					if car >=1 then stripZeroes(quo^"0") else stripZeroes(quo)
					else rem
				else
					if lessEq(divisor,rem) then 
						let 
							val quo_append = gusser(divisor,rem)
						in
							let
								val nrem_after_sub = subtract(rem,multiply(divisor,quo_append))
							in
								divi(divident,divisor,i,quo^quo_append,stripZeroes(nrem_after_sub),0,to_return)
							end
						end
					
					else
						let
							val nrem = rem ^ Char.toString(String.sub(divident,i))
							(* val ni  = i+1 *)
						in 
							if car >= 1 then 
							divi(divident,divisor,i+1,quo^"0",nrem,car+1,to_return)
							else
							divi(divident,divisor,i+1,quo,nrem,car+1,to_return)
						end
		in
			divi(x,y,0,"","",0,answer)
		end
	fun quotient(a:bigint,b:bigint):bigint = 
		let 
			val m = abs(a)
			val n = abs(b)
		in
			let 
				val m = divider(a,b,true)
			in
				if (m="") then "0"
				else if String.sub(a,0) = #"-" orelse String.sub(b,0) = #"-" then "-"^divider(a,b,true)
				else divider(a,b,true)
			end
		end
	fun remainder(a:bigint,b:bigint):bigint = 
		let 
			val m = abs(a)
			val n = abs(b)
		in
			if String.sub(a,0) = #"-" orelse String.sub(b,0) = #"-" then "-"^divider(a,b,true)
			else divider(a,b,false)
		end

	fun gcd (a :bigint, b:bigint):bigint =
		let
		  val m =  stripZeroes(a)
		  val n = stripZeroes(b)
		in
		  if (m = "0" orelse m="") then n
		else gcd (divider(n,m,false),m)
		end
		

	fun lcm(a:bigint,b:bigint):bigint = 
		let
			val gcd_ab = gcd(a,b)
		in
			divider(multiply(a,b),gcd_ab,true)
		end
	fun toString(a:bigint):string = a
	fun fromString(a:string):bigint = a
	val zero = "0"
	val empty = ""
	val one  = "1"
end



