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
				if i = String.size(x) then true
				else if charToInt(String.sub(x,i)) >= charToInt(String.sub(y,i)) then false
				else lessrecurse(x,y,i+1)
		in
			let 
				val m = stripZeroes(abs(a))
				val n = stripZeroes(abs(b))
			in
				if String.size(n) < String.size(m) then false
				else if String.size(m) < String.size(n) then true
				else if String.sub(a,0) = #"-" andalso String.sub(b,0) <> #"-" then true
				else if String.sub(a,0) <> #"-" andalso String.sub(b,0) = #"-" then false
				else if String.sub(a,0) = #"-" andalso String.sub(b,0) = #"-" then less(n,m)
				else lessrecurse(m,n,0)
			end
		end
	fun greater(a:bigint,b : bigint) :bool = 
		let
			fun grtrecurse(x:string,y:string,i:int): bool = 
				if i = String.size(x) then true
                else if charToInt(String.sub(x,i)) > charToInt(String.sub(y,i)) then true
				else if charToInt(String.sub(x,i)) < charToInt(String.sub(y,i)) then false
				else grtrecurse(x,y,i+1)
		in
			let 
				val m = stripZeroes(abs(a))
				val n = stripZeroes(abs(b))
			in
			if String.sub(a,0) = #"-" andalso String.sub(b,0) <> #"-" then false
			else if String.sub(a,0) <> #"-" andalso String.sub(b,0) = #"-" then true
			else if String.sub(a,0) = #"-" andalso String.sub(b,0) = #"-" then greater(n,m)
			else if String.size(n) < String.size(m) then true
			else if String.size(m) < String.size(n) then false
			else grtrecurse(m,n,0)
			end
		end

	fun lessEq(a:bigint,b : bigint) :bool = 
		let
			fun lessEqrecurse(x:string,y:string,i:int): bool = 
				if i = String.size(x) then true
				else if charToInt(String.sub(x,i)) < charToInt(String.sub(y,i)) then true
				else if charToInt(String.sub(x,i)) > charToInt(String.sub(y,i)) then false
				else lessEqrecurse(x,y,i+1)
		in
			let
			  val m = stripZeroes(abs(a))
			  val n = stripZeroes(abs(b))
			in
			if String.sub(a,0) = #"-" andalso String.sub(b,0) <> #"-" then true
			else if String.sub(a,0) <> #"-" andalso String.sub(b,0) = #"-" then false
			else if String.sub(a,0) = #"-" andalso String.sub(b,0) = #"-" then lessEq(n,m)
			else if String.size(n) < String.size(m) then false
			else if String.size(m) < String.size(n) then true
			else lessEqrecurse(m,n,0)
			end
		end

	fun greaterEq(a:bigint,b : bigint) :bool = 
		let
			fun grtEqrecurse(x:string,y:string,i:int): bool = 
				if i = String.size(x) then true
				else if charToInt(String.sub(x,i)) > charToInt(String.sub(y,i)) then true
        		else if charToInt(String.sub(x,i)) < charToInt(String.sub(y,i)) then false
				else grtEqrecurse(x,y,i+1)
		in
			let 
				val m = stripZeroes(abs(a))
				val n = stripZeroes(abs(b))
			in
			if String.sub(a,0) = #"-" andalso String.sub(b,0) <> #"-" then false
			else if String.sub(a,0) <> #"-" andalso String.sub(b,0) = #"-" then true
			else if String.sub(a,0) = #"-" andalso String.sub(b,0) = #"-" then greaterEq(n,m)
			else if String.size(n) < String.size(m) then true
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
			let 
				val m = abs(a)
				val n = abs(b)
			in
				let 
				val ans = multiplier(String.rev(m),String.rev(n),0,"")
				in
					if String.sub(a,0) = #"-" then 
						if String.sub(b,0) = #"-" then ans
						else negate(ans)
					else 
						if String.sub(b,0) = #"-" then negate(ans)
						else ans
				end
			end
			
		end
fun gusser(divisor:bigint,rem:bigint):bigint = 
    let
        fun recur (divs:bigint,r:bigint,i:int):bigint = 
            let 
                val m = multiply(divs,Int.toString(i))
				fun equal(a:bigint , b : bigint) : bool = 
					let 
						fun eqrecurse(x:string,y:string,i:int): bool = 
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
		in
		let 
			fun divi(divident:bigint,divisor:bigint,i:int , quo:bigint,rem:bigint,car:int,to_return:bool):bigint = 
				if i =String.size(divident) then 
					if greaterEq(rem,divisor) then 
						let 
							val g = gusser(divisor,stripZeroes(rem))
						in 
							let 
								val nrem = subtract(rem,multiply(divisor,g))
							in
								divi(divident,divisor,i,quo^g,stripZeroes(nrem),0,to_return)
							end
						end
					else
					if to_return then 
						if car >=1 then 
							if quo = "" then 
								"0"
							else 
								stripZeroes(quo^"0") 
						else 
							if quo = "" then 
								"0"
							else
								stripZeroes(quo)
					else
						if rem = "" then "0" else  stripZeroes(rem)
				else
					if lessEq(divisor,rem) then 
						let 
							val quo_append = gusser(divisor,stripZeroes(rem))
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
		end
	fun quotient(a:bigint,b:bigint):bigint = 
		let 
			val m = stripZeroes(abs(a))
			val n = stripZeroes(abs(b))
			val ans = divider(abs(m),abs(n),true)
		in
			if ans = "" then "0"
			else if String.sub(a,0) = #"-" andalso String.sub(b,0) = #"-" then ans
			else if (String.sub(a,0) = #"-" andalso String.sub(b,0) <> #"-" )orelse (String.sub(a,0) <> #"-" andalso String.sub(b,0) = #"-" ) then negate(ans)
			else ans
		end
	fun remainder(a:bigint,b:bigint):bigint = 
		let 
			val m = stripZeroes(abs(a))
			val n = stripZeroes(abs(b))
		in
			if String.sub(a,0) = #"-" andalso String.sub(b,0) = #"-" then divider(abs(m),abs(n),false)
			else if (String.sub(a,0) = #"-" andalso String.sub(b,0) <> #"-" )orelse (String.sub(a,0) <> #"-" andalso String.sub(b,0) = #"-" ) then subtract(b,divider(abs(m),abs(n),false))
			else divider(m,n,false)
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




functor Rat (BigInt : BIGINT) :
sig
  type rational
  exception rat_error
  val make_rat : BigInt.bigint * BigInt.bigint -> rational option
  val rat : BigInt.bigint ->rational
  val reci : BigInt.bigint -> rational option
  val neg : rational -> rational 
  val inverse : rational -> rational option 
  val equal : rational * rational -> bool
  val less : rational * rational -> bool
  val add : rational * rational -> rational 
  val subtract : rational * rational -> rational
  val multiply : rational * rational -> rational
  val divide : rational * rational -> rational option
  val showRat : rational -> string  
  val showDecimal : rational -> string
  val fromDecimal : string -> rational
  val toDecimal : rational -> string
end  = 
struct
  type rational = BigInt.bigint * BigInt.bigint 
	exception rat_error 
  fun addZeroes2(str: BigInt.bigint, i: int): BigInt.bigint =
		let
			val zeroes = String.implode(List.tabulate(i, fn _ => #"0"))
		in
			BigInt.fromString(BigInt.toString(str)^zeroes)
		end
  fun normalizer(a:rational):rational = 
    let 
      val d  = BigInt.gcd(BigInt.abs(#1 a),BigInt.abs(#2 a))
    in
      let 
        val n = BigInt.quotient(BigInt.abs(#1 a),d)
        val e = BigInt.quotient(BigInt.abs(#2 a),d)
      in 
        (n,e)
      end
    end
	fun make_rat(a:BigInt.bigint, b:BigInt.bigint):rational option = 
		if BigInt.equal(b,BigInt.zero) orelse BigInt.equal(b,BigInt.empty) then raise rat_error
		else
			let 
				val d  = BigInt.gcd(BigInt.abs(a),BigInt.abs(b))
			in
				let 
					val p =BigInt.quotient(BigInt.abs(a),d)
					val q = BigInt.quotient(BigInt.abs(b),d)
				in
					if BigInt.equal(b,BigInt.zero) then raise rat_error
					else if BigInt.less(a,BigInt.zero) andalso BigInt.less(b,BigInt.zero) then SOME(p,q)
          else if BigInt.less(a,BigInt.zero) orelse BigInt.less(b,BigInt.zero) then SOME(BigInt.negate(p),q)
					else SOME(p,q)
				end
			end
		
	fun rat(a:BigInt.bigint):rational = valOf(make_rat(a,BigInt.one))

  fun reci(a:BigInt.bigint):rational option = make_rat(BigInt.one,a)

  fun neg(a:rational):rational = 
    let 
      val n = BigInt.negate(#1 a)
    in
      (n,#2 a)
    end
  
  fun inverse(a:rational):rational option = 
		if BigInt.equal((#1 a),BigInt.zero) then raise rat_error
		else 
			let 
				val n = normalizer((#2 a,#1 a))
			in 
				if BigInt.less((#1 a),BigInt.zero) andalso BigInt.less((#2 a),BigInt.zero) then SOME n
				else if BigInt.less((#1 a),BigInt.zero) orelse BigInt.less((#2 a),BigInt.zero) then 
					let 
						val nn = neg(n)
					in 
						SOME n
					end
				else SOME n
			end

  fun equal(a:rational,b:rational):bool = 
    if BigInt.equal((#1 a),(#1 b)) andalso BigInt.equal((#2 a),(#2 b)) then true else false

  fun less(a:rational,b:rational):bool = 
    let 
      val g = BigInt.gcd((#2 a),(#2 b))
    in 
      let
        val n1 = BigInt.multiply((#1 a),BigInt.quotient(g,(#2 a)))
        val n2 = BigInt.multiply((#1 b),BigInt.quotient(g,(#2 b)))
      in
        if BigInt.less(n1,n2) then true
        else false
      end
    end
  
  fun add(a:rational,b:rational):rational = 
    let
      val m = BigInt.lcm(BigInt.stripZeroes(BigInt.abs((#2 a))),BigInt.stripZeroes(BigInt.abs((#2 b))))
			val p1 = (#1 a)
			val p2 =  (#2 a)
    in
        let
          val a1 = BigInt.quotient(m,BigInt.stripZeroes(BigInt.abs((#2 a))))
          val b1 = BigInt.quotient(m,BigInt.stripZeroes(BigInt.abs((#2 b))))
        in 
					let
						val n = normalizer((BigInt.add(BigInt.multiply((#1 a),a1),BigInt.multiply((#1 b),b1)),m))
					in
						if BigInt.less(p1,BigInt.zero) andalso  BigInt.less(p2,BigInt.zero)  then n
						else if BigInt.less(p1,BigInt.zero) orelse  BigInt.less(p2,BigInt.zero)  then neg(n)
						else n
					end 
						
        end
    end
  fun subtract(a:rational,b:rational):rational = 
    let
      val m = BigInt.lcm((#2 a),(#2 b))
			val p1 = (#1 a)
			val p2 =  (#2 a)
    in
        let
          val a1 = BigInt.quotient(m,(#2 a))
          val b1 = BigInt.quotient(m,(#2 b))
        in  
          let
						val n = BigInt.subtract(BigInt.multiply((#1 a),a1),BigInt.multiply((#1 b),b1))
					in
						if BigInt.less(n,BigInt.zero) then neg(normalizer(n,m))
						else (n,m)
					end 
        end
    end
  fun multiply(a:rational,b:rational):rational = 
    let 
      val n = BigInt.multiply((#1 a),(#1 b))
      val d = BigInt.multiply((#2 a),(#2 b))
    in
			let 

      	val n = normalizer((n,d))
			in
				if BigInt.less((#1 a),BigInt.zero) orelse BigInt.less((#1 b),BigInt.zero) then neg(n)
				else n
			end
    end
  fun divide(a:rational,b:rational):rational option = 
    let 
      val n = BigInt.multiply((#1 a),(#2 b))
      val d = BigInt.multiply((#2 a),(#1 b))
    in 
    	let 
      		val n = normalizer((n,d))
		in
			if BigInt.less((#1 a),BigInt.zero) orelse BigInt.less((#1 b),BigInt.zero) then 
				let 
					val k = neg(n)
				in 
					SOME k
				end
			else SOME n
		end
    end

  fun showRat(a:rational):string = 
    let 
      val norm = normalizer(a)
    in
			if BigInt.less((#1 a),BigInt.zero) then "("^"-"^BigInt.toString((#1 norm))^","^BigInt.toString((#2 norm))^")"
      else "("^BigInt.toString((#1 norm))^","^BigInt.toString((#2 norm))^")"
    end

  fun chmod(a:string):string = 
    let
      fun bd(x:string,i:int,ans:string):string = 
          if String.sub(x,i) = #"." then ans
          else 
            bd(x,i+1,ans^Char.toString(String.sub(x,i)))
      fun y(x:string,i:int,found:bool,ans:string):string = 
          if String.sub(x,i) = #"(" then ans
          else if String.sub(x,i) = #"." then y(x,i+1,true,ans)
          else 
            if found then y(x,i+1,found,ans^Char.toString(String.sub(x,i)))
            else y(x,i+1,found,ans)
      fun z(x:string,i:int,found:bool,ans:string):string = 
          if String.sub(x,i) = #")" then ans
          else if String.sub(x,i) = #"(" then z(x,i+1,true,ans)
          else 
            if found then z(x,i+1,found,ans^Char.toString(String.sub(x,i)))
            else z(x,i+1,found,ans)
      fun remove_suffix(a : string, b : string) : string =
        let
            val a_len = String.size(a)
            val b_len = String.size(b)
        in
            if a_len >= b_len andalso String.substring(a, a_len - b_len,b_len) = b
            then remove_suffix(substring(a, 0, a_len - b_len),b)
            else a
        end
    in
      let 
        val l = bd(a,0,"")
        val m = y(a,0,false,"")
        val n = z(a,0,false,"")
      in 
        let 
          val nnr = remove_suffix(m,n)
        in 
          l^"."^nnr^"("^n^")"
        end
      end
    end
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
            else if i = String.size(x)-1 then raise rat_error
            else if i_th = #"(" andalso i = String.size(x) then raise rat_error
            else if  i_th = #"(" andalso String.sub(x,i+1) = #")" then raise rat_error
            else if i_th = #"(" then correct(x,i+1,true,bracclose)
            else if i_th = #")" then raise rat_error
            else if search(string_list,i_th) then correct(x,i+1,bracopen,bracclose)
            else raise rat_error
            end
    in
        if String.sub(a,0) = #"+" orelse  String.sub(a,0) = #"-" then correct(String.substring(a,1,String.size(a)-1),0,false,false)
        else raise rat_error
    end
  fun fromDecimal(a:string):rational = 
		if DFA(a) then 
			let 
				fun y_len(x:string,i:int,found:bool,ans:int):int= 
					if  String.sub(x,i) = #"(" then ans
					else if String.sub(x,i) = #"." then y_len(x,i+1,true,ans) 
					else if found then y_len(x,i+1,true,ans+1)
					else y_len(x,i+1,false,ans)
				fun z_len(x:string,i:int,found:bool,ans:int):int= 
					if  String.sub(x,i) = #")" then ans
					else if String.sub(x,i) = #"("  then z_len(x,i+1,true,ans) 
					else if found then z_len(x,i+1,true,ans+1)
					else z_len(x,i+1,false,ans)
				fun xy(x:string,i:int,ans:string):BigInt.bigint = 
						if String.sub(x,i) = #"(" then BigInt.fromString(ans)
						else if String.sub(x,i) = #"."  then xy(x,i+1,ans)
						else xy(x,i+1,ans^Char.toString(String.sub(x,i)))
				fun xyz(x:string,i:int,ans:string):BigInt.bigint = 
						if String.sub(x,i) = #")" then BigInt.fromString(ans)
						else if String.sub(x,i) = #"." orelse String.sub(x,i) = #"("  then xyz(x,i+1,ans)
						else xyz(x,i+1,ans^Char.toString(String.sub(x,i)))
				val a = chmod(a)
			in  
				let
					val k = y_len(a,0,false,0)
					val b = z_len(a,0,false,0)
					val c = BigInt.abs(xy(a,0,""))
					val d = BigInt.abs(xyz(a,0,""))
				in
						let 
							val bigz = addZeroes2(BigInt.one,k+b)
							val smlz = addZeroes2(BigInt.one,k)
						in
									if String.sub(a,0) = #"-" then 
										let 
										val n = normalizer((BigInt.subtract(d,c),BigInt.stripZeroes(BigInt.subtract(bigz,smlz))))
										in 
											if b=0 then raise rat_error
											else 
												(BigInt.negate(#1 n),(#2 n))
										end
									else
										if b=0 orelse String.sub(a,0) <> #"+" then raise rat_error
										else
										normalizer((BigInt.subtract(d,c),BigInt.stripZeroes(BigInt.subtract(bigz,smlz))))	
						end
				end
			end
			else raise rat_error
  fun showDecimal(a:rational):string = 
    let
      val numerator = (#1 a)
      val denominator = (#2 a)
			fun equal(a:BigInt.bigint , b : BigInt.bigint) : bool = 
				let 
					fun eqrecurse(x:string,y:string,i:int): bool = 
						if i = String.size(x) then true
						else if String.sub(x,i) <> String.sub(y,i) then false
						else eqrecurse(x,y,i+1)
				in
					let
						val m = BigInt.toString(BigInt.stripZeroes(BigInt.abs(a)))
							val n = BigInt.toString(BigInt.stripZeroes(BigInt.abs(b)))
						in
					if String.size(m) <> String.size(n) then false
					else eqrecurse(m,n,0)
					end
				end
				fun greater(a:BigInt.bigint,b : BigInt.bigint) :bool = 
					let
						fun grtrecurse(x:string,y:string,i:int): bool = 
							if i = String.size(x) then true
											else if String.sub(x,i) > String.sub(y,i) then true
							else if String.sub(x,i) < String.sub(y,i) then false
							else grtrecurse(x,y,i+1)
					in
						let 
							val m = BigInt.toString(BigInt.stripZeroes(BigInt.abs(a)))
							val n = BigInt.toString(BigInt.stripZeroes(BigInt.abs(b)))
						in
							if String.size(n) < String.size(m) then true
							else if String.size(m) < String.size(n) then false
							else grtrecurse(m,n,0)
						end
					end
    in 
      let 
        val beforedec = BigInt.quotient(BigInt.stripZeroes(BigInt.abs(numerator)),BigInt.stripZeroes(BigInt.abs(denominator)))
        val remainderleft = BigInt.remainder(BigInt.stripZeroes(BigInt.abs(numerator)),BigInt.stripZeroes(BigInt.abs(denominator)))
        fun gusser(divisor:BigInt.bigint,rem:BigInt.bigint):BigInt.bigint = 
          let
              fun recur (divs:BigInt.bigint,r:BigInt.bigint,i:int):BigInt.bigint = 
                  let 
                      val m = BigInt.multiply(divs,BigInt.fromString(Int.toString(i)))
                  in 
                      if equal(m,rem) then BigInt.fromString(Int.toString(i))
                      else if greater(m,rem) then BigInt.fromString(Int.toString(i-1))
                      else recur(divs,r,i+1)
                  end
          in
              recur(divisor,rem,0)
          end
        fun find_position(lst: string list, s: string) : int =
          let
              fun helper(lst', pos) =
                  case lst' of
                      [] => ~1
                    | x::xs => if x = s then pos else helper(xs, pos+1)
          in
              helper(lst, 0)
          end
      in
        let 
          fun answer(n:BigInt.bigint, d:BigInt.bigint, rem_list: string list, ans:string):string = 
            if equal(n,BigInt.zero) then BigInt.toString(beforedec)^"."^"(0)"
			else if BigInt.less(n,d) then answer(addZeroes2(n,1),d,rem_list@["0"],ans^"0") 
            else  
              let 
                val guess = gusser(d,n)
              in
                let 
                  val nrem = BigInt.subtract(n,BigInt.multiply(guess,d))
                in
                  let val sznrem =  BigInt.stripZeroes(nrem)
                      val blkstr = BigInt.empty
                      val pos = find_position(rem_list,BigInt.toString(BigInt.stripZeroes(nrem)))
                  in
                  if equal(sznrem,blkstr) then BigInt.toString(beforedec)^"."^ans^BigInt.toString(guess)^"(0)"
                  else if pos>=0  then BigInt.toString(beforedec)^"."^String.substring(ans^BigInt.toString(guess),0,pos)^"("^String.substring(ans^BigInt.toString(guess),pos,(String.size(ans)+1-pos))^")"
                  else answer(addZeroes2(nrem,1),d,rem_list @ [BigInt.toString(sznrem)],ans^BigInt.toString(guess))
                  end
                end
              end
        in 
					let 
					val an = answer(addZeroes2(remainderleft,1),BigInt.stripZeroes(BigInt.abs(denominator)),[BigInt.toString(remainderleft)],"")
					in
						if BigInt.less(numerator,BigInt.zero) andalso BigInt.less(denominator,BigInt.zero) then "+"^an
						else if BigInt.less(numerator,BigInt.zero) orelse BigInt.less(denominator,BigInt.zero) then "-"^an
						else "+"^an
					end
        end 
      end
    end
  fun toDecimal(a:rational):string = showDecimal(a)
end 

structure Rational = Rat(BigInt)
