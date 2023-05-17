use "bigint.sml";

signature RATIONAL =
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
end
(* 
 showRat displays a rational number in fractional-normal form.
• showDecimal displays the rational number in decimal-normal form.
• bigint is an integer of arbitrary size (not limited by the implementation) and could have a magnitude much larger than the maxInt or much smaller than the minInt defined by the SML-NJ structure Int.
• fromDecimal may take a possibly non-standard decimal form and create a rational from it.
• toDecimal converts a rational number into its standard decimal representation.
• rat inputs an integer i and converts it into the rational 1i .
• inverse finds the reciprocal of a non-zero rational.
• reci finds the reciprocal of a non-zero integer.
• make rat takes any two integers and creates a rational number fractional-normal form. Hence make rat
(4, ~10) = (~2,5).Ra
• neg takes any rational number and negates it. *)

functor Rat (BigInt : BIGINT) =
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
        val n = BigInt.quotient((#1 a),d)
        val e = BigInt.quotient((#2 a),d)
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
					val (p,q) = (BigInt.quotient(a,d),BigInt.quotient(b,d))
				in
          SOME(p,q)
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
  
  fun inverse(a:rational):rational option = if BigInt.equal((#1 a),BigInt.zero) then raise rat_error else SOME (#2 a,#1 a)

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
      val m = BigInt.lcm((#2 a),(#2 b))
    in
        let
          val a1 = BigInt.quotient(m,(#2 a))
          val b1 = BigInt.quotient(m,(#2 b))
        in  
          normalizer((BigInt.add(BigInt.multiply((#1 a),a1),BigInt.multiply((#1 b),b1)),m))
        end
    end
  fun subtract(a:rational,b:rational):rational = 
    let
      val m = BigInt.lcm((#2 a),(#2 b))
    in
        let
          val a1 = BigInt.quotient(m,(#2 a))
          val b1 = BigInt.quotient(m,(#2 b))
        in  
          (BigInt.subtract(BigInt.multiply((#1 a),a1),BigInt.multiply((#1 b),b1)),m)
        end
    end
  fun multiply(a:rational,b:rational):rational = 
    let 
      val n = BigInt.multiply((#1 a),(#1 b))
      val d = BigInt.multiply((#2 a),(#2 b))
    in
      normalizer((n,d))
    end
  fun divider(a:rational,b:rational):rational = 
    let 
      val n = BigInt.multiply((#1 a),(#2 b))
      val d = BigInt.multiply((#2 a),(#1 b))
    in 
      normalizer((n,d))
    end

  fun showRat(a:rational):string = 
    let 
      val norm = normalizer(a)
    in
      "("^BigInt.toString((#1 norm))^","^BigInt.toString((#2 norm))^")"
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
  
  fun fromDecimal(a:string):rational = 
    let 
      fun y_len(x:string,i:int,found:bool,ans:int):int= 
        if  String.sub(x,i) = #"(" then ans
        else if String.sub(x,i) = #"." then y_len(x,i+1,true,ans) 
        else if found then y_len(x,i+1,true,ans+1)
        else y_len(x,i+1,false,ans)
      fun z_len(x:string,i:int,found:bool,ans:int):int= 
        if  String.sub(x,i) = #")" then ans
        else if String.sub(x,i) = #"(" then z_len(x,i+1,true,ans) 
        else if found then z_len(x,i+1,true,ans+1)
        else z_len(x,i+1,false,ans)
      fun xy(x:string,i:int,ans:string):BigInt.bigint = 
          if String.sub(x,i) = #"(" then BigInt.fromString(ans)
          else if String.sub(x,i) = #"." then xy(x,i+1,ans)
          else xy(x,i+1,ans^Char.toString(String.sub(x,i)))
      fun xyz(x:string,i:int,ans:string):BigInt.bigint = 
          if String.sub(x,i) = #")" then BigInt.fromString(ans)
          else if String.sub(x,i) = #"." orelse String.sub(x,i) = #"(" then xyz(x,i+1,ans)
          else xyz(x,i+1,ans^Char.toString(String.sub(x,i)))
      val a = chmod(a)
    in  
      let
        val k = y_len(a,0,false,0)
        val b = z_len(a,0,false,0)
        val c = xy(a,0,"")
		    val d = xyz(a,0,"")
      in
		      let 
            val bigz = addZeroes2(BigInt.one,k+b)
            val smlz = addZeroes2(BigInt.one,k)
		      in
			      normalizer((BigInt.subtract(d,c),BigInt.stripZeroes(BigInt.subtract(bigz,smlz))))
		      end
      end
	end
  fun showDecimal(a:rational):string = 
    let
      val numerator = (#1 a)
      val denominator = (#2 a)
    in 
      let 
        val beforedec = BigInt.quotient(numerator,denominator)
        val remainderleft = BigInt.remainder(numerator,denominator)
        fun gusser(divisor:BigInt.bigint,rem:BigInt.bigint):BigInt.bigint = 
          let
              fun recur (divs:BigInt.bigint,r:BigInt.bigint,i:int):BigInt.bigint = 
                  let 
                      val m = BigInt.multiply(divs,BigInt.fromString(Int.toString(i)))
                  in 
                      if BigInt.equal(m,rem) then BigInt.fromString(Int.toString(i))
                      else if BigInt.greater(m,rem) then BigInt.fromString(Int.toString(i-1))
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
            if BigInt.equal(n,BigInt.zero) then BigInt.toString(beforedec)^"."^"(0)"
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
                  if BigInt.equal(sznrem,blkstr) then BigInt.toString(beforedec)^"."^ans^BigInt.toString(guess)^"(0)"
                  else if pos>=0  then BigInt.toString(beforedec)^"."^String.substring(ans^BigInt.toString(guess),0,pos)^"("^String.substring(ans^BigInt.toString(guess),pos,(String.size(ans)+1-pos))^")"
                  else answer(addZeroes2(nrem,1),d,rem_list @ [BigInt.toString(sznrem)],ans^BigInt.toString(guess))
                  end
                end
              end
        in 
          answer(addZeroes2(remainderleft,1),denominator,[BigInt.toString(remainderleft)],"")
        end 
      end
    end
  fun toDecimal(a:rational):string = showDecimal(a)
end 

structure Rational = Rat(BigInt)
