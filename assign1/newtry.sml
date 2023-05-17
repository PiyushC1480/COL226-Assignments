fun multiply(x:string ,y:string) :string list = 
  let 
    val int1 = List.map(fn c=> String.str c)(String.explode(x))
    val int2 = valOf(Int.fromString(y))
    val size1 = String.size x
    val size2 = String.size y
    fun helper(lis,str,carry,pos,ans) = 
      if pos = 0 
      then 
        List.update(ans,pos, Int.toString(valOf(Int.fromString(List.nth(lis,0))) * valOf(Int.fromString(str)) +carry))
      else 
        let 
          val temp = Int.toString(valOf(Int.fromString(List.nth(lis,pos))) * valOf(Int.fromString(str)) + carry)
        in
          if String.size(temp)=1 then 
            let  
              val newans =List.update(ans,pos,temp)
              val newpos = pos-1
              val newcarry = 0
            in 
              helper(lis,str,newcarry,newpos,newans)
            end
          else 
            let 
              val split = List.map(fn c=>String.str c)(String.explode(temp))
            in
              let 
                val updator = List.nth(split,1)
                val carryn = valOf(Int.fromString(List.nth(split,0)))
                val newpos = pos-1
              in
                let
                    val newansn = List.update(ans,pos,updator)
                  in
                    helper(lis,str,carryn,newpos,newansn)
                end
              end
            end
          end
  in
    let 
      val ans= List.tabulate(size1+size2-1,fn i=>"0")
      val carry= 0 
      val pos = size1 -1
    in  
      helper(int1,y,carry,pos,ans)
    end
  end

fun concaten(lis:string list) : string = String.concat lis

fun mulandconcat(x:string ,y :string):string = 
    let 
        val mul = multiply(x,y)
    in 
        concaten(mul)
    end

fun ninecomplement(b:string) =
    let
        val strli = List.map (fn c => String.str c) (String.explode b)
        val baselist = List.tabulate(String.size b, fn i => "9")
        fun subt (l1:string list , l2 : string list, pos:int) = 
            if pos < 0 then l1 
            else
                let 
                    val tem = Int.toString(valOf(Int.fromString(List.nth(l1,pos))) - valOf(Int.fromString(List.nth(l2,pos))))
                in
                    let 
                        val n = List.update(l1,pos,tem)
                    in
                        subt(n,l2,pos-1)
                    end
                end
    in
    
        subt(baselist,strli,String.size b-1)
    end
fun concaten(lis:string list) : string = String.concat lis

fun tencomplement(b:string) = 
    let 
        val t = ninecomplement(b)
    in 
        let 
            val up = Int.toString(valOf(Int.fromString(List.nth(t,(List.length(t)-1)))) +1)
        in 
            List.update(t,List.length(t)-1,up)
        end
    end


fun carryadd(lis1:string list,posn : int,car :int):string list = 
  if car = 0 then lis1
  else if (car = 1 andalso posn < 0) then "1"::lis1
  else
    let
      val tempo = List.map(fn c => String.str c) (String.explode(Int.toString(valOf(Int.fromString(List.nth(lis1,posn))) + 1 )))
    in 
      if List.length(tempo) >1 then 
        let 
          val updat = List.nth(tempo,0)
          val new = posn - 1
        in 
          let 
            val newl = List.update(lis1,posn,updat)
          in 
            carryadd(newl,new,1)
          end
        end
      else
        let 
          val updat = List.nth(tempo,0)
          val new = posn - 1
        in 
          let 
            val newl  = List.update(lis1,posn,updat)
          in
            carryadd(newl,new,0)
          end
        end
    end

      
fun addition(a:string , b: string) = 
  let 
    fun addr(lis1:string list ,lis2 : string list,carry :string,pos : int) :string list = 
      if (pos < 0) then
        if carry = "0" then lis1 else carryadd(lis1,(List.length(lis1) - List.length(lis2) - 1),1)
      else
      let 
        val tempp = (valOf(Int.fromString(List.nth(lis1,(List.length(lis1) - List.length(lis2) + pos)))) + valOf(Int.fromString(List.nth(lis2,pos))) + valOf(Int.fromString(carry)))
      in
        if tempp > 9 then  
          let 
            val update = Int.toString(tempp - 10)
            val newp = (pos -1)
          in
            let 
            val newlis = List.update(lis1,(List.length(lis1) - List.length(lis2) + pos), update)
            in 
              addr(newlis,lis2,"1",newp)
            end
          end
        else
          let
            val update =  Int.toString(tempp)
            val newp = (pos -1)
          in
            let 
              val newlis = List.update(lis1,List.length(lis1) - List.length(lis2) + pos, update)
            in 
              addr(newlis,lis2,"0",newp)
            end
          end
      end
  in
    let 
      val int1 = List.map(fn c =>String.str c) (String.explode a)
      val int2 =  List.map(fn c =>String.str c) (String.explode b)
    in
      addr(int1,int2,"0",(List.length(int2)-1))
    end

  end   
    
fun addnandconcat(x:string,y:string) :string =
let
    val a = addition(x,y) 
in
    concaten(a)
end

fun subtractor(x:string,y:string) = 
    let
        val padd= concaten(List.tabulate((String.size(x) - String.size(y)),fn i=>"9"))
    in
        let
            val tc = (padd^concaten(tencomplement(y)))
        in 
            let 
                val lisy = addition(x,tc)
            in 
                String.concat(List.tl(lisy))
            end
        end
    end


fun comparator(a:string, b:string) = 
  if String.size(a) > String.size(b) then 1
  else if String.size(a) < String.size(b) then 0
  else 
    let 
      fun rtl(a_list:string list ,b_list:string list,pos : int)  = 
        if pos = (List.length(a_list)-1) then 0 else
        if List.nth(a_list,pos) >List.nth(b_list,pos) then 1 
        else if List.nth(a_list,pos) >List.nth(b_list,pos) then 0
        else rtl(a_list ,b_list,pos +1)
    in
      let 
        val a_list = List.map(fn c=>String.str c)(String.explode(a))
        val b_list = List.map(fn c=>String.str c)(String.explode(b))
      in 
        rtl(a_list,b_list,0)
      end
    end





fun isqrtld value = 
  let 
    val len = String.size value
    fun divideHelper (s, acc) =
      if String.size s = 0 
      then 
        acc
      else 
        divideHelper (String.substring (s, 2, String.size s - 2), (String.substring (s, 0, 2) :: acc))
    
    fun nearestSqrt (ls) =
      let
        val n = valOf(Int.fromString(List.hd ls))
        val guess = 1
        fun tryGuess (m:int ,n:int) :int= 
          let 
            val temp = m*m
            val temp2 = (m+1)*(m+1)
          in 
            if (n>= temp andalso temp2>n) then m else tryGuess ((m+1),n)
          end
      in 
        tryGuess (guess,n)
      end
    

    fun step1(lis,sq) =
      let 
        val n = valOf(Int.fromString(List.hd lis)) 
      in
          List.update(lis,0,Int.toString(n-(sq*sq)))
      end

    
    fun helper1 (ls_in, x_k)= 
      let
        val guess = 1
        fun concatene ls_in =
          let 
            val pos1 = List.nth(ls_in,0)
            val pos2 = List.nth(ls_in,1)
          in
            let 
              val finlis = List.tl(ls_in)
            in 
              if pos1 = "0" then finlis else List.update(finlis,0,pos1^pos2)
            end
          end
          fun gusser (guesso,one,x_in) = 
            let
                val t1 = (mulandconcat(x_in,"2"))^"0"
                val t2 = (mulandconcat(x_in,"2"))^"0"
            in 
                let 
                  val t3 = addnandconcat(t1,Int.toString(guesso))
                  val t4 = addnandconcat(t2,Int.toString(guesso+1))

                in 
                  let 
                    val t5 = mulandconcat(t3,Int.toString(guesso))
                    val t6 = mulandconcat(t4,Int.toString(guesso+1))
                  in
                    let 
                      val c1 = comparator(t5,one) 
                      val c2 = comparator(t6,one)
                    in
                        if (c1= 0  andalso c2 = 1) then (Int.toString(guesso)) else gusser((guesso+1),one,x_in)
                    end
                      
                  end  
                end
              
            end
          fun updation(lis,go,xin)=
            let 
              val fir  = List.hd(lis)
            in 
              let 
                val expr3 = mulandconcat(addnandconcat((mulandconcat(xin,"2"))^"0",go),go)
                (* val expr3 = concatene(multiply(addition(concatene(multiply((concatene(multiply(x,"2")) ^ "0"),xin)),go),go)) *)
              in 
                List.update(lis,0,subtractor(fir,expr3)) 
              end
            end
            
          fun xupdate(xs,go) = (xs^go)
          (* concaten(addition(x^"0"), go) *)
      in 
        let 
          val conlis = concatene(ls_in)
        in
          let 
            val fir  = List.hd(conlis)
          in
            let 
              val gout = gusser(guess,fir,x_k)
            in
              let 
                val newlis = updation(conlis,gout,x_k)
                val x = xupdate(x_k,gout)
              in  
                  if List.length(newlis) = 1 then (x,List.hd(newlis)) else helper1(newlis,x)
              end
            end
          end
        end
      end

  in
    if len mod 2 = 1 then 
      let 
        val primls = (String.substring(value,0,1)::List.rev (divideHelper (String.substring(value,1,len-1),[])))
      in 
        let
          val after1 = nearestSqrt primls
        in 
          let
            val after2 = step1(primls,after1)
          in  
            if ((valOf(Int.fromString(value)))<100) then (Int.toString(after1),List.hd(after2)) else helper1(after2,Int.toString(after1))
          end
        end
      end

    else 
      let 
        val primls = List.rev (divideHelper (value, []))
      in 
        let
          val after1 = nearestSqrt primls
        in 
          let
            val after2 = step1(primls,after1)
            
          in  
            if ((valOf(Int.fromString(value)))<100) then (Int.toString(after1),List.hd(after2)) else helper1(after2,Int.toString(after1))
          end
        end
      end
  end
