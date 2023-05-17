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


    
    fun helper1 (ls_in, x)= 
      let
        val guess = 0
        fun concatene ls_in =
          let 
            val pos1 = List.nth(ls_in,0)
            val pos2 = List.nth(ls_in,1)
          in
            let 
              val finlis = List.tl(ls_in)
            in 
              List.update(finlis,0,pos1^pos2)
            end
          end
          fun recurse (guesso,one,x) = 
            if (((20*x)+guesso) *guesso <= one ) andalso ((((20*x)+(guesso+1)) *(guesso+1 ))> one ) then guesso else recurse ((guesso+1),one,x)      
          fun updation(lis,go,xin)=
            let 
              val fir  = valOf(Int.fromString(List.hd(lis)))
            in 
              List.update(lis,0,Int.toString(fir-(((20*xin)+go))*go)) 
            end
            
          fun xupdate(x,go) = x*10 + go
      in 
        let 
          val conlis = concatene(ls_in)
        in
          let 
            val fir  = valOf(Int.fromString(List.hd(conlis)))
          in
            let 
              val gout = recurse(guess,fir,x)
            in
              let 
                val newlis = updation(conlis,gout,x)
              in
                let 
                  val x = xupdate(x,gout)
                in
                  if List.length(newlis) = 1 then (Int.toString(x),List.hd(newlis)) else helper1(newlis,x)
                end
              end
            end
          end
        end
      end

  in
    if value = "0" then ("0","0") else
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
                if ((valOf(Int.fromString(value)))<100) then (Int.toString(after1),List.hd(after2)) else helper1(after2,after1)
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
                if ((valOf(Int.fromString(value)))<100) then (Int.toString(after1),List.hd(after2)) else helper1(after2,after1)
            end
            end
        end
    end
