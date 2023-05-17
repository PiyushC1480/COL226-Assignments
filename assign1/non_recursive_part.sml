

fun complete_step_one str = 
  let
    val len = String.size str
    fun divideHelper (s, acc) =
      if String.size s = 0 
      then 
        acc
      else 
        divideHelper (String.substring (s, 2, String.size s - 2), (String.substring (s, 0, 2) :: acc))
        
    fun step1(lis) =
    let 
      val SOME n = Int.fromString(List.hd lis)
      fun nearestSqrt (ls) =
        let
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
    in
      let
        val sq = nearestSqrt(lis)
      in 
        List.update(lis,0,Int.toString(n-(sq*sq)))
      end
    end
    


  in
        if len mod 2 = 1 then 
        let 
          val primls = (String.substring(str,0,1)::List.rev (divideHelper (String.substring(str,1,len-1),[])))
        in 
          step1 primls
        end
        else 
          let val primls = List.rev (divideHelper (str, []))
        in 
          step1 primls
        end
  end
