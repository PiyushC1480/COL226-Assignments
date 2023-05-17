
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
    

            
