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


