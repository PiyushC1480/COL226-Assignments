fun part_2 (ls,x_in) = 
  let
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
        if (((20*x)+guesso) *guesso <= one ) andalso ((((20*x)+(guesso+1)) *(guesso+1 ))>= one ) then guesso else recurse ((guesso+1),one,x)
    in 
      let 
        val conlis = concatene(ls_in)
      in
          let 
            val SOME fir  = Int.fromString(List.hd(conlis))
          in
            let 
              val gout = recurse(guess,fir,x)
            in
              List.update(conlis,0,Int.toString(fir-(((20*x)+gout))*gout))
            end
          end
      end
    end
  in
    helper1 (ls,x_in)
  end

  (* fun helper2 (lis ,gssout,x)= 
    let 
      val SOME fir  = Int.fromString(List.hd(lis))
    in
      List.update(lis,0,Int.toString(fir-(((20*x)+gssout))*gssout))
    end




  in 
    if List.len(ls) =1 then List.nth(ls,0) 
    else  *)
    

