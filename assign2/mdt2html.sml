fun bhandler(s:string,i :int):string = 
    let 
        fun re(ostr:string,i:int):string =
                if i = 0 then ostr
                else re("<blockquote>"^ostr^"</blockquote>",i-1)
    in
        re(s,i)
    end
fun final_read (str:string,isTable:int,isList:int,iscoblk:int)  =
(*can process string of any length containing bold,italic,underline,heading*)
    let
        fun recurse(str:string,isHeading:int,heading:int,bold:int,italic:int,uline:int,isLink:int , link_txt :string, isBlock :int, alphaencr:int,posn:int,out:string):string = 
            if (posn >= String.size(str)-1 andalso bold = 1  ) then "<font color = red>error encountered.error type  wrong bold in line::: " ^ str^ "\n"
            else if (posn >= String.size(str)-1 andalso italic = 1  ) then "<font color = red>error encountered.error type  wrong italic in line::: " ^ str^ "\n"
            else if (posn >= String.size(str)-1 andalso uline = 1  ) then "<font color = red>error encountered.error type  wrong uline in line::: " ^ str^ "\n"

            else if (posn >= String.size(str)-1 andalso isHeading =2 )then out^"</h"^Int.toString(heading)^">" 
            else if (posn >= String.size(str)-1 andalso isBlock =1 ) then bhandler(out,isBlock)
            (* else if (posn >=String.size(str)-1 andalso isList = 1) then out^"</li>" *)
            else if posn >= String.size(str)-1 then out
            (* else if (String.sub(str,posn) = #"\\ ") then recurse(str,isHeading,heading,bold,italic,uline,isLink,link_txt,isBlock,posn+1,out) *)
            else if (String.sub(str,posn) = #">" andalso isTable = 0 andalso alphaencr = 0 andalso iscoblk = 0) then recurse(str,isHeading,heading,bold,italic,uline,isLink,link_txt,isBlock+1,0,posn+1,out)
            
            else if (String.sub(str,posn) = #"<" andalso  iscoblk = 0 andalso String.substring(str,posn+1,4) = "http" andalso isTable = 0 ) then recurse(str,isHeading,heading,bold,italic,uline,1,link_txt,isBlock,1,posn+1,out^"<a href =")
            else if (String.sub(str,posn) = #">" andalso  iscoblk = 0 andalso isLink  = 1 andalso isTable = 0 ) then recurse(str,isHeading,heading,bold,italic,uline,0," ",isBlock,1,posn+1,out^link_txt^">"^link_txt^"</a>")
            else if (String.sub(str,posn) = #"#") then recurse(str,1,heading+1,bold,italic,uline,isLink,link_txt,isBlock,1,posn+1,out)
            
            else if (String.sub(str,posn) = #"[" ) then recurse(str,isHeading,heading,bold,italic,uline,1,link_txt,isBlock,1,posn+1,out)

            else if (String.sub(str,posn) = #"]") then
                if posn < String.size(str)-1 then
                    if String.sub(str,posn+1) = #"("  then 
                        recurse(str,isHeading,heading,bold,italic,uline,2,link_txt,isBlock,1,posn+2,out^"< a href = ")
                    else    
                        recurse(str,isHeading,heading,bold,italic,uline,0,"",isBlock,1,posn+1,out^"["^link_txt^"]")
                else
                    recurse(str,isHeading,heading,bold,italic,uline,0,"",isBlock,1,posn+1,out^"["^link_txt^"]")
                

            else if (String.sub(str,posn) = #")" andalso isLink  = 2  ) then recurse(str,isHeading,heading,bold,italic,uline,0,link_txt,isBlock,1,posn+1,out^">"^link_txt^"</a>")

            (* else if (String.sub(str,posn) = #")") then recurse(str,isHeading,heading,bold,italic,uline,0,"",posn+1,out^"["^link_txt^"]") *)

            else if (String.sub(str,posn) = #"*" ) then
                if posn < String.size(str)-1 then
                    if (String.sub(str,posn+1) = #"*" andalso bold =0) then recurse(str,isHeading,heading,1,italic,uline,isLink,link_txt,isBlock,1,posn+2,out^"<b>")

                    else if (String.sub(str,posn+1) = #"*"  andalso bold =1) then recurse(str,isHeading,heading,0,italic,uline,isLink,link_txt,isBlock,1,posn+2,out^"</b>")
                    else if (italic = 0 ) then recurse(str,isHeading,heading,bold,1,uline,isLink,link_txt,isBlock,1,posn+1,out^"<i>")
                        (* "limit exceeded" *)
                    else if (italic = 1) then recurse(str,isHeading,heading,bold,0,uline,isLink,link_txt,isBlock,1,posn+1,out^"</i>")
                    else "limit"
                else 
                    if italic = 1 then recurse(str,isHeading,heading,bold,0,uline,isLink,link_txt,isBlock,1,posn+1,out^"</i>")
                    else "limit2"

            else if (String.sub(str,posn) = #" " andalso uline =1) then recurse(str,isHeading,heading,bold,italic,0,isLink,link_txt,isBlock,1,posn+1,out^" </u> ")

            else if (String.sub(str,posn) = #"_" andalso uline = 0 ) then recurse(str,isHeading,heading,bold,italic,1,isLink,link_txt,isBlock,1,posn+1,out^"<u> ")

            else if (String.sub(str,posn) = #"_" andalso uline = 1) then 
                if posn = String.size(str)-1 then recurse(str,isHeading,heading,bold,italic,uline,isLink,link_txt,isBlock,1,posn+1,out^"</u>")
                else recurse(str,isHeading,heading,bold,italic,uline,isLink,link_txt,isBlock,1,posn+1,out^" ")

            else if (String.sub(str,posn) = #" " andalso isLink = 1) then recurse(str,isHeading,heading,bold,italic,uline,1,link_txt^Char.toString(String.sub(str,posn)),isBlock,1,posn+1,out)

            else if (String.sub(str,posn) = #" " andalso isHeading = 1) then recurse(str,2,heading,bold,italic,uline,isLink,link_txt,isBlock,1,posn+1,out^"<h"^Int.toString(heading)^">") 

            else 
                if isLink =1 then recurse(str,isHeading,heading,bold,italic,uline,isLink,link_txt^Char.toString(String.sub(str,posn)),isBlock,1,posn+1,out)
                else 
                recurse(str,isHeading,heading,bold,italic,uline,isLink,link_txt,isBlock,1,posn+1,out^Char.toString(String.sub(str,posn)))
    in
        recurse(str,0,0,0,0,0,0,"",0,0,0,"")
    end


fun appendFile (filename, content) =
    let val fd = TextIO.openAppend filename
        val _ = TextIO.output (fd, "\n "^content^" ") handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end

fun htmlmaker filename= 
    let val fd = TextIO.openOut filename
        val _ = TextIO.output(fd,"<!DOCTYPE html>\n<html>\n  <head>\n    <title>My HTML Page</title>\n  </head>\n  <body>\n ")
        (* val _=TextIO.output(fd,"<!DOCTYPE html>\n<html>\n  <head>\n    <title>My HTML Page</title>\n  </head>\n  <body>\n"^cont^"  </body>\n</html>\n") *)
        val _ = TextIO.closeOut fd
    in () end

fun htmlcloser fileName = 
    let val fd = TextIO.openAppend fileName
        val _ = TextIO.output (fd, "\n</body>\n</html>\n") handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end

fun table_making(str:string,isList:int) = 
(*to be called after table_starter has been initiated
will run untill ">>" this has not been encontered*)
    let 
        val raw_table_line = String.translate (fn #"|" => " </td> <td> " | c => Char.toString(c)) str
    in 
        let 
            val up = final_read(String.substring(raw_table_line,0,String.size(raw_table_line)-1),1,isList,0)
        in
            "<tr> <td> "^up^" </td> </tr>"
        end
    end

fun checkString stri =
    let
        val someInt = Int.fromString stri
    in
        if (someInt = NONE) then [0,0]
        else 
            if String.isPrefix ((Int.toString(valOf(someInt)))^".") stri then [1,String.size(Int.toString(valOf(someInt)))]
            else [0,0]
    end
fun rem str =
    let
        fun loop (i) =
            if i < String.size str andalso String.sub(str,i) = #" " then
                loop (i + 1)
            else
                (i)
        val (i) = loop (0)  
    in
            if i = 0 then ["0", str]
            else [Int.toString(i),String.substring (str, i, size str - i)]
    
    end
    
    
fun html_processor(file)  = 
    let 
        val files = TextIO.openIn file
    in
        let 
            fun main_body(files,html_file,isTable,isListor,isListunor,wasSpace,popen) = 
                if (TextIO.endOfStream files andalso isListunor= 1 andalso isListor = 1) then
                    let 
                        val _ = appendFile(html_file,"</ul> </ol>")
                    in TextIO.closeIn files end
                
                else if (TextIO.endOfStream files andalso isListunor= 1 ) then
                    let 
                        val _ = appendFile(html_file,"</ul>")
                    in TextIO.closeIn files end

                else if (TextIO.endOfStream files andalso isListor= 1 ) then
                    let 
                        val _ = appendFile(html_file,"</ol>")
                    in TextIO.closeIn files end
                else if (TextIO.endOfStream files andalso isTable = 1) then
                    let 
                        val _ = appendFile(html_file,"<font color=red> error encountered . table not closed. ")
                    in TextIO.closeIn files end

                else if TextIO.endOfStream files then TextIO.closeIn files

                else
                    let
                        val list = rem(valOf(TextIO.inputLine files))
                    in
                        let 
                            val line = List.nth(list,1)
                            val isspace = List.nth(list,0)
                        
                        in
                            if isspace = "12" then
                                let 
                                    val ss = final_read(line,0,0,1)
                                in
                                let
                                    val _ = appendFile(html_file,"<code>")
                                    val _ = appendFile(html_file,ss)
                                    val _ = appendFile(html_file,"</code>")
                                    val _ = main_body(files,html_file,isTable,isListor,isListunor,wasSpace,popen)
                                in () end
                                end
                            else if (line = "<<\n") then 
                                let 
                                    val _ =appendFile (html_file, "<center> <table border = 1>")
                                    val _ = main_body(files,html_file,1,isListor,isListunor,wasSpace,popen)
                                in () end
                            else if (line = ">>\n") then
                                let 
                                    val _ = appendFile (html_file, "</table></center>")
                                    val _ =main_body(files,html_file,0,isListor,isListunor,wasSpace,popen)
                                in () end

                            else if String.isPrefix "---" line then 
                                let
                                    val _ = appendFile (html_file, "<hr>")
                                    val _ = main_body(files,html_file,isTable,isListor,isListunor,wasSpace,popen)
                                in () end

                            else if (String.isPrefix "-" line ) then
                                if (isListunor = 0) then 
                                    let val m = final_read(String.substring(line,1,String.size(line)-1),isTable,1,0) in
                                        if isListor = 1 andalso isspace = "0" then 
                                                let     
                                                    val _  = appendFile(html_file,"</ol>")
                                                    val _ = appendFile(html_file,"<ul>")
                                                    val _ = appendFile(html_file,"<li>")
                                                    val _ = appendFile(html_file,m)
                                                    val _ = main_body(files,html_file,isTable,0,1,wasSpace,popen) 
                                                in () end
                                        else 
                                                let
                                                    val _ = appendFile(html_file,"<ul>")
                                                    val _ = appendFile(html_file,"<li>")
                                                    val _ = appendFile(html_file,m)
                                                    val _ = main_body(files,html_file,isTable,isListor,1,0,popen) 
                                                in () end
                                    end
                                else 
                                    let val ma = final_read(String.substring(line,1,String.size(line)-1),isTable,1,0) in
                                        if isListor = 1 andalso isspace="0" then
                                        let 
                                            val _ = appendFile(html_file,"</ol>")
                                            val _ = appendFile(html_file,"<li>")
                                            val _ = appendFile(html_file,ma)
                                            val _ = main_body(files,html_file,isTable,0,1,0,popen)
                                        in () end
                                        else if isListor = 1 andalso isspace<>"0" then
                                        let 
                                            val _ = appendFile(html_file,"</li>")
                                            val _ = appendFile(html_file,"<li>")
                                            val _ = appendFile(html_file,ma)
                                            val _ = main_body(files,html_file,isTable,isListor,1,0,popen)
                                        in () end
                                        else
                                        let 
                                            
                                            val _ = appendFile(html_file,"<li>")
                                            val _ = appendFile(html_file,ma)
                                            val _ = main_body(files,html_file,isTable,isListor,1,0,popen)

                                        in () end
                                    end
                            
                            else if List.nth(checkString(line),0) = 1 then
                                if isListor = 0 then
                                    let
                                        val pp = final_read(String.substring(line,List.nth(checkString(line),1)+1 ,String.size(line)-List.nth(checkString(line),1)-1),isTable,1,0)
                                    in
                                        if isListunor = 1 andalso isspace = "0" then 
                                                let     
                                                    val _  = appendFile(html_file,"</ul>")
                                                    val _ = appendFile(html_file,"<ol>")
                                                    val _ = appendFile(html_file,"<li>")
                                                    val _ = appendFile(html_file,pp)
                                                    val _ = main_body(files,html_file,isTable,1,0,0,popen) 
                                                in () end
                                        else 
                                            let
                                                val _ = appendFile(html_file,"<ol>")
                                                val _ = appendFile(html_file,"<li>")
                                                val _ = appendFile(html_file,pp)
                                                    
                                                val _ = main_body(files,html_file,isTable,1,isListunor,0,popen) 
                                            in () end
                                        
                                    end
                                else 
                                    let
                                        val pr = final_read(String.substring(line,List.nth(checkString(line),1)+1,String.size(line)-List.nth(checkString(line),1)-1),isTable,1,0)
                                    in
                                        
                                        if isListunor = 1 andalso isspace= "0" then
                                        let 
                                            val _ = appendFile(html_file,"</ul>")
                                            val _ = appendFile(html_file,"<li>")
                                            val _ = appendFile(html_file,pr)
                                            val _ = main_body(files,html_file,isTable,1,0,0,popen)
                                        in () end
                                        else if isListunor = 1 andalso isspace<>"0" then
                                        let 
                                            val _ = appendFile(html_file,"</li>")
                                            val _ = appendFile(html_file,"<li>")
                                            val _ = appendFile(html_file,pr)
                                            val _ = main_body(files,html_file,isTable,1,isListunor,0,popen)
                                        in () end
                            
                                        else
                                        let 
                                            val _ = appendFile(html_file,"<li>")
                                            val _ = appendFile(html_file,pr)
                                            val _ = main_body(files,html_file,isTable,1,isListunor,0,popen)
                                        in () end
                                    end

                            else if (isTable  = 1 ) then 
                                let 
                                    val m = if (isListor+isListunor) >=1 then 1 else 0
                                in 
                                    let 
                                        val a = table_making(line,m)
                                    in
                                            let
                                                val _ = appendFile (html_file, a)
                                                val _ = main_body(files,html_file,1,isListor,isListunor,wasSpace,popen)
                                            in () end
                                    end
                                end
                            
                            else 
                                if line = "\n" then
                                        if (isListor=1  orelse isListunor = 1) then
                                        let
                                            val _ = appendFile(html_file,"</li>")
                                            val _ = appendFile(html_file,"")
                                            val _ = main_body(files,html_file,isTable,isListor,isListunor,1,popen)
                                        in () end
                                        else
                                            if popen = 1 then 
                                            let
                                                val _ = appendFile(html_file,"</p>")
                                                val _ = main_body(files,html_file,isTable,isListor,isListunor,1,0)
                                            in () end
                                            else 
                                            let
                                                val _ = appendFile(html_file,"<p>")
                                                val _ = main_body(files,html_file,isTable,isListor,isListunor,1,1)
                                            in () end

                                else
                                    let 
                                        val sp = final_read(line,0,0,0)
                                    in
                                        if isListor = 1 andalso wasSpace = 1 then  
                                            let 
                                                val _ = appendFile(html_file,"</ol>")
                                                val _ = appendFile(html_file,sp)
                                                val _ = main_body(files,html_file,isTable,0,isListunor,0,popen)
                                            in () end
                                        else if isListunor = 1 andalso wasSpace = 1 then   
                                            let 
                                                val _ = appendFile(html_file,"</ul>")
                                                val _ = appendFile(html_file,sp)
                                                val _ = main_body(files,html_file,isTable,isListor,0,0,popen)
                                            in () end
                                        else  
                                                let
                                                val _ = appendFile(html_file,sp)
                                                val _ = main_body(files,html_file,isTable,isListor,isListunor,0,popen)
                                            in () end
                                    end
                            end
                    end
        in  
            main_body(files,"filename.html",0,0,0,0,0)
        end
    end



fun mdt2html(file) = 
    let
    val _ = htmlmaker "filename.html"
    val _ = html_processor(file)
    val _ = htmlcloser("filename.html")
    in () end