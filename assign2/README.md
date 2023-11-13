COL226 : ASSIGNMENT2 
OVERIVEW AND IMPOLEMETATION APPROACH 
helper function are:

0) handler : takes  a strign and a int
    the int tells how many <blockquotes> to be added in front of the strign.
1) final_read :
    this take the parameters as 
        str: which is the current string 

        isTable : will take value 1 if there is a table else 0. this parameter will be set by the funciton html_processor when after encountering "<<" we seestring is of form <some>|<string>|...
        
        isList ;  will take value 1 if there is a List else 0. this will hemp in addting

        iscoblk: this will take non-zere if there is a block code gien otherwise it will take 0. use to handle nested blockcodes.

this final_read function has a helper function named recurse which will read the string character by chareacter and then process accordingly. 
2) recurse : the parameters taken are    
    str: the siven string 

    isHeading : int value. take value 1 if a heading format is present in the given markdown string 

    heading: will take value equal to the number of hashes present (if any) to specify for the heading tags.
    It will count the hashes and then at then end will give "<h (hashes)>" the output "</h (hashes)>" as output.

    bold : will take 1 if '**' is present in the string . this is checked in the following way. If we encounter '*' then i check for the next symbol , if it is '*' then it is bold format in markdown language.it will become 0 when '*' followed by '*' , and will add the closing tag of bold in the output string

    italic: will take 1 if after encoutering "*" we do not have a floolwing "*", it will become 0 when again '*' is encountered and will add the closing tag of italic in the output string ;

    uline : will take value 1 if we encounter "_"  . It will become 0 when again "_" is encountered and will add the closing tag of underline in the output string 

    isLink: this will take value 1 if  link is present 
    since there are 2 forms of links, they have been encountered in the following way
        1) <https://www.<link>.com> : this is the direct format so a parameter link_text stores the link in it and when '>' is ecountered then '<a href =' <followed by link> ">" <link> "</a>" is added to the output string 

        2) [link](link_text): this is the indirect form . so when i encounter '[' then i start saving the part of string which is till ']' is stored in link_txt, if we encounter '(' just after the ']' then it will considered as link else not.
        Now, when i reach at '['  the couner pos (which is the position in string) by 2 and add "<a href = "+link_txt + ">" , to the output string , then link_text is direcly added to the string.

    isBlock : int value . this will take non-zero value (specifically, number of > encountered for nested blockquotes) . this is then given to bhandler to add the required number of blockquotes.
    if while reading the string we read ">" then we increase the counter by 1. and then we call the bhandler

    alphaencr : int value. 0 if no '>' is encountered in the start of string. example:  
    if string is ">>> string" then alphaencrwill remain 0 till 4th position and will take value 1 for the rest of the string

    posn ; position counter. since we are reading string character by character so we need a position which is served by this.

    out: stirng storing the html converted string of the input markdown string

3) appendfile : takes a filename and the content and writes the content in the specified file.

4) htmlmaker : creates a html file and writes the basic starte html code in it.

5) htmlcloser : take a html file and writes "</body></html>" in it thus closing the html file.

6) table_making:this is called if declaration of a table is encountered . this will replace '|' in the strign by '</td><td>' thereby creating cells in the table.this will then be passed to finla read for further processing.

7) checkstring: this takes a strign and checks whether it begins with "<integer>. " or not. this is used when list function is used

8) rem : this takes a string and then countes the number of leading spaces in the string.it returns a list with first element as number of leading spaces and second space as the string whose leading spaces are removed.

9) html_processor:
    this takes the make of the file. and then processes it line by line . it takes a line, identifies its main features line whether it is specifying a list or a table or the p tags count and then writs the html code line by line in the html file.

    the helper funciton for this is 
    1) main_body : the parameter and working is as follows.
        files : take the opened file from where the markdown language has to be read.

        html_file: name of the file where the html_code has to be written

        isListor: (int)will take value 1 if an ordered list has been declared else 0

        isListunor:(int) will take value 1 if an unordered list has been declared else 0

        wasspace : takes value 1 if a black line was present before the current line

        popen : take 1 if a paragraph tag is open else 0.

        working:
            for HR tags :
                if stirng suffix is "---" then hr tags are added 

            for CODEBLOCKS;
                the following design implementation is used for the codeblock design.
                if we ecnounter 12 spaces before a strign then it is treated as a codeblock. the <code> are added to the html_file. the string after the 12 spaces is processed by final_read and then pasted insode the <code> blocks.

            for TABLES: 
                tables sre specified by writitng "<<" in a line . encountering thi, the funciton will add <table> in the html_file. 
                the next sring after this declarating will be given to table_making helper funciton which will take the string . and reaplce all the '|' and eventually calls final_read which will read the string character by charecer . All the inline tags will be read and pasted as it is in the HTML file.
                if we encounter ">>" then the table is closed the line is changed..

            for ORDERED LIST:
                ordered list is declared bu a foemat "<int>." . this will be recognized by the checkstring helper function wchich will give 1 as the output if the list is declared. is 1 is received by checkstring
                then floolwing checks are hold 
                
                Now if the oredred list is being declared for the first time and unordered list was not declared before ,then , "<ol> <li>" is added to the html file.and the strign following it is given to the final_read for processing and then it is appended into the html file. 
                Otherwise if unorderedlist was declared before and now an ordered list is being declared with zero leadig spaces then the previous unordered lsit is closed and hte oredered list is opened

                If earlier the ordered list was declared and the followingchecks are done
                *) if unordered list was declared earlier and the new orederd is being declared with zero indentation so , the unorderedlist is closed and new <li> is added.

                *)  if unordered list was declared earlier and the new orederd is being declared with non zero indentation , so the list tags are closed and openend for proper formatting .following is an example if this
                        - un list element
                            2. ol 1st element
                            5. ol second element

                *)otherwise only li tags are added.

                In each case the string following the "<int>." is processed by final_read and then appended to the html_file


            for UNORDERED LIST: 
                they are declared usign '-'. so if a sring prefix is '-' then ul list is fprocessed in following way.
                if the unoredred list is being declared for the first time and ordered list was not declared before ,then , "<ul> <li>" is added to the html file.and the strign following it is given to the final_read for processing and then it is appended into the html file.
                Otherwise if an orderedlist was declared before and now an unordered list is being declared with zero leading spaces then the previous ordered lsit is closed and the oredered list is opened.

                If earlier the unordered list was declared and the followingchecks are done
                *) if ordered list was declared earlier and the new unordered is being declared with zero indentation so , the orderedlist is closed and new <li> is added.

                *)  if ordered list was declared earlier and the new orederd is being declared with non zero indentation , so the list tags are closed and openend for proper formatting .following is an example if this
                        6. ol list element
                            -ul 1st element
                            -ul second element

                *)otherwise only li tags are added.

                In each case the string following the "<int>." is processed by final_read and then appended to the html_file.

        to terminate either an ordered list or an unordered list we need to give a blank line after the last line of this list. 

        if a new line is encountered, i.e '/n' , then we have some check conditions for correct syntac of lists an s<p> tag to be followed.

        if no special character is read in from of the stirng then it is given to final_read to be processed as  a normal string.


mdt2html function: this take the name of he file as string 
then it calls the htmlmaker with the parameter filename.html. this will create a file named as filename.html and write the basic html code in it which contains <html> , <head> and all tags. the html_processor will translate the markdown code into html code and then the htmlcloser will closr the html file.


This method reads the input file line by line and then character by character. There is no backtracking and the file is read only once.
ERROR HANDELING ;
if the line ahs used bold/italic or underline and it has not been closed rporperly then my code appends line in the HTML file saying the error and the line in which the error occured.
also if the table is opened and not closed procerly then the error is appended int eh similar manner.
