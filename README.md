# COL226-Assignments

## Assignment 1:
This algorithm uses the long division method to calculate the square root of an integer in the form of a string. It first separates the input string into pairs of digits and stores them in a list, which is then reversed. Next, the algorithm uses a helper function called "nearest Sqrt" to find the nearest square value which is less than or equal to the first digit of the input string. This nearest square value is then used as the first digit of the square root.
The algorithm then uses another helper function "step1" to update the input list by subtracting the square of the nearest square value from the first digit of the input string. This updated list is then passed to the next helper function "helper1" which uses a recursive approach to find the next digits of the square root.
The function "helper1" takes the updated list and the nearest square value as inputs and starts a loop. In each iteration of the loop, it concatenates the first two digits of the input list, and uses a function "recurse" to find the next digit of the square root by dividing the concatenated digits by a value calculated from the nearest square value and the current digit of the square root. It then updates the input list by subtracting the product of the current digit of the square root and a value calculated from the nearest square value and the current digit of the square root. The current digit of the square root is then added to the final square root value.
The loop continues until the input list has only one digit left. At that point, the final square root value and the remaining digit in the input list are returned as the integral part and the remainder of the square root respectively.
This algorithm can be useful when the input integer is very large and cannot be stored or processed directly as a whole number. The long division method allows the calculation of the square root to be done incrementally and in a way that is memory efficient. However, it may have a relatively large time complexity due to the many recursive calls and looping through the input list.

## Assignment 2:
SML function mdt2html which takes a single input text file filename.mdt and translates it into a HTML file filename.html.
Specifications are listed in the mdtab-2023.md.pdf

## Assignment 3:
a (command-line) user-interface for rational number expressions using ML-lex ad ML-Yacc.
I have made a simple command line interface for rational function. The base files of lex and yacc were takes from the official document available on SML/NJ website. 
the folder parser and the files within it will simply make a command line interface where suer can evaluate long expressions involvinf rationals ( +  for add,  - for sub , * for mult and / for div). He will also be able to convert a decimal to rational.
Rest of the func tionality defined in rational.sml can't be used in the interface and can be used by importing rational sml in the SML REPL.

## Assignment 4:

the "rationalnumber" are generated from the graamer in assignmnet 2. "intnumber" are generated from the bigint grammer on the second assignment.
NOTE: FOR COMPILING THE FUNCTION YOU HAVE TO RUN "run.sml" FILE 

File names and function. :
the following file has been implemented 
1) compile.sml : this is to compile the lex and yacc file for an input file . this will give the AST.
2) datatypes.sml : this contains all the datatypes defined for the grammar. 
3) glue.sml : this is a glue code for lex and yacc to be parsed.
4) hashmap.sml : this contains implementation of a hashmap.
5) input.rat : input file 
6) rational.sml : this contain the rational and bigInt data structure . details of these were provided in assigment 3
7) rationalPL0.cm : compile file to compile all the strucutres
8) rationalPL0.lex :  lexer for token generation
9) rationalPL0.grm  : contains the grammar for the language. 
        I am checking the types of terminals on which the operation is defined by making  a hashmap and putting the values in it.
10) reader.sml : this contains the main Structure . details are provided below.
11) stack.sml : contains a implementation of stack in sml
12) stackDT.sml : "contains the structure for all the commands which will be stored in the Control stack."


READER.sml
1) The lex and yacc file are giving the AST for the given program . 
2) create_stack function takes this tree and generates ControlStack.
3) for Symbol table and memory i have implemented the following strucutres
        a. memory :  this is a string array which will store the values of variables.
        b. memorytable : a hashmap which will map the name of the variable to its location in memory array.
        c. scopetable :  a hashmap which will map the name of the variable to its scope .
        d. num_var : this keeps count of the number of varibales declared 
4) for computation i have created a Value stack.
5) the names of the function declared in the reader.sml show the functions they perform.

Working
First I am converting the AST to the postfix notation and then storing it in control stack. 
then i'm taking popping the commands from ControlStack one by one and processign it using execute function.
Note: Static scoping is missing in this assigment

Following error are implemented 
1) rat error: any error in expression involving rationals will give a rat error
2) UnDeclaredVariableException: whenever a variable is used wihout declaration.
3) OperationNotFound: symbol which is not part of operators.
4) ProcedureNotDeclared: procedure is used wihotu declaration
5) VariableRedeclarationException : when a varible is redeclared .
6) TypeMisMatchException : if two different types of number are used in a operator. say ".+." being used on two bigint number. 


## Assignment 5:
4 prolog programs for the problems listed in 4-simple-problems.pdf

