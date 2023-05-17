functor rationalPL0LexFun(structure Tokens: rationalPL0_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

structure Tokens = Tokens

type pos = int (*type of the position in the tokens*)
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0 
val eof = fn () => Tokens.EOF (!pos,!pos);


(*keywords declaratoin here . *)
(* dont think i need a keyword in here . if not running then see. *)


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[([(#"\t",#"\t",1),
(#" ",#" ",1),
(#"\n",#"\n",2),
(#"!",#"!",3),
(#"\"",#"\"",4),
(#"%",#"%",5),
(#"&",#"&",6),
(#"(",#"(",7),
(#")",#")",8),
(#"*",#"*",9),
(#"+",#"+",10),
(#",",#",",11),
(#"-",#"-",12),
(#".",#".",13),
(#"/",#"/",14),
(#"0",#"9",15),
(#":",#":",16),
(#";",#";",17),
(#"<",#"<",18),
(#"=",#"=",19),
(#">",#">",20),
(#"A",#"Z",21),
(#"g",#"h",21),
(#"j",#"l",21),
(#"n",#"n",21),
(#"q",#"q",21),
(#"u",#"u",21),
(#"x",#"z",21),
(#"a",#"a",22),
(#"b",#"b",23),
(#"c",#"c",24),
(#"d",#"d",25),
(#"e",#"e",26),
(#"f",#"f",27),
(#"i",#"i",28),
(#"m",#"m",29),
(#"o",#"o",30),
(#"p",#"p",31),
(#"r",#"r",32),
(#"s",#"s",33),
(#"t",#"t",34),
(#"v",#"v",35),
(#"w",#"w",36),
(#"{",#"{",37),
(#"|",#"|",38),
(#"}",#"}",39),
(#"~",#"~",40)], []), ([(#"\t",#"\t",1),
(#" ",#" ",1)], [1]), ([], [0]), ([], [24]), ([(#".",#".",154),
(#"0",#"9",4)], []), ([], [11]), ([(#"&",#"&",171)], []), ([(#"*",#"*",168)], [19]), ([], [20]), ([], [8]), ([(#".",#".",154),
(#"0",#"9",4)], [7]), ([], [53]), ([], [9]), ([(#"(",#"(",155),
(#"*",#"*",158),
(#"+",#"+",159),
(#"-",#"-",160),
(#"/",#"/",161),
(#"0",#"9",154),
(#"~",#"~",162)], []), ([], [10]), ([(#".",#".",154),
(#"0",#"9",15)], [56]), ([(#"=",#"=",153)], []), ([], [23]), ([(#"=",#"=",151),
(#">",#">",152)], [14]), ([], [12]), ([(#"=",#"=",150)], [15]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"m",21),
(#"o",#"z",21),
(#"n",#"n",144)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"n",21),
(#"p",#"z",21),
(#"o",#"o",138)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",135)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"n",21),
(#"p",#"z",21),
(#"o",#"o",134)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",131)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"e",21),
(#"g",#"h",21),
(#"j",#"q",21),
(#"s",#"z",21),
(#"f",#"f",119),
(#"i",#"i",120),
(#"r",#"r",121)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"e",21),
(#"g",#"m",21),
(#"o",#"z",21),
(#"f",#"f",107),
(#"n",#"n",108)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",100)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"c",21),
(#"e",#"q",21),
(#"s",#"z",21),
(#"d",#"d",94),
(#"r",#"r",95)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"q",21),
(#"s",#"z",21),
(#"r",#"r",83)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"d",21),
(#"f",#"z",21),
(#"a",#"a",73),
(#"e",#"e",74)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"g",21),
(#"i",#"z",21),
(#"h",#"h",60)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"g",21),
(#"i",#"n",21),
(#"p",#"s",21),
(#"u",#"z",21),
(#"h",#"h",48),
(#"o",#"o",49),
(#"t",#"t",50)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",46)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"g",21),
(#"i",#"z",21),
(#"h",#"h",42)], [58]), ([], [22]), ([(#"|",#"|",41)], []), ([], [21]), ([], [28]), ([], [26]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"h",21),
(#"j",#"z",21),
(#"i",#"i",43)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",44)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",45)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [40, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"q",21),
(#"s",#"z",21),
(#"r",#"r",47)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [32, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",58)], [58]), ([(#"0",#"9",21),
(#"A",#"C",21),
(#"E",#"Z",21),
(#"a",#"z",21),
(#"D",#"D",51)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [54, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",52)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"b",21),
(#"d",#"z",21),
(#"c",#"c",53)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"h",21),
(#"j",#"z",21),
(#"i",#"i",54)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"l",21),
(#"n",#"z",21),
(#"m",#"m",55)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",56)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",57)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [52, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"m",21),
(#"o",#"z",21),
(#"n",#"n",59)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [37, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"n",21),
(#"p",#"z",21),
(#"o",#"o",61)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"v",21),
(#"x",#"z",21),
(#"w",#"w",62)], [58]), ([(#"0",#"9",21),
(#"A",#"C",21),
(#"E",#"Q",21),
(#"S",#"Z",21),
(#"a",#"z",21),
(#"D",#"D",63),
(#"R",#"R",64)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",67)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",65)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"s",21),
(#"u",#"z",21),
(#"t",#"t",66)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [49, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"b",21),
(#"d",#"z",21),
(#"c",#"c",68)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"h",21),
(#"j",#"z",21),
(#"i",#"i",69)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"l",21),
(#"n",#"z",21),
(#"m",#"m",70)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",71)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",72)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [50, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"s",21),
(#"u",#"z",21),
(#"t",#"t",77)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",75)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"c",21),
(#"e",#"z",21),
(#"d",#"d",76)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [44, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"h",21),
(#"j",#"z",21),
(#"i",#"i",78)], [47, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"n",21),
(#"p",#"z",21),
(#"o",#"o",79)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"m",21),
(#"o",#"z",21),
(#"n",#"n",80)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",81)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",82)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [33, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"h",21),
(#"j",#"n",21),
(#"p",#"z",21),
(#"i",#"i",84),
(#"o",#"o",85)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"m",21),
(#"o",#"z",21),
(#"n",#"n",92)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"b",21),
(#"d",#"z",21),
(#"c",#"c",86)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",87)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"c",21),
(#"e",#"z",21),
(#"d",#"d",88)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"t",21),
(#"v",#"z",21),
(#"u",#"u",89)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"q",21),
(#"s",#"z",21),
(#"r",#"r",90)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",91)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [31, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"s",21),
(#"u",#"z",21),
(#"t",#"t",93)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [43, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [42, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",96)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",97)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"r",21),
(#"t",#"z",21),
(#"s",#"s",98)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",99)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [30, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"j",21),
(#"l",#"z",21),
(#"k",#"k",101)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",102)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21),
(#"_",#"_",103)], [58]), ([(#"r",#"r",104)], []), ([(#"a",#"a",105)], []), ([(#"t",#"t",106)], []), ([], [48]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [36, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"s",21),
(#"u",#"u",21),
(#"w",#"z",21),
(#"t",#"t",109),
(#"v",#"v",110)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",115)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",111)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"q",21),
(#"s",#"z",21),
(#"r",#"r",112)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"r",21),
(#"t",#"z",21),
(#"s",#"s",113)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",114)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [46, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"f",21),
(#"h",#"z",21),
(#"g",#"g",116)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",117)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"q",21),
(#"s",#"z",21),
(#"r",#"r",118)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [34, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [55, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [39, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"n",21),
(#"p",#"z",21),
(#"o",#"o",122)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"l",21),
(#"n",#"z",21),
(#"m",#"m",123)], [58]), ([(#"0",#"9",21),
(#"A",#"C",21),
(#"E",#"Z",21),
(#"a",#"z",21),
(#"D",#"D",124)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",125)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"b",21),
(#"d",#"z",21),
(#"c",#"c",126)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"h",21),
(#"j",#"z",21),
(#"i",#"i",127)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"l",21),
(#"n",#"z",21),
(#"m",#"m",128)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",129)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",130)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [51, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"r",21),
(#"t",#"z",21),
(#"s",#"s",132)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",133)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [38, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [41, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",136)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",137)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [45, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"n",21),
(#"p",#"z",21),
(#"o",#"o",139)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",140)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"d",21),
(#"f",#"z",21),
(#"e",#"e",141)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",142)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"m",21),
(#"o",#"z",21),
(#"n",#"n",143)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [35, 58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"c",21),
(#"e",#"z",21),
(#"d",#"d",145)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"b",#"z",21),
(#"a",#"a",146)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"k",21),
(#"m",#"z",21),
(#"l",#"l",147)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"r",21),
(#"t",#"z",21),
(#"s",#"s",148)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"n",21),
(#"p",#"z",21),
(#"o",#"o",149)], [58]), ([(#"0",#"9",21),
(#"A",#"Z",21),
(#"a",#"z",21)], [29, 58]), ([], [17]), ([], [16]), ([], [13]), ([], [18]), ([(#"(",#"(",155),
(#"0",#"9",154)], []), ([(#"0",#"9",156)], []), ([(#")",#")",157),
(#"0",#"9",156)], []), ([], [57]), ([(#".",#".",167)], []), ([(#".",#".",166)], []), ([(#".",#".",165)], []), ([(#".",#".",164)], []), ([(#".",#".",163)], []), ([], [27]), ([], [6]), ([], [5]), ([], [3]), ([], [4]), ([(#"\n",#"\n",168),
(#" ",#")",168),
(#"+",#"~",168),
(#"*",#"*",169)], []), ([(#"\n",#"\n",168),
(#" ",#"(",168),
(#"+",#"~",168),
(#")",#")",170),
(#"*",#"*",169)], []), ([(#"\n",#"\n",168),
(#" ",#")",168),
(#"+",#"~",168),
(#"*",#"*",169)], [2]), ([], [25])]
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      ( pos:= (!pos) + 1; lex()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (pos := !pos + size yytext ; continue())
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATPLUS(!pos,!pos)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATTIMES(!pos,!pos)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATSUB(!pos,!pos)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATDIV(!pos,!pos)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTPLUS(!pos,!pos)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTTIMES(!pos,!pos)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTSUB(!pos,!pos)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTDIV(!pos,!pos)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTMOD(!pos,!pos)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQUAL(!pos,!pos)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NOTEQUAL(!pos,!pos)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LESS(!pos,!pos)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GREATER(!pos,!pos)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LESSEQ(!pos,!pos)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GREATEREQ(!pos,!pos)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ASSIGN(!pos,!pos)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(!pos,!pos)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(!pos,!pos)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(!pos,!pos)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(!pos,!pos)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMI(!pos,!pos)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NOT(!pos,!pos)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      ( Tokens.ANDALSO(!pos,!pos)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ORELSE(!pos,!pos)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEGATE(!pos,!pos)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTNEGATE(!pos,!pos)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ANDALSO(!pos,!pos)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ORELSE(!pos,!pos)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PROCEDURE(!pos,!pos)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VAR(!pos,!pos)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATIONAL(!pos,!pos)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTEGER(!pos,!pos)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BOOL(!pos,!pos)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF(!pos,!pos)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.THEN(!pos,!pos)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ELSE(!pos,!pos)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FI(!pos,!pos)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.WHILE(!pos,!pos)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DO(!pos,!pos)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OD(!pos,!pos)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PRINT(!pos,!pos)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.READ(!pos,!pos)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.CALL(!pos,!pos)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INVERSE(!pos,!pos)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RAT(!pos,!pos)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MAKERAT(!pos,!pos)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SHOWRAT(!pos,!pos)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SHOWDEC(!pos,!pos)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FROMDEC(!pos,!pos)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TODEC(!pos,!pos)))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(!pos,!pos)))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TRUE(!pos,!pos)))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FALSE(!pos,!pos)))
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.NUM(BigInt.fromString(yytext), !pos ,!pos))
      end
fun yyAction57 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.DECIMAL(Rational.fromDecimal(yytext),!pos,!pos))
      end
fun yyAction58 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( Tokens.IDENTIFIER( yytext ,!pos , !pos ))
      end
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54, yyAction55, yyAction56, yyAction57, yyAction58])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of INITIAL => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
      (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
