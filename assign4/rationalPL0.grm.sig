signature rationalPL0_TOKENS =
sig
type ('a,'b) token
type svalue
val INVERSE:  'a * 'a -> (svalue,'a) token
val RAT:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val BOOLEAN: (string) *  'a * 'a -> (svalue,'a) token
val IDENTIFIER: (string) *  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val TODEC:  'a * 'a -> (svalue,'a) token
val FROMDEC:  'a * 'a -> (svalue,'a) token
val SHOWDEC:  'a * 'a -> (svalue,'a) token
val SHOWRAT:  'a * 'a -> (svalue,'a) token
val MAKERAT:  'a * 'a -> (svalue,'a) token
val CALL:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val OD:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val FI:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val INTEGER:  'a * 'a -> (svalue,'a) token
val RATIONAL:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val PROCEDURE:  'a * 'a -> (svalue,'a) token
val INTNEGATE:  'a * 'a -> (svalue,'a) token
val NEGATE:  'a * 'a -> (svalue,'a) token
val ORELSE:  'a * 'a -> (svalue,'a) token
val ANDALSO:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val GREATEREQ:  'a * 'a -> (svalue,'a) token
val LESSEQ:  'a * 'a -> (svalue,'a) token
val GREATER:  'a * 'a -> (svalue,'a) token
val LESS:  'a * 'a -> (svalue,'a) token
val NOTEQUAL:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val INTMOD:  'a * 'a -> (svalue,'a) token
val INTDIV:  'a * 'a -> (svalue,'a) token
val INTSUB:  'a * 'a -> (svalue,'a) token
val INTTIMES:  'a * 'a -> (svalue,'a) token
val INTPLUS:  'a * 'a -> (svalue,'a) token
val RATDIV:  'a * 'a -> (svalue,'a) token
val RATSUB:  'a * 'a -> (svalue,'a) token
val RATTIMES:  'a * 'a -> (svalue,'a) token
val RATPLUS:  'a * 'a -> (svalue,'a) token
val DECIMAL: (Rational.rational) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val NUM: (BigInt.bigint) *  'a * 'a -> (svalue,'a) token
end
signature rationalPL0_LRVALS=
sig
structure Tokens : rationalPL0_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
