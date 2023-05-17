structure rationalPL0LrVals =
  rationalPL0LrValsFun(structure Token = LrParser.Token)

structure rationalPL0Lex =
  rationalPL0LexFun(structure Tokens = rationalPL0LrVals.Tokens)

structure rationalPL0Parser =
  Join(structure LrParser = LrParser
  structure ParserData = rationalPL0LrVals.ParserData
  structure Lex = rationalPL0Lex)