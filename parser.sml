structure Parser =
struct

structure MyLangLrVals = MyLangLrValsFun(structure Token = LrParser.Token)

structure MyLangLex = MyLangLexFun(structure Tokens = MyLangLrVals.Tokens)

structure MyLangParser = Join(structure Lex= MyLangLex
		              structure LrParser = LrParser
		              structure ParserData = MyLangLrVals.ParserData
                      structure AST = DataTypes)

open DataTypes;


val lexing_error = ref false 

exception SyntaxError;

fun parse s =
    let val dev = TextIO.openIn s
	val stream = MyLangParser.makeLexer(fn i => TextIO.inputN(dev,i))
	fun error (e,i:int,_) = (
            lexing_error := true;
	    TextIO.output(TextIO.stdOut,
                          s ^ "," ^ " line " ^ (Int.toString i) ^ ", Error: " ^ e ^ "\n")
        )
        val ast = MyLangParser.parse(0,stream,error,()) 
     in
        if !lexing_error then
                raise SyntaxError
        else 
                ast
    end
    handle LrParser.ParseError => raise SyntaxError
    handle IO.Io{cause, function, name} => ( raise cause)
    handle e => (print("Unknown lexer/parser error \n"); raise e)

fun lex filename =
    let val file = TextIO.openIn filename
        val lexer = MyLangLex.makeLexer(fn i => TextIO.inputN(file, i))
        fun do_it() =
                let val t = lexer()
                 in t
        end in 
         do_it()
    end 

end (* structure Parser *)
