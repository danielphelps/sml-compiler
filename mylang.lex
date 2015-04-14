structure Tokens = Tokens

(* Any functions or values defined here will be available in the 
   semantic actions.

   Set want_debug = true to get information from the lexer via stdout. 
 *)
type pos = int
type col = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val lineNum = ref 1
val pos = ref 1
val col = ref 1

fun advance_column(str) = col := (!col) + String.size(str)

fun eof () = Tokens.EOF(!pos, !col)

fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

val want_debug = false;

fun debug str = 
    if want_debug then print ("lexer found: "^str^"\n") else ()

%%
%header (functor MyLangLexFun(structure Tokens: MyLang_TOKENS));

ws=[\ \t];
ALPHABETIC=[A-Za-z];
IDENTIFIER={ALPHABETIC}+;

STRLIT=\"{ALPHABETIC}+\";

LINE_COMMENT=\/\/.*;

BLOCK_COMMENT=##(#?[^#])*##;
%%

\n              => (pos := (!pos) + 1; col := 0; debug (yytext^"=>return ");lex());
{ws}+           => (advance_column(yytext); debug (yytext^"=> whitespace ");lex());

[pP][rR][iI][nN][tT]          => (debug (yytext^"=>RW_PRINT "); advance_column(yytext); Tokens.RW_PRINT(!pos, !col));
[cC][lL][aA][sS][sS]          => (debug (yytext^"=>RW_CLASS "); advance_column(yytext); Tokens.RW_CLASS(!pos, !col));
[vV][oO][iI][dI]          => (debug (yytext^"=>RW_VOID "); advance_column(yytext); Tokens.RW_VOID(!pos, !col));

{STRLIT}        => (debug (yytext^"=>STRLIT "); advance_column(yytext); Tokens.STRLIT(yytext, !pos, !col));
";"             => (debug (yytext^"=>SEMI "); advance_column(yytext);Tokens.SEMI(!pos, !col));
"{"             => (debug (yytext^"=>LBRACE "); advance_column(yytext); Tokens.LBRACE(!pos, !col));
"}"             => (debug (yytext^"=>RBRACE "); advance_column(yytext); Tokens.RBRACE(!pos, !col));
"("             => (debug (yytext^"=>LPAREN "); advance_column(yytext); Tokens.LPAREN(!pos, !col));
")"             => (debug (yytext^"=>RPAREN "); advance_column(yytext); Tokens.RPAREN(!pos, !col));
{BLOCK_COMMENT} => (debug (yytext^"=>block comment"); 
                    let val num_nl = length(List.filter (fn c => c = #"\n") (explode yytext))
                    in
                        pos := (!pos) + num_nl;
                        lex()
                    end);

{IDENTIFIER}    => (debug (yytext^"=>ID "); advance_column(yytext); Tokens.ID(yytext, !pos, !col));

{LINE_COMMENT} => (debug (yytext^"=>line comment");
                   lex());

.      => (error ("Invalid Token: "^yytext, !pos,!pos); 
           OS.Process.exit(OS.Process.success));
