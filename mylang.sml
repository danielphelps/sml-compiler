structure MyLang : sig
               val compile : string -> int 
               val usage : unit -> unit
               val main : (string * string list -> int)
                 end = 
struct

open TextIO;
open List;
exception TypeError;

  structure MyLangLrVals =
    MyLangLrValsFun(structure Token = LrParser.Token)

  structure MyLangLex =
    MyLangLexFun(structure Tokens = MyLangLrVals.Tokens)

  structure MyLangParser =
    Join(structure LrParser = LrParser
	 structure ParserData = MyLangLrVals.ParserData
	 structure Lex = MyLangLex)

 fun class_name(path) = 
   let val filename = last(String.tokens (fn x => x = #"/") path)
       val no_ext = hd(String.tokens (fn x => x = #".") filename) in
       no_ext
   end

   fun usage() =  (
     print("usage: mylang [mylang source file]")
   )

   fun exnToString(e) =
      List.foldr (op ^) "" ["[",
                            exnName(e),
                            if exnName(e) = exnMessage(e) then "" else " "^exnMessage(e),
                            "]"]

  fun println(s) = print(s ^ "\n");

  fun file_exists(file) = (
        let val s = openIn(file) in 
          closeIn(s);
	  true
        end
        handle e => (println(exnMessage(e)); false)
  )
  
   fun do_compile(file) = (
     let val (parse_tree, _) = (Parser.parse(file)) in 
                Unparser.unparse(parse_tree, 0)
         end
         )

 fun compile(file) = (
   if file_exists(file) then (
       do_compile(file);
       0
   )
   else (
       usage();
       1
   )
 )

   fun main(prog_name, args) = (
     if length(args) = 2 then (
         let val rv = compile(hd(tl(args))) 
                 handle e => 
                    (print("Compilation failed: "^exnToString(e)^"\n"); 1)
      in (
            rv
         )
         end
     )
     else (
       usage();
       1
     )
   )
   handle TypeError => (1 (*we already printed some diagnostics.*) )
   handle e => (print("MyLang compiler bug: " ^ exnToString(e) ^ "\n");
                2)

end (* structure MyLang *)
