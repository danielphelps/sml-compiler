structure Unparser  = 
struct 
open DataTypes;

(* Print the AST to standard output. *)

fun indent n = 
    let fun mk_str size = if size < 1 then "" else " " ^ mk_str(size - 1);
    in
      print(mk_str(n))
    end

fun print_indent(str, margin) = (indent(margin);
                                 print(str))

fun unparse(Program(id(name), members), margin) =
     (print("class " ^ name ^ " { \n"); 
      unparse_members(members, margin + 1);
      print("\n}\n"))
and 
    unparse_members(MemberDecls(methods), margin) =
     (unparse_decls(methods, margin))
and 
    unparse_decls(decls, margin) = List.map (fn d => unparse_decl(d, margin)) decls
and 
     unparse_decl(Method(id(name), stmts, _, _), margin) = 
      (indent(margin);
       print("void "^name^"() { \n");
       unparse_stmts(stmts, margin + 1);
       print_indent("}\n", margin))
   | unparse_decl(NoneDecl, margin) = ()
and 
     unparse_stmts(stmts, margin) = List.map (fn stmt => unparse_stmt(stmt, margin)) stmts
and 
      unparse_stmt(Write(str, _, _), margin) =  (
        print_indent("print(", margin);
        unparse_expr(str);
        print(");\n")
      )
    | unparse_stmt(NoneStmt, margin) = ()

and unparse_expr(StrLit(str, _, _)) = (print(str)
      )
end
