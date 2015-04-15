structure DataTypes =
struct

datatype
         id = id of string
and
         mylang_type = Void
and
         expr = StrLit of (string * int * int)
and
         mylang_decl = Method of (id * stmt list * int * int)
                  | NoneDecl
and 
         mylang_member_decls = MemberDecls of (mylang_decl list)
and      
         stmt = Write of (expr * int * int)
              | NoneStmt
and 
         mylang_program = Program of (id * mylang_member_decls)


fun typeToString(Void) = "void"

end;
