structure DataTypes =
struct

datatype
         id = id of string
and
         csx_type = Void
and
         expr = StrLit of (string * int * int)
and
         csx_decl = Method of (id * stmt list * int * int)
                  | NoneDecl
and 
         csx_member_decls = MemberDecls of (csx_decl list)
and      
         stmt = Write of (expr * int * int)
              | NoneStmt
and 
         csx_program = Program of (id * csx_member_decls)


fun typeToString(Void) = "void"

end;
