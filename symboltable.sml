structure SymbolTable =  
struct 
  open HashTable;
  open DataTypes;

  (* The implementation and behavior of the symbol table depends partly on
     the desired semantics of your language.  This implementation is not
     complete, and only intended to be a reference.
  *)
     
  datatype symb_info = YouDecide

  datatype scope = Scope of { table : (string, symb_info) hash_table}

  datatype symb_table = SymbTable of (scope list)

  exception DuplicateSymb;
  exception NoAvailableScope;
  exception NotFound;
  exception NotAMethod;

  val hash_fn : string->word = HashString.hashString
  fun cmp_fn (a : string, b : string) = (a = b)

  (* It is unlikely to have more than 50 declarations. *)
  (* Anyway, the table will resize, if necessary. *)
  val initial_size : int = 50  

  fun make_hash_table() = (mkTable (hash_fn, cmp_fn) 
                                             (initial_size, NotFound))
  fun make_scope() = Scope({table = make_hash_table()});

  fun open_scope(SymbTable(nil) : symb_table) = 
      SymbTable([make_scope()])
    | open_scope(SymbTable(top::scopes)) = 
      SymbTable(make_scope()::([top] @ scopes))

  fun make_symbol_table() = SymbTable([make_scope()]);

  fun close_scope(SymbTable(Scope(top)::rest)) = SymbTable(rest)
    | close_scope(SymbTable(nil)) = raise NoAvailableScope

  fun insert_symb(_,_, SymbTable(nil)) = raise NoAvailableScope
    | insert_symb(name : string, 
                  info : symb_info, 
                  SymbTable(Scope(top)::rest) : symb_table) =
        let val the_table = (#table(top))
        in 
          if inDomain (the_table) name then
            raise DuplicateSymb
          else
            let val _ = insert the_table (name, info) in (
              SymbTable(Scope({table = the_table})::rest)
              )
            end
        end

  fun open_scope_and_then(table, action) = 
    let val augmented_table = open_scope(table) in
      action(augmented_table)
    end

end;
