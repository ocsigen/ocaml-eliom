
type res = {
  server : Parsetree.structure ;
  client : Parsetree.structure ;
}

val untype : Typedtree.structure -> res
