open Types
class t :
  object
    method  abbrev_memo : abbrev_memo -> abbrev_memo
    method  array : 'a . ('a -> 'a) -> 'a array -> 'a array
    method  bool : bool -> bool
    method  char : char -> char
    method  class_declaration : class_declaration -> class_declaration
    method  class_signature : class_signature -> class_signature
    method  class_type : class_type -> class_type
    method  class_type_declaration :
      class_type_declaration -> class_type_declaration
    method  commutable : commutable -> commutable
    method  constructor_arguments :
      constructor_arguments -> constructor_arguments
    method  constructor_declaration :
      constructor_declaration -> constructor_declaration
    method  constructor_description :
      constructor_description -> constructor_description
    method  constructor_tag : constructor_tag -> constructor_tag
    method  ext_status : ext_status -> ext_status
    method  extension_constructor :
      extension_constructor -> extension_constructor
    method  field_kind : field_kind -> field_kind
    method  ident : Ident.t -> Ident.t
    method  int : int -> int
    method  label_declaration : label_declaration -> label_declaration
    method  label_description : label_description -> label_description
    method  list : 'a . ('a -> 'a) -> 'a list -> 'a list
    method  location : Location.t -> Location.t
    method  longident : Longident.t -> Longident.t
    method  modtype_declaration : modtype_declaration -> modtype_declaration
    method  module_declaration : module_declaration -> module_declaration
    method  module_type : module_type -> module_type
    method  option : 'a . ('a -> 'a) -> 'a option -> 'a option
    method  parsetree_attributes :
      Parsetree.attributes -> Parsetree.attributes
    method  path : Path.t -> Path.t
    method  pervasives_ref :
      'a . ('a -> 'a) -> 'a Pervasives.ref -> 'a Pervasives.ref
    method  rec_status : rec_status -> rec_status
    method  record_representation :
      record_representation -> record_representation
    method  row_desc : row_desc -> row_desc
    method  row_field : row_field -> row_field
    method  signature : signature -> signature
    method  signature_item : signature_item -> signature_item
    method  string : string -> string
    method  type_declaration : type_declaration -> type_declaration
    method  type_desc : type_desc -> type_desc
    method  type_expr : type_expr -> type_expr
    method  type_kind : type_kind -> type_kind
    method  type_transparence : type_transparence -> type_transparence
    method  unit : unit -> unit
    method  value_description : value_description -> value_description
    method  value_kind : value_kind -> value_kind
  end
