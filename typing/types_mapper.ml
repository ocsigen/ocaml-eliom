[@@@ocaml.warning "-40"]
open Types
class t =
  object (self)
    method abbrev_memo : abbrev_memo -> abbrev_memo =
      fun x  ->
        match x with
        | Mnil  -> Mnil
        | Mcons (a,b,c,d,e) ->
            let a = (fun x  -> x) a  in
            let b = self#path b  in
            let c = self#type_expr c  in
            let d = self#type_expr d  in
            let e = self#abbrev_memo e  in Mcons (a, b, c, d, e)
        | Mlink (a) ->
            let a = (self#pervasives_ref self#abbrev_memo) a  in Mlink (a)
    method array : 'a . ('a -> 'a) -> 'a array -> 'a array = Array.map
    method bool : bool -> bool =
      fun x  -> match x with | false  -> false | true  -> true
    method char : char -> char = fun x  -> x
    method class_declaration : class_declaration -> class_declaration =
      fun
        { cty_params; cty_type; cty_path; cty_new; cty_variance; cty_loc;
          cty_attributes }
         ->
        let cty_params = (self#list self#type_expr) cty_params  in
        let cty_type = self#class_type cty_type  in
        let cty_path = self#path cty_path  in
        let cty_new = (self#option self#type_expr) cty_new  in
        let cty_variance = (self#list (fun x  -> x)) cty_variance  in
        let cty_loc = self#location cty_loc  in
        let cty_attributes = self#parsetree_attributes cty_attributes  in
        {
          cty_params;
          cty_type;
          cty_path;
          cty_new;
          cty_variance;
          cty_loc;
          cty_attributes
        }
    method class_signature : class_signature -> class_signature =
      fun { csig_self; csig_vars; csig_concr; csig_inher }  ->
        let csig_self = self#type_expr csig_self  in
        let csig_vars = (fun x  -> x) csig_vars  in
        let csig_concr = (fun x  -> x) csig_concr  in
        let csig_inher =
          (self#list
             (fun (a,b)  ->
                let a = self#path a  in
                let b = (self#list self#type_expr) b  in (a, b))) csig_inher
           in
        { csig_self; csig_vars; csig_concr; csig_inher }
    method class_type : class_type -> class_type =
      fun x  ->
        match x with
        | Cty_constr (a,b,c) ->
            let a = self#path a  in
            let b = (self#list self#type_expr) b  in
            let c = self#class_type c  in Cty_constr (a, b, c)
        | Cty_signature (a) ->
            let a = self#class_signature a  in Cty_signature (a)
        | Cty_arrow (a,b,c) ->
            let a = (fun x  -> x) a  in
            let b = self#type_expr b  in
            let c = self#class_type c  in Cty_arrow (a, b, c)
    method class_type_declaration :
      class_type_declaration -> class_type_declaration =
      fun
        { clty_params; clty_type; clty_path; clty_variance; clty_loc;
          clty_attributes }
         ->
        let clty_params = (self#list self#type_expr) clty_params  in
        let clty_type = self#class_type clty_type  in
        let clty_path = self#path clty_path  in
        let clty_variance = (self#list (fun x  -> x)) clty_variance  in
        let clty_loc = self#location clty_loc  in
        let clty_attributes = self#parsetree_attributes clty_attributes  in
        {
          clty_params;
          clty_type;
          clty_path;
          clty_variance;
          clty_loc;
          clty_attributes
        }
    method commutable : commutable -> commutable =
      fun x  ->
        match x with
        | Cok  -> Cok
        | Cunknown  -> Cunknown
        | Clink (a) ->
            let a = (self#pervasives_ref self#commutable) a  in Clink (a)
    method constructor_arguments :
      constructor_arguments -> constructor_arguments =
      fun x  ->
        match x with
        | Cstr_tuple (a) ->
            let a = (self#list self#type_expr) a  in Cstr_tuple (a)
        | Cstr_record (a) ->
            let a = (self#list self#label_declaration) a  in Cstr_record (a)
    method constructor_declaration :
      constructor_declaration -> constructor_declaration =
      fun { cd_id; cd_args; cd_res; cd_loc; cd_attributes }  ->
        let cd_id = self#ident cd_id  in
        let cd_args = self#constructor_arguments cd_args  in
        let cd_res = (self#option self#type_expr) cd_res  in
        let cd_loc = self#location cd_loc  in
        let cd_attributes = self#parsetree_attributes cd_attributes  in
        { cd_id; cd_args; cd_res; cd_loc; cd_attributes }
    method constructor_description :
      constructor_description -> constructor_description =
      fun
        { cstr_name; cstr_res; cstr_existentials; cstr_args; cstr_arity;
          cstr_tag; cstr_consts; cstr_nonconsts; cstr_normal;
          cstr_generalized; cstr_private; cstr_loc; cstr_attributes;
          cstr_inlined }
         ->
        let cstr_name = self#string cstr_name  in
        let cstr_res = self#type_expr cstr_res  in
        let cstr_existentials = (self#list self#type_expr) cstr_existentials
           in
        let cstr_args = (self#list self#type_expr) cstr_args  in
        let cstr_arity = self#int cstr_arity  in
        let cstr_tag = self#constructor_tag cstr_tag  in
        let cstr_consts = self#int cstr_consts  in
        let cstr_nonconsts = self#int cstr_nonconsts  in
        let cstr_normal = self#int cstr_normal  in
        let cstr_generalized = self#bool cstr_generalized  in
        let cstr_private = (fun x  -> x) cstr_private  in
        let cstr_loc = self#location cstr_loc  in
        let cstr_attributes = self#parsetree_attributes cstr_attributes  in
        let cstr_inlined = (self#option self#type_declaration) cstr_inlined
           in
        {
          cstr_name;
          cstr_res;
          cstr_existentials;
          cstr_args;
          cstr_arity;
          cstr_tag;
          cstr_consts;
          cstr_nonconsts;
          cstr_normal;
          cstr_generalized;
          cstr_private;
          cstr_loc;
          cstr_attributes;
          cstr_inlined
        }
    method constructor_tag : constructor_tag -> constructor_tag =
      fun x  ->
        match x with
        | Cstr_constant (a) -> let a = self#int a  in Cstr_constant (a)
        | Cstr_block (a) -> let a = self#int a  in Cstr_block (a)
        | Cstr_extension (a,b) ->
            let a = self#path a  in
            let b = self#bool b  in Cstr_extension (a, b)
    method ext_status : ext_status -> ext_status =
      fun x  ->
        match x with
        | Text_first  -> Text_first
        | Text_next  -> Text_next
        | Text_exception  -> Text_exception
    method extension_constructor :
      extension_constructor -> extension_constructor =
      fun
        { ext_type_path; ext_type_params; ext_args; ext_ret_type;
          ext_private; ext_loc; ext_attributes }
         ->
        let ext_type_path = self#path ext_type_path  in
        let ext_type_params = (self#list self#type_expr) ext_type_params  in
        let ext_args = self#constructor_arguments ext_args  in
        let ext_ret_type = (self#option self#type_expr) ext_ret_type  in
        let ext_private = (fun x  -> x) ext_private  in
        let ext_loc = self#location ext_loc  in
        let ext_attributes = self#parsetree_attributes ext_attributes  in
        {
          ext_type_path;
          ext_type_params;
          ext_args;
          ext_ret_type;
          ext_private;
          ext_loc;
          ext_attributes
        }
    method field_kind : field_kind -> field_kind =
      fun x  ->
        match x with
        | Fvar (a) ->
            let a = (self#pervasives_ref (self#option self#field_kind)) a  in
            Fvar (a)
        | Fpresent  -> Fpresent
        | Fabsent  -> Fabsent
    method ident : Ident.t -> Ident.t =
      fun x -> Ident.with_side (Ident.side x) x
    method int : int -> int = fun x  -> x
    method label_declaration : label_declaration -> label_declaration =
      fun { ld_id; ld_mutable; ld_type; ld_loc; ld_attributes }  ->
        let ld_id = self#ident ld_id  in
        let ld_mutable = (fun x  -> x) ld_mutable  in
        let ld_type = self#type_expr ld_type  in
        let ld_loc = self#location ld_loc  in
        let ld_attributes = self#parsetree_attributes ld_attributes  in
        { ld_id; ld_mutable; ld_type; ld_loc; ld_attributes }
    method label_description : label_description -> label_description =
      fun
        { lbl_name; lbl_res; lbl_arg; lbl_mut; lbl_pos; lbl_all; lbl_repres;
          lbl_private; lbl_loc; lbl_attributes }
         ->
        let lbl_name = self#string lbl_name  in
        let lbl_res = self#type_expr lbl_res  in
        let lbl_arg = self#type_expr lbl_arg  in
        let lbl_mut = (fun x  -> x) lbl_mut  in
        let lbl_pos = self#int lbl_pos  in
        let lbl_all = (self#array self#label_description) lbl_all  in
        let lbl_repres = self#record_representation lbl_repres  in
        let lbl_private = (fun x  -> x) lbl_private  in
        let lbl_loc = self#location lbl_loc  in
        let lbl_attributes = self#parsetree_attributes lbl_attributes  in
        {
          lbl_name;
          lbl_res;
          lbl_arg;
          lbl_mut;
          lbl_pos;
          lbl_all;
          lbl_repres;
          lbl_private;
          lbl_loc;
          lbl_attributes
        }
    method list : 'a . ('a -> 'a) -> 'a list -> 'a list =
      fun map_a  ->
        fun x  ->
          match x with
          | [] -> []
          | a::b ->
              let a = map_a a  in let b = (self#list map_a) b  in a :: b
    method location : Location.t -> Location.t =
      fun { loc_start; loc_end; loc_ghost }  ->
        let loc_start = (fun x  -> x) loc_start  in
        let loc_end = (fun x  -> x) loc_end  in
        let loc_ghost = self#bool loc_ghost  in
        { loc_start; loc_end; loc_ghost }
    method longident : Longident.t -> Longident.t =
      fun x  ->
        match x with
        | Lident (a) -> let a = self#string a  in Lident (a)
        | Ldot (a,b) ->
            let a = self#longident a  in
            let b = self#string b  in Ldot (a, b)
        | Lapply (a,b) ->
            let a = self#longident a  in
            let b = self#longident b  in Lapply (a, b)
    method modtype_declaration : modtype_declaration -> modtype_declaration =
      fun { mtd_type; mtd_attributes; mtd_loc }  ->
        let mtd_type = (self#option self#module_type) mtd_type  in
        let mtd_attributes = self#parsetree_attributes mtd_attributes  in
        let mtd_loc = self#location mtd_loc  in
        { mtd_type; mtd_attributes; mtd_loc }
    method module_declaration : module_declaration -> module_declaration =
      fun { md_type; md_attributes; md_loc }  ->
        let md_type = self#module_type md_type  in
        let md_attributes = self#parsetree_attributes md_attributes  in
        let md_loc = self#location md_loc  in
        { md_type; md_attributes; md_loc }
    method module_type : module_type -> module_type =
      fun x  ->
        match x with
        | Mty_ident (a) -> let a = self#path a  in Mty_ident (a)
        | Mty_signature (a) -> let a = self#signature a  in Mty_signature (a)
        | Mty_functor (a,b,c) ->
            let a = self#ident a  in
            let b = (self#option self#module_type) b  in
            let c = self#module_type c  in Mty_functor (a, b, c)
        | Mty_alias (a) -> let a = self#path a  in Mty_alias (a)
    method option : 'a . ('a -> 'a) -> 'a option -> 'a option =
      fun map_a  ->
        fun x  ->
          match x with
          | None  -> None
          | Some (a) -> let a = map_a a  in Some (a)
    method parsetree_attributes :
      Parsetree.attributes -> Parsetree.attributes = self#list (fun x  -> x)
    method path : Path.t -> Path.t =
      fun x  ->
        match x with
        | Pident (a) -> let a = self#ident a  in Pident (a)
        | Pdot (a,b,c) ->
            let a = self#path a  in
            let b = self#string b  in let c = self#int c  in Pdot (a, b, c)
        | Papply (a,b) ->
            let a = self#path a  in let b = self#path b  in Papply (a, b)
    method pervasives_ref :
      'a . ('a -> 'a) -> 'a Pervasives.ref -> 'a Pervasives.ref =
      fun map_a  ->
        fun { contents }  -> let contents = map_a contents  in { contents }
    method rec_status : rec_status -> rec_status =
      fun x  ->
        match x with
        | Trec_not  -> Trec_not
        | Trec_first  -> Trec_first
        | Trec_next  -> Trec_next
    method record_representation :
      record_representation -> record_representation =
      fun x  ->
        match x with
        | Record_regular  -> Record_regular
        | Record_float  -> Record_float
        | Record_inlined (a) -> let a = self#int a  in Record_inlined (a)
        | Record_extension  -> Record_extension
    method row_desc : row_desc -> row_desc =
      fun
        { row_fields; row_more; row_bound; row_closed; row_fixed; row_name } 
        ->
        let row_fields =
          (self#list
             (fun (a,b)  ->
                let a = (fun x  -> x) a  in
                let b = self#row_field b  in (a, b))) row_fields
           in
        let row_more = self#type_expr row_more  in
        let row_bound = self#unit row_bound  in
        let row_closed = self#bool row_closed  in
        let row_fixed = self#bool row_fixed  in
        let row_name =
          (self#option
             (fun (a,b)  ->
                let a = self#path a  in
                let b = (self#list self#type_expr) b  in (a, b))) row_name
           in
        { row_fields; row_more; row_bound; row_closed; row_fixed; row_name }
    method row_field : row_field -> row_field =
      fun x  ->
        match x with
        | Rpresent (a) ->
            let a = (self#option self#type_expr) a  in Rpresent (a)
        | Reither (a,b,c,d) ->
            let a = self#bool a  in
            let b = (self#list self#type_expr) b  in
            let c = self#bool c  in
            let d = (self#pervasives_ref (self#option self#row_field)) d  in
            Reither (a, b, c, d)
        | Rabsent  -> Rabsent
    method signature : signature -> signature = self#list self#signature_item
    method signature_item : signature_item -> signature_item =
      fun x  ->
        match x with
        | Sig_value (a,b) ->
            let a = self#ident a  in
            let b = self#value_description b  in Sig_value (a, b)
        | Sig_type (a,b,c) ->
            let a = self#ident a  in
            let b = self#type_declaration b  in
            let c = self#rec_status c  in Sig_type (a, b, c)
        | Sig_typext (a,b,c) ->
            let a = self#ident a  in
            let b = self#extension_constructor b  in
            let c = self#ext_status c  in Sig_typext (a, b, c)
        | Sig_module (a,b,c) ->
            let a = self#ident a  in
            let b = self#module_declaration b  in
            let c = self#rec_status c  in Sig_module (a, b, c)
        | Sig_modtype (a,b) ->
            let a = self#ident a  in
            let b = self#modtype_declaration b  in Sig_modtype (a, b)
        | Sig_class (a,b,c) ->
            let a = self#ident a  in
            let b = self#class_declaration b  in
            let c = self#rec_status c  in Sig_class (a, b, c)
        | Sig_class_type (a,b,c) ->
            let a = self#ident a  in
            let b = self#class_type_declaration b  in
            let c = self#rec_status c  in Sig_class_type (a, b, c)
    method string : string -> string = fun x  -> x
    method type_declaration : type_declaration -> type_declaration =
      fun
        { type_params; type_arity; type_kind; type_private; type_manifest;
          type_variance; type_newtype_level; type_loc; type_attributes;
          type_immediate }
         ->
        let type_params = (self#list self#type_expr) type_params  in
        let type_arity = self#int type_arity  in
        let type_kind = self#type_kind type_kind  in
        let type_private = (fun x  -> x) type_private  in
        let type_manifest = (self#option self#type_expr) type_manifest  in
        let type_variance = (self#list (fun x  -> x)) type_variance  in
        let type_newtype_level =
          (self#option
             (fun (a,b)  ->
                let a = self#int a  in let b = self#int b  in (a, b)))
            type_newtype_level
           in
        let type_loc = self#location type_loc  in
        let type_attributes = self#parsetree_attributes type_attributes  in
        let type_immediate = self#bool type_immediate  in
        {
          type_params;
          type_arity;
          type_kind;
          type_private;
          type_manifest;
          type_variance;
          type_newtype_level;
          type_loc;
          type_attributes;
          type_immediate
        }
    method type_desc : type_desc -> type_desc =
      fun x  ->
        match x with
        | Tvar (a) -> let a = (self#option self#string) a  in Tvar (a)
        | Tarrow (a,b,c,d) ->
            let a = (fun x  -> x) a  in
            let b = self#type_expr b  in
            let c = self#type_expr c  in
            let d = self#commutable d  in Tarrow (a, b, c, d)
        | Ttuple (a) -> let a = (self#list self#type_expr) a  in Ttuple (a)
        | Tconstr (a,b,c) ->
            let a = self#path a  in
            let b = (self#list self#type_expr) b  in
            let c = (self#pervasives_ref self#abbrev_memo) c  in
            Tconstr (a, b, c)
        | Tobject (a,b) ->
            let a = self#type_expr a  in
            let b =
              (self#pervasives_ref
                 (self#option
                    (fun (a,b)  ->
                       let a = self#path a  in
                       let b = (self#list self#type_expr) b  in (a, b)))) b
               in
            Tobject (a, b)
        | Tfield (a,b,c,d) ->
            let a = self#string a  in
            let b = self#field_kind b  in
            let c = self#type_expr c  in
            let d = self#type_expr d  in Tfield (a, b, c, d)
        | Tnil  -> Tnil
        | Tlink (a) -> let a = self#type_expr a  in Tlink (a)
        | Tsubst (a) -> let a = self#type_expr a  in Tsubst (a)
        | Tvariant (a) -> let a = self#row_desc a  in Tvariant (a)
        | Tunivar (a) -> let a = (self#option self#string) a  in Tunivar (a)
        | Tpoly (a,b) ->
            let a = self#type_expr a  in
            let b = (self#list self#type_expr) b  in Tpoly (a, b)
        | Tpackage (a,b,c) ->
            let a = self#path a  in
            let b = (self#list self#longident) b  in
            let c = (self#list self#type_expr) c  in Tpackage (a, b, c)
    method type_expr : type_expr -> type_expr =
      fun { desc; level; id }  ->
        let desc = self#type_desc desc  in
        let level = self#int level  in
        let id = self#int id  in { desc; level; id }
    method type_kind : type_kind -> type_kind =
      fun x  ->
        match x with
        | Type_abstract  -> Type_abstract
        | Type_record (a,b) ->
            let a = (self#list self#label_declaration) a  in
            let b = self#record_representation b  in Type_record (a, b)
        | Type_variant (a) ->
            let a = (self#list self#constructor_declaration) a  in
            Type_variant (a)
        | Type_open  -> Type_open
    method type_transparence : type_transparence -> type_transparence =
      fun x  ->
        match x with
        | Type_public  -> Type_public
        | Type_new  -> Type_new
        | Type_private  -> Type_private
    method unit : unit -> unit = fun x  -> match x with | () -> ()
    method value_description : value_description -> value_description =
      fun { val_type; val_kind; val_loc; val_attributes }  ->
        let val_type = self#type_expr val_type  in
        let val_kind = self#value_kind val_kind  in
        let val_loc = self#location val_loc  in
        let val_attributes = self#parsetree_attributes val_attributes  in
        { val_type; val_kind; val_loc; val_attributes }
    method value_kind : value_kind -> value_kind =
      fun x  ->
        match x with
        | Val_reg  -> Val_reg
        | Val_prim (a) -> let a = (fun x  -> x) a  in Val_prim (a)
        | Val_ivar (a,b) ->
            let a = (fun x  -> x) a  in
            let b = self#string b  in Val_ivar (a, b)
        | Val_self (a,b,c,d) ->
            let a = (self#pervasives_ref (fun x  -> x)) a  in
            let b = (self#pervasives_ref (fun x  -> x)) b  in
            let c = self#string c  in
            let d = self#type_expr d  in Val_self (a, b, c, d)
        | Val_anc (a,b) ->
            let a =
              (self#list
                 (fun (a,b)  ->
                    let a = self#string a  in let b = self#ident b  in (a, b)))
                a
               in
            let b = self#string b  in Val_anc (a, b)
        | Val_unbound  -> Val_unbound
  end
