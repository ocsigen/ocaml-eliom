(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Compenv

type info = {
  sourcefile : string ;
  modulename : string ;
  outputprefix : string ;
  env : Env.t ;
  ppf : Format.formatter ;
  tool_name : string ;
  source_provenance : Timings.source_provenance ;
}


let cmx i = i.outputprefix ^ ".cmx"
let obj i = i.outputprefix ^ Config.ext_obj

let print_if i flag printer arg =
  if !flag then Format.fprintf i.ppf "%a@." printer arg;
  arg

let init ppf ~init_path ~tool_name ~sourcefile ~outputprefix =
  let source_provenance = Timings.File sourcefile in
  Compmisc.init_path init_path;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  { modulename ; outputprefix ; env ; sourcefile ; ppf ;
    tool_name ; source_provenance ;
  }


(** Compile a .mli file *)

let parse_intf i =
  Pparse.parse_interface ~tool_name:i.tool_name i.ppf i.sourcefile
  |> print_if i Clflags.dump_parsetree Printast.interface
  |> print_if i Clflags.dump_source Pprintast.signature

let typecheck_intf info ast =
  let tsg =
    ast
    |> Typemod.type_interface info.env
    |> print_if info Clflags.dump_typedtree Printtyped.interface
  in
  let sg = tsg.Typedtree.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env info.env (fun () ->
        Format.(fprintf std_formatter) "%a@."
          Printtyp.signature (Typemod.simplify_signature sg));
  ignore (Includemod.signatures info.env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  tsg

let emit_signature info ast tsg =
  let sg =
    let deprecated = Builtin_attributes.deprecated_of_sig ast in
    Env.save_signature ~deprecated tsg.Typedtree.sig_type
      info.modulename (info.outputprefix ^ ".cmi")
  in
  Typemod.save_signature info.modulename tsg
    info.outputprefix info.sourcefile info.env sg

let interface ppf ~tool_name ~sourcefile ~outputprefix =
  let info = init ppf ~init_path:false ~tool_name ~sourcefile ~outputprefix in
  let ast = parse_intf info in
  let tsg = typecheck_intf info ast in
  if not !Clflags.print_types then begin
    emit_signature info ast tsg
  end


(** Frontend for a .ml file *)

let parse_impl i =
  Pparse.parse_implementation ~tool_name:i.tool_name i.ppf i.sourcefile
  |> print_if i Clflags.dump_parsetree Printast.implementation
  |> print_if i Clflags.dump_source Pprintast.structure

let typecheck_impl i parsetree =
  parsetree
  |> Timings.(time (Typing i.sourcefile))
    (Typemod.type_implementation i.sourcefile i.outputprefix i.modulename i.env)
  |> print_if i Clflags.dump_typedtree
    Printtyped.implementation_with_coercion

let wrap_compilation ~frontend ~backend info =
  try
    let typed = frontend info in
    if not !Clflags.print_types then
      backend info typed
    else begin
      Warnings.check_fatal ();
      Stypes.dump (Some (info.outputprefix ^ ".annot"));
    end
  with x ->
    Stypes.dump (Some (info.outputprefix ^ ".annot"));
    Misc.remove_file (obj info);
    Misc.remove_file (cmx info);
    raise x


(** C file. *)

let c_file name =
  Location.input_name := name;
  if Ccomp.compile_file name <> 0 then exit 2

(** Eliom files *)

let eliom_wrap ~frontend ~client ~server info =
  let backend _info (ty,_) =
    let pty = Eliom_emit.untype ty in
    server pty.Eliom_emit.server ;
    client pty.Eliom_emit.client  ;
  in
  wrap_compilation ~frontend ~backend info

let eliom_init suffix ppf ~init_path ~tool_name ~sourcefile ~outputprefix =
  let outputprefix = outputprefix ^ "." ^ suffix in
  let sourcefile = sourcefile ^ "." ^ suffix in
  init ppf ~init_path ~tool_name ~sourcefile ~outputprefix
let silent_typing i ast =
  let val_dont_write_files = !Clflags.dont_write_files in
  Clflags.dont_write_files := true;
  let typedtree =
    Timings.(time (Typing i.sourcefile))
      (Typemod.type_implementation i.sourcefile i.outputprefix i.modulename i.env)
      ast
  in
  Clflags.dont_write_files := val_dont_write_files;
  typedtree
