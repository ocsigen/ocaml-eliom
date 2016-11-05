(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Libcompile

let tool_name = "ocamlc"

let c_file = Libcompile.c_file
let interface = Libcompile.interface ~tool_name

(** Compile a .ml file to bytecode *)

let to_bytecode i (typedtree, coercion) =
  (typedtree, coercion)
  |> Timings.(time (Transl i.sourcefile))
    (Translmod.transl_implementation i.modulename)
  |> print_if i Clflags.dump_rawlambda Printlambda.lambda
  |> Timings.(accumulate_time (Generate i.sourcefile))
    (fun lambda ->
       Simplif.simplify_lambda lambda
       |> print_if i Clflags.dump_lambda Printlambda.lambda
       |> Bytegen.compile_implementation i.modulename
       |> print_if i Clflags.dump_instr Printinstr.instrlist)

let emit_bytecode i bytecode =
  let objfile = i.outputprefix ^ ".cmo" in
  let oc = open_out_bin objfile in
  try
    bytecode
    |> Timings.(accumulate_time (Generate i.sourcefile))
      (Emitcode.to_file oc i.modulename objfile);
    Warnings.check_fatal ();
    close_out oc;
    Stypes.dump (Some (i.outputprefix ^ ".annot"))
  with x ->
    close_out oc;
    Misc.remove_file objfile;
    raise x

let frontend info = typecheck_impl info @@ parse_impl info
let backend info typed = emit_bytecode info @@ to_bytecode info typed

let implementation ppf sourcefile outputprefix =
  let info = init ppf ~init_path:false ~tool_name ~sourcefile ~outputprefix in

  wrap_compilation ~frontend ~backend info

(* ELIOM *)
let eliom_implementation ppf sourcefile outputprefix =
  let info = init ppf ~init_path:false ~tool_name ~sourcefile ~outputprefix in
  let comp_side s side ast =
    Eliom_base.set_mode (Eliom_base.Splitted side) ;
    Envaux.reset_cache () ;
    Env.reset_required_globals () ;
    let info = eliom_init s
        ppf ~init_path:false ~tool_name ~sourcefile ~outputprefix
    in
    let ppf =
      Format.formatter_of_out_channel @@ open_out (info.outputprefix^".ml")
    in
    Format.fprintf ppf "%a@." Pprintast.structure ast ;
    backend info @@ silent_typing info ast ;
  in
  let server = comp_side "server" Eliom_base.Server in
  let client = comp_side "client" Eliom_base.Client in

  eliom_wrap ~frontend ~server ~client info
(* /ELIOM *)
