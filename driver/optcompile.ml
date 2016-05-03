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

(* The batch compiler *)

open Libcompile

let tool_name = "ocamlopt"

let c_file = Libcompile.c_file
let interface = Libcompile.interface ~tool_name

(* Compile a .ml file *)

let (|>>) (x, y) f = (x, f y)

let flambda ({source_provenance} as i) backend typed =
  if !Clflags.classic_inlining then begin
    Clflags.default_simplify_rounds := 1;
    Clflags.use_inlining_arguments_set Clflags.classic_arguments;
    Clflags.unbox_free_vars_of_closures := false;
    Clflags.unbox_specialised_args := false
  end;
  typed
  |> Timings.(time (Timings.Transl i.sourcefile)
      (Translmod.transl_implementation_flambda i.modulename))
  |>> print_if i Clflags.dump_rawlambda Printlambda.lambda
  |> Timings.time (Timings.Generate i.sourcefile) (fun lambda ->
    lambda
    |>> Simplif.simplify_lambda
    |>> print_if i Clflags.dump_lambda Printlambda.lambda
    |> (fun ((module_ident, size), lam) ->
      Middle_end.middle_end i.ppf ~source_provenance
        ~prefixname:i.outputprefix
        ~size
        ~filename:i.sourcefile
        ~module_ident
        ~backend
        ~module_initializer:lam)
    |> Asmgen.compile_implementation_flambda ~source_provenance
      i.outputprefix ~backend i.ppf;
    Compilenv.save_unit_info (cmx i))

let clambda i typed =
  Clflags.use_inlining_arguments_set Clflags.classic_arguments;
  typed
  |> Timings.(time (Transl i.sourcefile))
    (Translmod.transl_store_implementation i.modulename)
  |> print_if i Clflags.dump_rawlambda Printlambda.program
  |> Timings.(time (Generate i.sourcefile))
    (fun { Lambda.code; main_module_block_size } ->
       { Lambda.code = Simplif.simplify_lambda code;
         main_module_block_size }
       |> print_if i Clflags.dump_lambda Printlambda.program
       |> Asmgen.compile_implementation_clambda
         ~source_provenance:i.source_provenance i.outputprefix i.ppf;
       Compilenv.save_unit_info (cmx i))

let implementation ppf sourcefile outputprefix ~backend =
  let info = init ppf ~init_path:true ~tool_name ~sourcefile ~outputprefix in
  Compilenv.reset ~source_provenance:info.source_provenance
    ?packname:!Clflags.for_package info.modulename;

  let frontend info = typecheck_impl info @@ parse_impl info in
  let backend info typed =
    if Config.flambda then flambda info backend typed
    else clambda info typed
  in

  wrap_compilation ~frontend ~backend info
