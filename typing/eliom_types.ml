
let cmi_magic_number = "Eliom000I020"
let () = assert
  (String.length cmi_magic_number = String.length Config.cmi_magic_number)

let is_mixed _ = false
