type t = Top | Bot | Atom of string
       | Conj of t*t | Disj of t*t | Imp of t*t
       | Box of t | Dia of t | Neg of t

let top = "true"
let bot = "false"
let atom = ""
let neg = "~"
let conj = "&"
let disj = "v"
let imp = "->"
let box = "box"
let dia = "dia"

module Tex = struct
  let top = "\\top"
	let bot = "\\bot"
	let conj = "\\wedge"
  let disj = "\\vee"
	let imp = "\\supset"
	let box = "\\Box"
	let dia = "\\Diamond"
end;;
