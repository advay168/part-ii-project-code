open! Base

type 'value t = (Language.Var.t * 'value) list

let empty = []

let get name lst =
  lst
  |> List.find ~f:(fun (name', _) -> Language.Var.equal name name')
  |> Option.map ~f:snd
;;

let set name value lst = (name, value) :: lst
