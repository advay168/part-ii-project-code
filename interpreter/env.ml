open! Base

type 'value t = (Language.Var.t * 'value) list

let string_of_t string_of_value (env : 'value t) =
  let members =
    List.map ~f:(fun (var, value) -> var ^ ": " ^ string_of_value value) env
    |> String.concat ~sep:", "
  in
  "{" ^ members ^ "}"
;;

let empty = []

let get name lst =
  lst
  |> List.find ~f:(fun (name', _) -> Language.Var.equal name name')
  |> Option.map ~f:snd
;;

let set name value lst =
  if String.equal name "_" then lst else (name, value) :: lst
;;
