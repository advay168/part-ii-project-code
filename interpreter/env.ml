open! Base

type 'value t = (Language.Var.t * 'value * bool) list

let string_of_t string_of_value (env : 'value t) =
  let members =
    List.filter_map
      ~f:(fun (var, value, hidden) ->
        Option.some_if (not hidden) (var ^ ": " ^ string_of_value value))
      env
    |> String.concat ~sep:", "
  in
  "{" ^ members ^ "}"
;;

let empty = []

let get name lst =
  lst
  |> List.find ~f:(fun (name', _, _) -> Language.Var.equal name name')
  |> Option.map ~f:(fun (_, value, _) -> value)
;;

let set ?(hidden = false) name value lst =
  if String.equal name "_" then lst else (name, value, hidden) :: lst
;;
