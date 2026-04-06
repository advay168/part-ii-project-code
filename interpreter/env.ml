open! Base

type 'value t = (Language.Var.t * 'value * bool) list

let string_of_t string_of_value (env : 'value t) =
  let members =
    List.filter_map
      ~f:(fun (var, value, hidden) ->
        Option.some_if
          (not (hidden || Language.Var.to_ignore var))
          (Language.Var.to_string var ^ ": " ^ string_of_value value))
      env
    |> String.concat ~sep:", "
  in
  "{" ^ members ^ "}"
;;

let empty = []

let get name env_lst =
  match
    List.find env_lst ~f:(fun (name', _, _) -> Language.Var.equal name name')
  with
  | None -> None
  | Some (_, value, _) -> Some value
;;

let set ?(hidden = false) name value lst = (name, value, hidden) :: lst
