open! Base

(** Polymorphic environment type (covariant). *)
type 'value t = (Language.Var.t * 'value * bool) list

let empty = []

let get name env_lst =
  match
    List.find env_lst ~f:(fun (name', _, _) -> Language.Var.equal name name')
  with
  | None -> None
  | Some (_, value, _) -> Some value
;;

(** Binds variable. Set [hidden] for builtin variables which should not be
    displayed. *)
let set ?(hidden = false) name value lst = (name, value, hidden) :: lst

(** Converts to string ignoring [hidden] variables. *)
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
