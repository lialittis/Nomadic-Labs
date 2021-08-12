(*
val ty_of_yojson : Yojson.Safe.json -> (ty, string) Result.result
val ty_of_yojson_exn : Yojson.Safe.json -> ty
val ty_to_yojson : ty -> Yojson.Safe.json
*)

type output = { storage_o : string } [@@deriving to_yojson]

type input = { parameter : string; storage_i : string } [@@deriving to_yojson]

type relation = { input : input; output : output } [@@deriving to_yojson]

type relation_t = { index : string; relation : relation } [@@deriving to_yojson]

type relations = relation_t list [@@deriving to_yojson]

let to_json (i1 : string) (i2 : string) (o1 : string) =
  print_endline
    (Yojson.Safe.pretty_to_string
       (relation_to_yojson
          { input = { parameter = i1; storage_i = i2 };
            output = { storage_o = o1 }
          }))

let to_json_format_string (i1 : string) (i2 : string) (o1 : string) =
  Yojson.Safe.pretty_to_string
    (relation_to_yojson
       { input = { parameter = i1; storage_i = i2 };
         output = { storage_o = o1 }
       })

let to_full_relation_json (relation_t : relation_t) =
  Yojson.Safe.pretty_to_string (relation_t_to_yojson relation_t)

let to_complete_json_list (relations : relations) =
  print_endline (Yojson.Safe.pretty_to_string (relations_to_yojson relations))
