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

let to_complet_json (relations : relations) =
  let a = List.nth relations 0 in
  let b = List.nth relations 1 in
  let relation_0 =
    { input =
        { parameter = a.relation.input.parameter;
          storage_i = a.relation.input.storage_i
        };
      output = { storage_o = a.relation.output.storage_o }
    }
  in
  let relation_1 =
    { input =
        { parameter = b.relation.input.parameter;
          storage_i = b.relation.input.storage_i
        };
      output = { storage_o = b.relation.output.storage_o }
    }
  in
  print_endline
    (Yojson.Safe.pretty_to_string
       (relations_to_yojson
          [ { index = "0"; relation = relation_0 };
            { index = "1"; relation = relation_1 } ]))

let x =
  [ { index = "0";
      relation =
        { input = { parameter = "A"; storage_i = "B" };
          output = { storage_o = "AB" }
        }
    };
    { index = "1";
      relation =
        { input = { parameter = "A"; storage_i = "B" };
          output = { storage_o = "ABC" }
        }
    } ]

;;
to_complet_json x

;;
to_json_format_string "x" "y" "xy"

;;
to_json "x" "y" "xy"
