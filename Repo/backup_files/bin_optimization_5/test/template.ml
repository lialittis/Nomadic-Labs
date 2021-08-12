type _ ty = TyInt : int ty | TyPair : 'a ty * 'b ty -> ('a * 'b) ty

type exty = Ex : 'a ty -> exty

let rec gen : type a. a ty -> a = function
  | TyInt ->
      Random.bits ()
  | TyPair (a, b) ->
      (gen a, gen b)

let rec show : type a. a ty -> a -> string = function
  | TyInt ->
      string_of_int
  | TyPair (a, b) ->
      fun x -> Printf.sprintf "(%s, %s)" (show a (fst x)) (show b (snd x))

let g (ex : exty) : string = match ex with Ex ty -> show ty (gen ty)

type dyn = ExValue : 'a ty * 'a -> dyn

let f (ex : exty) : dyn = match ex with Ex ty -> ExValue (ty, gen ty)

type (_, _) eq = Refl : ('a, 'a) eq

let rec equal_ty : type a b. a ty -> b ty -> (a, b) eq option =
 fun a b ->
  match (a, b) with
  | (TyInt, TyInt) ->
      Some Refl
  | (TyPair (a, b), TyPair (a', b')) -> (
    match equal_ty a a' with
    | Some Refl -> (
      match equal_ty b b' with Some Refl -> Some Refl | None -> None )
    | None ->
        None )
