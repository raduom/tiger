type type_id = string

type id = string

type ty =
  | StringType
  | IntType
  | TypeId of type_id
  | Record of (id * type_id) list
  | Array  of type_id

type op =
  | Plus
  | Minus
  | Times
  | Divide
  | Equals
  | NotEquals
  | Gt
  | Lt
  | Gte
  | Lte
  | And
  | Or

type lvalue =
  | Id          of id
  | RecordIndex of id * id
  | ArrayIndex  of id * int

type tm =
  (* Combinators *)
  | App        of id * tm list
  | Seq        of tm * tm
  | Op         of op * tm * tm
  | IfThenElse of tm * tm * tm
  | Let        of decl list * tm option

  (* Values *)
  | Nil
  | Unit
  | StringLiteral of string
  | IntLiteral    of int
  | NewRecord     of type_id * (id * tm) list
  | NewArray      of type_id * tm * tm

  (* Statements *)
  | Assoc  of lvalue * tm
  | IfThen of tm * tm
  | While  of tm * tm
  | For    of id * tm * tm * tm
  | Break

and decl =
  | Var      of id * type_id option * tm
  | Type     of type_id * ty
  | Function of id * (id * type_id) list * type_id option * tm
