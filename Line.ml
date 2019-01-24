type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm of exp list
and exp = IdExp of id
        | NumExp of int
        | OpExp of exp * binop * exp
        | EseqExp of stm * exp

let prog =
  CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
              CompoundStm(AssignStm("b",
                                    EseqExp(PrintStm [IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)],
                                            OpExp(NumExp 10, Times, IdExp "a"))),
                          PrintStm [IdExp "b"]))

let rec maxargs stm : int =
  match stm with
  | CompoundStm(sl, sr) -> max (maxargs sl) (maxargs sr)
  | AssignStm(_, e) -> maxargsE e
  | PrintStm es -> List.fold_left (fun m s -> max m (maxargsE s)) (List.length es) es
and maxargsE exp : int =
  match exp with
  | EseqExp(s, e) -> max (maxargs s) (maxargsE e)
  | _ -> 0

module type ST = sig
  type t

  val add_sym : id -> int -> t -> t

  val value_of : id -> t -> int

  val append_output : string -> t -> t

  val output : t -> string list
end

module BasicState : ST = struct
  type t = { symbol_table : (id * int) list ; output : string list }

  let add_sym l v st =
    { symbol_table = (l, v) :: List.remove_assoc l st.symbol_table ;
      output = st.output }

  let value_of l st = List.assoc l st.symbol_table

  let append_output l st =
    { symbol_table = st.symbol_table ; output = l :: st.output }

  let output st = st.output
end

module S = BasicState

let runOp op vl vr =
  match op with
  | Plus -> vl + vr
  | Minus -> vl - vr
  | Times -> vl * vr
  | Div -> vl / vr

let rec evalE e (s : S.t) : (S.t * int) =
  match e with
  | IdExp(label) -> (s, S.value_of label s)
  | NumExp(n) -> (s, n)
  | OpExp(el, op, er) ->
    let (s', vl) = evalE el s in
    let (s'', vr) = evalE er s' in
    (s'', runOp op vl vr)
  | EseqExp(stm, exp) ->
    let s' = evalS stm s in
    evalE exp s'
and evalS s st : S.t =
  match s with
  | CompoundStm(sl, sr) ->
    let st' = evalS sl st in
    evalS sr st'
  | AssignStm(label, e) ->
    let (s', v) = evalE e st in
    S.add_sym label v s'
  | PrintStm _ -> st (* how do i print stuff? *)

type key = string

type tree = Leaf
          | Branch of tree * key * tree

let empty = Leaf

(* Exercise 1.1 *)

let rec insert key = function
  | Leaf -> Branch(Leaf, key, Leaf)
  | Branch(lt, k, rt) ->
    if key < k
    then Branch(insert key lt, k, rt)
    else if key > k
    then Branch(lt, k, insert key rt)
    else Branch(lt, k, rt)

let rec member key = function
  | Leaf -> false
  | Branch(lt, k, rt) ->
    if key < k
    then member k lt
    else if key > k
    then member k rt
    else true

type 'a kv_tree = Leaf
                | Branch of 'a kv_tree * key * 'a * 'a kv_tree

let rec kv_insert key value = function
  | Leaf -> Branch(Leaf, key, value, Leaf)
  | Branch(lt, k, v, rt) ->
    if key < k
    then Branch(kv_insert key value lt, k, v, rt)
    else if key > k
    then Branch(lt, k, v, kv_insert key value rt)
    else Branch(lt, k, v, rt)

let rec kv_member key = function
  | Leaf -> false
  | Branch(lt, k, _, rt) ->
    if key < k
    then kv_member k lt
    else if key > k
    then kv_member k rt
    else true

let rec lookup key = function
  | Leaf -> None
  | Branch(lt, k, v, rt) ->
    if key < k
    then lookup key lt
    else if key > k
    then lookup key rt
    else v
