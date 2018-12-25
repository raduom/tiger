module Line where

import Data.List (find)
import Data.Maybe (fromJust)

type Id = String

data BinOp = Plus | Minus | Times | Div deriving (Show, Eq)

data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp
         deriving (Show, Eq)

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]
         deriving (Show, Eq)

data St = St
  { symbolTable :: [(Id, Int)]
  , output      :: [Int]
  } deriving (Show, Eq)

prog :: Stm
prog = CompoundStm (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
                   (CompoundStm (AssignStm "b"
                                           (EseqExp (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                                                    (OpExp (NumExp 10) Times (IdExp "a"))))
                                (PrintStm [IdExp "b"]))

maxargs :: Stm -> Int
maxargs (CompoundStm sl sr) = maxargs sl + maxargs sr
maxargs (AssignStm _ e)     = maxargsE e
maxargs (PrintStm es)       = foldl (\m e -> max m (maxargsE e)) (length es) es

maxargsE :: Exp -> Int
maxargsE (EseqExp s e) = max (maxargs s) (maxargsE e)
maxargsE _ = 0

evalS :: Stm -> St -> St
evalS (CompoundStm sl sr) st = (evalS sr . evalS sl) st
evalS (AssignStm label e) st =
  let (st', v) = evalE e st
  in  st' { symbolTable = (label, v) : symbolTable st' }
evalS (PrintStm es) st =
  foldl appendOut st es
  where
    appendOut :: St -> Exp -> St
    appendOut st' e =
      let out = output st
          (st'', v) = evalE e st'
      in  st'' { output = v : out }

evalE :: Exp -> St -> (St, Int)
evalE (IdExp l) st     = (st, fromJust $ snd <$> find ((== l) . fst) (symbolTable st))
evalE (NumExp n) st    = (st, n)
evalE (OpExp el op er) st =
  let (st', vl)        = evalE el st
      (st'', vr)       = evalE er st'
  in  (st'', runOp op vl vr)
evalE (EseqExp s e) st = evalE e (evalS s st)

runOp :: BinOp -> Int -> Int -> Int
runOp Plus v1 v2  = v1 + v2
runOp Minus v1 v2 = v1 - v2
runOp Times v1 v2 = v1 * v2
runOp Div v1 v2   = v1 `div` v2

{- Exercise 1.1 -}

type Key = String

data Tree = Leaf
          | Branch Tree Key Tree
          deriving (Show, Eq)

empty :: Tree
empty = Leaf

insert :: Key -> Tree -> Tree
insert k Leaf = Branch Leaf k Leaf
insert k t@(Branch lt ck rt)
  | k < ck = Branch (insert k lt) ck rt
  | k > ck = Branch lt ck (insert k rt)
  | otherwise = t

member :: Key -> Tree -> Bool
member _ Leaf = False
member k (Branch lt ck rt)
  | k < ck = member k lt
  | k > ck = member k rt
  | otherwise = True

data KVTree a = KVLeaf
              | KVBranch (KVTree a) Key a (KVTree a)
              deriving (Show, Eq)

kvEmpty :: KVTree a
kvEmpty = KVLeaf

kvInsert :: Key -> a -> KVTree a -> KVTree a
kvInsert key value KVLeaf = KVBranch KVLeaf key value KVLeaf
kvInsert key value tree@(KVBranch lt k v rt)
  | key < k = KVBranch (kvInsert key value lt) k v rt
  | key > k = KVBranch lt k v (kvInsert key value rt)
  | otherwise = tree

kvLookup :: Key -> KVTree a -> Maybe a
kvLookup _ KVLeaf = Nothing
kvLookup key (KVBranch lt k v rt)
  | key < k = kvLookup key lt
  | key > k = kvLookup key rt
  | otherwise = Just v
