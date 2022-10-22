module AdderOp where

data OpType = Sum | Sub | Mult | Div | Pow | Sqrt deriving (Show)

data OpAssoc = Left | Right | Weak | None deriving (Show)

data OpExprType = Prefix | Postfix | Infix deriving (Show)

data OpArity = Unary | Binary | Multary deriving (Show)

arity :: OpArity -> Maybe Int
arity a = case a of
  Unary -> Just 1
  Binary -> Just 2
  Multary -> Nothing

data Op = Op {
  optype :: OpType,
  opassoc :: OpAssoc,
  opexprtype :: OpExprType,
  oparity :: OpArity,
  opprecedence :: Int,
  opcanonical :: String
} deriving (Show)

sum :: Op
sum = Op Sum AdderOp.Left Infix Multary 0 "+"

sub :: Op
sub = Op Sub AdderOp.Left Infix Binary 0 "-"

mult :: Op
mult = Op Mult AdderOp.Left Infix Multary 1 "*"

div :: Op
div = Op Div AdderOp.Left Infix Binary 1 "/"

pow :: Op
pow = Op Pow AdderOp.Right Infix Binary 2 "^"

sqrt :: Op
sqrt = Op Sqrt AdderOp.Right Prefix Unary 2 "sqrt"