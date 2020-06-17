{-
expr  = Const( float  val  )
    | Var  ( string name )
    | Add  ( expr lhs, expr rhs )
    | Mul  ( expr lhs, expr rhs )
    | Let  ( string name, expr rhs, expr body )
-}

data Expr
  = Const Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval (Const x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

main :: IO ()
main = do
--  input <- getContents
  let input = Add (Mul (Const 5) (Const 2)) (Const 1)
  let output = eval input
  print output
