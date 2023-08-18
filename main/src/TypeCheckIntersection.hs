-- Define the types in the type system
data Type
  = TyInt
  | TyBool
  | TyArrow Type Type  -- Function type
  | TyIntersect Type Type  -- Intersection type

-- Define the expressions in the language
data Expr
  = EVar String
  | EApp Expr Expr
  | EAbs String Type Expr  -- Lambda abstraction
  | EBool Bool
  | EInt Int
  | EIf Expr Expr Expr  -- Conditional expression
  | EIntersect Expr Expr  -- Intersection type expression

-- A type environment maps variable names to their types
type Environment = [(String, Type)]

-- Type inference function
inferType :: Environment -> Expr -> Maybe Type
inferType env (EVar x) = lookup x env
inferType env (EApp e1 e2) = do
  TyArrow argType resultType <- inferType env e1
  argType' <- inferType env e2
  if argType == argType' then Just resultType else Nothing
inferType env (EAbs x argType body) = do
  resultType <- inferType ((x, argType) : env) body
  Just (TyArrow argType resultType)
inferType _ (EBool _) = Just TyBool
inferType _ (EInt _) = Just TyInt
inferType env (EIf cond thenExpr elseExpr) =
  case (inferType env cond, inferType env thenExpr, inferType env elseExpr) of
    (Just TyBool, Just t1, Just t2) | t1 == t2 -> Just t1
    _ -> Nothing
inferType env (EIntersect e1 e2) = do
  t1 <- inferType env e1
  t2 <- inferType env e2
  case (t1, t2) of
    (TyIntersect t1' t2', _) -> if t1' == t2' then Just t1' else Nothing
    (_, TyIntersect t1' t2') -> if t1' == t2' then Just t2' else Nothing
    (_, _) -> Nothing

-- Type checking function
checkType :: Environment -> Expr -> Type -> Bool
checkType env (EVar x) expected = case lookup x env of
  Just t -> t == expected
  Nothing -> False
checkType env (EApp e1 e2) expected = case inferType env e1 of
  Just (TyArrow argType resultType) ->
    checkType env e2 argType && resultType == expected
  _ -> False
checkType env (EAbs x argType body) (TyArrow expectedArgType resultType) =
  checkType ((x, argType) : env) body resultType
checkType _ (EBool _) TyBool = True
checkType _ (EInt _) TyInt = True
checkType env (EIf cond thenExpr elseExpr) expected =
  checkType env cond TyBool
    && checkType env thenExpr expected
    && checkType env elseExpr expected
checkType env (EIntersect e1 e2) expected = case (inferType env e1, inferType env e2) of
  (Just (TyIntersect t1' t2'), _) -> t1' == expected && t2' == expected
  (_, Just (TyIntersect t1' t2')) -> t1' == expected && t2' == expected
  (_, _) -> False
