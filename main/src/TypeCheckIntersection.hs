-- Define the types in the type system
data Type
  = TyInt
  | TyBool
  | TyArrow Type Type  -- Function type
  | TyIntersect Type Type  -- Intersection type
  deriving Eq
  -- ^Paul: Added the `deriving Eq` clause as a hack to compile this
  -- code, since `inferType` and `checkType` below uses equality on
  -- types to check if two types are the same.  There will almost
  -- certainly be two types that we expect to be the same, but that
  -- the derived equality operation will consider different, causing
  -- bugs where the type checker rejects programs it should allow.
  -- This is why the main pi-forall typechecker uses a more
  -- sophisticated notion of equality between types that has to be
  -- manually implemented.

-- Define the expressions in the language
data Expr
  = EVar String
  | EApp Expr Expr
  | EAbs String Expr  -- Lambda abstraction
  -- ^Paul Removed the type annotation on function arguments, these
  -- will be covered by the generic type annotation ETyAnn below
  | EBool Bool
  | EInt Int
  | EIf Expr Expr Expr  -- Conditional expression
  -- | EIntersect Expr Expr  -- Intersection type expression
  -- ^Paul: Intersection types don't have their own syntax in Expr
  | ETyAnn Expr Type
  -- ^Paul: Added type-annotated expressions, used in bi-directional
  -- typing to convert a type-checked expression to a type-inferred
  -- expression

-- A type environment maps variable names to their types
type Environment = [(String, Type)]

-- Paul: One goal of bi-directional type checking: instead of checking
-- type equalities all over the place, type equality is only checked
-- in *one* place: the case where we check the type of an inferrable
-- expression.

-- Paul: Notice that inferType needs to be driven by the `Expr`
-- argument.  In cases where there is no constructor of `Expr`
-- associated with a type (like intersection types), or where the
-- constructor doesn't have enough information to immediately infer a
-- type (like with the form `EAbs x e` standing for `\x -> e`), we
-- just give up.  Instead, for those cases, we expect the programmer
-- to add an explicit type annotation into the program, such as
-- `ETyAnn (EAbs x e) (TyArrow a b)` that will tell us what type to
-- expect.

-- Type inference function
inferType :: Environment -> Expr -> Maybe Type
inferType env (EVar x) = lookup x env
inferType env (EApp e1 e2) = do
  TyArrow argType resultType <- inferType env e1
  if checkType env e2 argType -- Paul: check the type of the argument, rather than infer
    then Just resultType
    else Nothing

-- inferType env (EAbs x argType body) = do
--  resultType <- inferType ((x, argType) : env) body
--  Just (TyArrow argType resultType)

-- ^Paul: Functions need to be type checked, so don't try to infer the type

inferType _ (EBool _) = Just TyBool
inferType _ (EInt _) = Just TyInt

-- inferType env (EIf cond thenExpr elseExpr) =
--  case (inferType env cond, inferType env thenExpr, inferType env elseExpr) of
--    (Just TyBool, Just t1, Just t2) | t1 == t2 -> Just t1
--    _ -> Nothing

-- ^Paul: If-then-else is easier to type check, rather than infer, so
-- that we are given the shared type that both the then and else
-- branches are expected to be

-- inferType env (EIntersect e1 e2) = do
--  t1 <- inferType env e1
--  t2 <- inferType env e2
--  case (t1, t2) of
--    (TyIntersect t1' t2', _) -> if t1' == t2' then Just t1' else Nothing
--    (_, TyIntersect t1' t2') -> if t1' == t2' then Just t2' else Nothing
--    (_, _) -> Nothing

-- ^Paul: We definitely have no hope of inferring an intersection type!

inferType env (ETyAnn e t) =
  if checkType env e t
  then Just t
  else Nothing

-- ^Paul: If the programmer writes a type annotation into their
-- program, then use that to check the expression.  If checking
-- succeeds, we infer the written type.  If checking fails, then type
-- inference fails.

inferType _ _ = Nothing

-- ^Paul: All other cases (that are to difficult to infer
-- immediately), just give up, and tell the programmer to write a type
-- annotation to tell us what to do.

-- Paul: In contrast to `inferType`, `checkType` is primarily driven
-- by the given `Type` argument.  In some cases (like function types),
-- `checkType` might also consult the given `Expr` to find a match.
-- In other cases (like intersection types), `checkType` doesn't even
-- care what the `Expr` looks like.  Notice that we will avoid
-- redundancy, so that any case covered already by `inferType` will
-- not be repeated in `checkType`.

-- Type checking function
checkType :: Environment -> Expr -> Type -> Bool
-- checkType env (EVar x) expected = case lookup x env of
--  Just t -> t == expected
--  Nothing -> False

-- checkType env (EApp e1 e2) expected = case inferType env e1 of
--   Just (TyArrow argType resultType) ->
--     checkType env e2 argType && resultType == expected
--   _ -> False

-- checkType _ (EBool _) TyBool = True
-- checkType _ (EInt _) TyInt = True

-- ^Paul: Skip, already covered by `inferType`

checkType env (EAbs x body) (TyArrow argType resultType) =
  checkType ((x, argType) : env) body resultType

-- ^Paul: Notice how skipping the argument type annotation in `EAbs`
-- makes type checking easier for functions, since we will already be
-- told what argument type to expect.  Now, we don't have to check
-- that our given expectations match what is written in the program.

checkType env (EIf cond thenExpr elseExpr) expected =
  checkType env cond TyBool
    && checkType env thenExpr expected
    && checkType env elseExpr expected

checkType env anything (TyIntersect ty1 ty2) =
  checkType env anything ty1 && checkType env anything ty2

-- ^Paul: To check that any expression belongs to an intersection of
-- two types, just check that it belongs to both types separately.
-- There's nothing else to look for in this case.

checkType env inferable expected =
  case inferType env inferable of
    Nothing -> False
    Just inferred -> inferred <: expected

-- ^Paul: In all other cases, we will just resort to inferring the
-- type for the given expression.  This will either fail, or succeed
-- and give us a potentially different type than what we expected; so
-- we will have to check that the expected type is a (possible more
-- specific) special case of the expressions inferred type.


-- Subtyping `a <: b`.  Checking that `b` is a special case of some
-- more general `a`.

(<:) :: Type -> Type -> Bool
TyInt  <: TyInt = True
TyBool <: TyBool = True

TyArrow a b <: TyArrow a' b' = a' <: a && b <: b'
-- ^Paul: Function subtyping flips the order of argument types!

c <: TyIntersect a b = c <: a && c <: b
TyIntersect a b <: c = a <: c || b <: c

_ <: _ = False
