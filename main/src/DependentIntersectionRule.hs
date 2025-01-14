data Type = TypeVar String | DependentTypeIntersection Type (Term -> Type) -- deriving Show
data Term = Var String | App Term Term deriving Show
data Context = EmptyContext | Extend Context String Type -- deriving Show

-- ^Paul: Haskell doesn't know how to `Show` functions, so `deriving
-- Show` doesn't work on `Type` (which causes it to fail for `Context`
-- since it contains un`Show`able `Type`s, too).

-- Typing judgment
(|-) :: Context -> Term -> Type -> Bool
(|-) EmptyContext (Var _) _ = False
(|-) (Extend ctx x a) (Var y) t = if x == y then ctx |- a : t else ctx |- Var y : t
(|-) ctx m (DependentTypeIntersection a f) =
    case (ctx |- m) a of
        True -> (ctx |- m) (f m)
        _    -> False

-- Dependent Intersection introduction rule
dependentIntersectionIntroduction :: Context -> Type -> Context -> (Term -> Type) -> Bool
dependentIntersectionIntroduction gamma1 a gamma2 b =
    case (gamma1, a, gamma2) of
        (gamma1', TypeVar _, gamma2') ->  -- Ensuring A is a valid type
            gamma1' == gamma2' &&
            gamma1' |- a : Type &&
            gamma2' |- b (Var "x") : Type
        _ -> False

-- Dependent Intersection elimination rule for A
dependentIntersectionEliminationA :: Context -> Term -> Type -> Bool
dependentIntersectionEliminationA ctx m a =
    case ctx |- m : DependentTypeIntersection (\x -> a) of
        True -> ctx |- m : a
        _    -> False

-- Dependent Intersection elimination rule for B
dependentIntersectionEliminationB :: Context -> Term -> (Term -> Type) -> Bool
dependentIntersectionEliminationB ctx m b =
    case ctx |- m : DependentTypeIntersection (\x -> b x) of
        True -> ctx |- m : b m
        _    -> False

-- Example usage
main :: IO ()
main = do
    let gamma = Extend EmptyContext "x" (TypeVar "A")
        m = Var "M"
        n = Var "N"

    putStrLn "Dependent Intersection typing rules:"
    -- Dependent Intersection introduction
    putStrLn $ "Result 1: " ++ show (dependentIntersectionIntroduction gamma (TypeVar "A") gamma (\x -> TypeVar "B"))

    -- Dependent Intersection elimination for A
    putStrLn $ "Result 2: " ++ show (dependentIntersectionEliminationA gamma m (TypeVar "A"))

    -- Dependent Intersection elimination for B
    putStrLn $ "Result 3: " ++ show (dependentIntersectionEliminationB gamma n (\x -> TypeVar "B"))
