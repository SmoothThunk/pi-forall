data Type = TypeVar String | TypeIntersection Type Type deriving Show
data Term = Var String | App Term Term deriving Show
data Context = EmptyContext | Extend Context String Type deriving Show

-- Typing judgment
(|-) :: Context -> Term -> Type -> Bool
(|-) EmptyContext (Var _) _ = False
(|-) (Extend ctx x a) (Var y) t = if x == y then ctx |- a : t else ctx |- Var y : t
(|-) ctx (App m1 m2) t =
    case ctx |- m1 : TypeIntersection a b of
        True -> ctx |- m2 : a
        _    -> False
  where
    TypeIntersection a b = t
(|-) ctx m (TypeIntersection a b) =
    ctx |- m : a && Extend ctx "x" a |- m : b

-- Substitution
substitute :: Term -> String -> Term -> Term
substitute (Var y) x n = if x == y then n else Var y
substitute (App m1 m2) x n = App (substitute m1 x n) (substitute m2 x n)

-- Type elimination rule
typeElimination :: Context -> Term -> Type -> Bool
typeElimination ctx m a =
    case ctx |- m : TypeIntersection a _ of
        True -> ctx |- m : a
        _    -> False

-- Type introduction rule
typeIntroduction :: Context -> Term -> Type -> Bool
typeIntroduction ctx m (TypeIntersection a b) =
    ctx |- m : a && Extend ctx "x" a |- substitute b "x" m : b

-- usage
main :: IO ()
main = do
    let gamma = Extend EmptyContext "x" (TypeVar "A")
        m = Var "M"

    putStrLn "Intersection typing rules:"
    -- Applying type elimination and introduction rules
    putStrLn $ "Result 1: " ++ show (typeElimination gamma m (TypeVar "A") && typeElimination gamma m (TypeIntersection (TypeVar "A") (TypeVar "B")))
    putStrLn $ "Result 2: " ++ show (typeIntroduction gamma m (TypeIntersection (TypeVar "A") (TypeVar "B")))
    -- Checking type introduction and elimination together
    putStrLn $ "Result 3: " ++ show (gamma |- m : TypeVar "A" && Extend gamma "x" (TypeVar "A") |- m : TypeVar "B")
