data Type = TypeVar String | TypeIntersection Type Type deriving Show
data Term = Var String | App Term Term deriving Show
data Context = EmptyContext | Extend Context String Type deriving Show

-- Typing judgment
(|-) :: Context -> Term -> Type -> Bool
(EmptyContext |- Var _) _ = False
(Extend ctx x a |- Var y) t = if x == y then a <: t else (ctx |- Var y) t
(ctx |- m) (TypeIntersection a b) =
    (ctx |- m) a && (ctx |- m) b

-- Subtyping judgement
(<:) :: Type -> Type -> Bool
TypeVar a <: TypeVar b = a == b
c <: TypeIntersection a b = c <: a && c <: b -- Paul: Be careful here...
TypeIntersection a b <: c = a <: c || b <: c -- Order of these lines matters!

-- Substitution
substitute :: Term -> String -> Term -> Term
substitute (Var y) x n = if x == y then n else Var y
substitute (App m1 m2) x n = App (substitute m1 x n) (substitute m2 x n)

-- Type elimination rule
typeElimination :: Context -> Term -> Type -> Bool
typeElimination ctx m a =
    case (ctx |- m) (TypeIntersection a _) of -- Paul: What is the `_`?
        True -> (ctx |- m) a
        _    -> False

-- Type introduction rule
typeIntroduction :: Context -> Term -> Type -> Bool
typeIntroduction ctx m (TypeIntersection a b) =
    (ctx |- m) a && (ctx |- m) b

-- ^Paul: Not sure the purpose of these two functions,
-- `typeElimination` and `typeIntroduction`.  Why are they in separate
-- functions, rather than in the type-checker algorithm (|-)?

-- usage
main :: IO ()
main = do
    let gamma = Extend EmptyContext "x" (TypeVar "A")
        m = Var "M"
        n = Var "N"
    
    putStrLn "Intersection typing rules:"
    
    -- Type elimination and introduction rules for intersection types
    putStrLn $ "Result 1: " ++ show (typeElimination gamma m (TypeVar "A") && typeElimination gamma m (TypeIntersection (TypeVar "A") (TypeVar "B")))
    putStrLn $ "Result 2: " ++ show (typeIntroduction gamma m (TypeIntersection (TypeVar "A") (TypeVar "B")))
    
    -- Intersection introduction rule
    putStrLn $ "Result 3: " ++ show ((gamma |- m) (TypeVar "A") && (Extend gamma "x" (TypeVar "A") |- n) (TypeVar "B") && (gamma |- n) (TypeIntersection (TypeVar "A") (TypeVar "B")))
    
    -- Intersection elimination rule
    putStrLn $ "Result 4: " ++ show ((gamma |- m) (TypeIntersection (TypeVar "A") (TypeVar "B")) && (gamma |- m) (TypeVar "A"))
    putStrLn $ "Result 5: " ++ show ((gamma |- m) (TypeIntersection (TypeVar "A") (TypeVar "B")) && (gamma |- m) (TypeVar "B"))
    
    -- Incorrect cases
    putStrLn $ "Result 6: " ++ show (typeIntroduction gamma m (TypeVar "A"))  -- Should fail
    putStrLn $ "Result 7: " ++ show (typeElimination gamma m (TypeVar "B"))  -- Should fail
    putStrLn $ "Result 8: " ++ show (typeElimination gamma m (TypeIntersection (TypeVar "A") (TypeVar "C")))  -- Should fail
