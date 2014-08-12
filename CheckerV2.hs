type Context = [(Char, Type)]
data Term = Tru
          | Fls
          | If Term Term Term
          | App Term Term
          | Abs Char Type Term
  deriving (Eq, Show)

data Type = Arrow Type Type
          | BoolT
  deriving (Eq, Show)

tcheck :: Context -> Term -> Maybe Type
tcheck g Fls = return BoolT
tcheck g Tru = return BoolT
tcheck g (If p a b) = do tcheck g p
                         t1 <- tcheck g a
                         t2 <- tcheck g b
                         if t1 == t2
                            then return t1
                            else fail "Nothing"
tcheck g (App t1 t2) =   case (tcheck g t1) of
       Just (Arrow s t) -> case (tcheck g t2) of
                              Just u -> if u == s
                                           then Just t
                                           else Nothing
                              _ -> Nothing
       _ -> Nothing
-- Tengo que chequear que sea un Arrow el Just, no cualquier Just
-- Cómo hago eso en la do-notation?
{-
  do e1 <- tcheck g t1
     e2 <- tcheck g t2
     if e1 == e2
     then return
     else fail "Nothing"
 -}
tcheck g (Abs c tp tm) =
  case tp of
       Arrow s t -> case (tcheck g tm) of
                         Just tmType -> if t == tmType
                                           then Just tp
                                           else Nothing
                         _ -> Nothing
       _ -> Nothing

-- Just BoolT
example0 = tcheck [] Tru                            

-- Nothing
example1 = tcheck [] (App Tru Tru)

-- Just BoolT
example2 = tcheck [] (If Tru Fls Tru)

-- Just (Arrow (Arrow BoolT BoolT) BoolT)
example3 = tcheck [] (Abs 'x' (Arrow (Arrow BoolT BoolT) BoolT) Tru) 

-- Just BoolT
example4 = tcheck [] (App (Abs 'x' (Arrow BoolT BoolT) Tru) Fls) 

example5 = tcheck [] (If (Abs 'c' (Arrow BoolT BoolT) Tru) Tru Fls)
