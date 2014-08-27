-- CheckerV2 + Trees
type Context = [(Char, Type)]
data Term = Tru
          | Fls
          | Var Char
          | If Term Term Term
          | App Term Term
          | Abs Char Type Term
          | Empty Type
          | Node Term Term Term
  deriving (Eq, Show)

data Type = BoolT
          | Arrow Type Type
          | Tree Type
  deriving (Eq, Show)

tcheck :: Context -> Term -> Maybe Type
tcheck g Fls = return BoolT
tcheck g Tru = return BoolT
tcheck g (Var c) = lkup g c
  where lkup [] _ = Nothing
        lkup (x:xs) c = if (fst x) == c
                           then Just (snd x)
                           else lkup xs c
tcheck g (If p a b) = do BoolT <- tcheck g p
                         t1    <- tcheck g a
                         t2    <- tcheck g b
                         if t1 == t2
                            then return t1
                            else fail "Nothing"
tcheck g (App t1 t2) = do Arrow s t <- tcheck g t1
                          e         <- tcheck g t2
                          if e == s
                             then return t
                             else Nothing
tcheck g (Abs c tp tm) = do Arrow s t1 <- return tp
                            t2         <- tcheck g tm
                            if t1 == t2
                               then return tp
                               else Nothing
tcheck g (Empty t) = return (Tree t)
tcheck g (Node e l r) = do t <- tcheck g e
                           Tree a <- tcheck g l
                           Tree b <- tcheck g r
                           if t == a && a == b
                              then return (Tree t)
                              else Nothing

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

-- Nothing
example5 = tcheck [] (If (Abs 'c' (Arrow BoolT BoolT) Tru) Tru Fls)

-- Just BoolT
example6 = tcheck [('x', BoolT)] (App (Abs 'x' (Arrow BoolT BoolT) (If (Var 'x') Tru Fls)) Tru)

-- Just (Tree BoolT)
exampleTree1 = tcheck [] (Node Tru (Empty BoolT) (Empty BoolT))

-- Just (Tree (Arrow BoolT BoolT))
exampleTree2 = tcheck [('x', BoolT)] (Node (Abs 'x' (Arrow BoolT BoolT) (If (Var 'x') Tru Fls)) (Empty (Arrow BoolT BoolT)) (Empty (Arrow BoolT BoolT)))
