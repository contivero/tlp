-- Cómo uso el context? Todos los ejemplos uso la lista vacia...
-- Y cómo hago un ejemplo de un Abs que no devuelva una constante,
-- por ejemplo un Abs de aplicación al estilo \x -> if x then true else false
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
tcheck g Fls = Just BoolT
tcheck g Tru = Just BoolT
tcheck g (If p a b) =
  case (tcheck g p) of
       Just BoolT -> case (tcheck g a) of
                          Just t -> case (tcheck g b) of
                                         Just t2 -> if t == t2
                                                       then Just t
                                                       else Nothing
                                         _ -> Nothing
                          _ -> Nothing
       _ -> Nothing
tcheck g (App t1 t2) =
  case (tcheck g t1) of
       Just (Arrow s t) -> case (tcheck g t2) of
                              Just u -> if u == s
                                           then Just t
                                           else Nothing
                              _ -> Nothing
       _ -> Nothing
tcheck g (Abs c tp tm) =
  case tp of
       Arrow s t -> case (tcheck g tm) of
                         Just tmType -> if t == tmType
                                           then Just tp
                                           else Nothing
                         _ -> Nothing
       _ -> Nothing

example0 = tcheck [] Tru
example1 = tcheck [] (App Tru Tru)
example2 = tcheck [] (If Tru Fls Tru)
example3 = tcheck [] (Abs 'x' (Arrow (Arrow BoolT BoolT) BoolT) Tru)
example4 = tcheck [] (App (Abs 'x' (Arrow BoolT BoolT) Tru) Fls)
example5 = tcheck [] (If (Abs 'c' (Arrow BoolT BoolT) Tru) Tru Fls)
