--Definiciones
data TreeEx d = Null | Node d [TreeEx d] deriving(Show, Eq)
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
    
type Estado = [String]

--Árbol de Sintaxis Abstracta
--1
toTree :: Prop -> TreeEx String
toTree (Cons b) = Node (boolToString b) []
toTree (Var p) = Node p []
toTree (Not p) = Node "Not" [(toTree p)]
toTree (And p q) = Node "And" [(toTree p),(toTree q)]
toTree (Or p q) = Node "Or" [(toTree p),(toTree q)]
toTree (Impl p q) = Node "Impl" [(toTree p),(toTree q)]
toTree (Syss p q) = Node "Syss" [(toTree p),(toTree q)]

--2
toProp :: TreeEx String -> Prop 
toProp (Node "True" [] ) = Cons True
toProp (Node "False" [] ) = Cons False
toProp (Node "Not" (x:xs)) = Not (toProp x)
toProp (Node "And" (x:y:xs)) = And (toProp x) (toProp y)
toProp (Node "Or" (x:y:xs)) = Or (toProp x) (toProp y)
toProp (Node "Impl" (x:y:xs)) = Impl (toProp x) (toProp y)
toProp (Node "Syss" (x:y:xs)) = Syss (toProp x) (toProp y)
toProp (Node p [] ) = Var p

--3                             
checkTree :: TreeEx String -> Estado -> Bool
checkTree (Node "True" []) l = True 
checkTree (Node "False" []) l = False
checkTree (Node "Not" (x:xs)) l = not (checkTree x l)
checkTree (Node "And" (x:y:xs)) l = (checkTree x l) && (checkTree y l)
checkTree (Node "Or" (x:y:xs)) l = (checkTree x l) || (checkTree y l)
checkTree (Node "Impl" (x:y:xs)) l = (not (checkTree x l)) || (checkTree y l)
checkTree (Node "Syss" (x:y:xs)) l = (checkTree x l) == (checkTree y l)
checkTree (Node p []) list = include p list

--Otras Funciones
--1
cantidadElementos :: TreeEx d -> Int
cantidadElementos (Node d []) = 1
cantidadElementos (Node d lst )  = 1 + itera lst
        where itera :: [TreeEx d] -> Int 
            itera (x:xs) = cantidadElementos x + itera xs
            itera [] = 0

--2
busca ::(Eq d)=> TreeEx d -> d -> Bool
busca (Node d []) f = f == d
busca (Node d (x:xs)) f = not(not(d == f) && (busca x f) && (itera xs f))
    where itera :: (Eq d) => [TreeEx d] -> d -> Bool
        itera (x:xs) f = (busca x f) && (itera xs f)
        itera [] _ = True

--3
sumaElementos :: TreeEx Int -> Int
sumaElementos (Node n []) = n
sumaElementos (Node n (x:xs)) =  n + (sumaElementos x) + (itera xs)
    where itera :: [TreeEx Int] -> Int
        itera (x:xs) = (sumaElementos x) + (itera xs)
        itera [] = 0

--4
preOrden :: TreeEx a -> [a]
preOrden Null = []
preOrden (Node d lst) = d : iterar lst
  where
    iterar :: [TreeEx a] -> [a]
    iterar [] = []
    iterar (x:xs) = preOrden x ++ iterar xs

--5
altura :: TreeEx d -> Int
altura (Node _ []) = 1
altura (Node _ (x:xs)) =
    1 + max (altura x) (alturaLista xs)
    where
        alturaLista [] = 0
        alturaLista (y:ys) = max (altura y) (alturaLista ys)

--6
espejo :: TreeEx d -> TreeEx d
espejo (Node d lst) = Node d (inv lst)
    where
        inv [] = []
        inv (x:xs) = inv xs ++ [espejo x]

-- 7
podar :: TreeEx d -> Int -> TreeEx d
podar (Node d _) 0 = Node d []
podar (Node d lst) n = Node d (p lst)
  where
    p [] = []
    p (x:xs) = podar x (n-1) : p xs

-- 8
elementosProfundidad :: TreeEx d -> Int -> [d]
elementosProfundidad (Node d _) 0 = [d]
elementosProfundidad (Node _ lst) n = iterar lst
  where
    iterar [] = []
    iterar (x:xs) = elementosProfundidad x (n-1) ++ iterar xs

-- Funciones auxiliares 
boolToString :: Bool -> String
boolToString True = "True"
boolToString False = "False"

include :: (Eq a) => a -> [a] -> Bool
include _ [] = False
include elem (head : sublist)
    | elem == head = True
    | otherwise    = include elem sublist

max :: Ord a => a -> a -> a
max a b
    | a >= b    = a
    | otherwise = b


