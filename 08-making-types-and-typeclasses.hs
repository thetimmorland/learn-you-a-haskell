import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surfaceArea :: Shape -> Float
surfaceArea (Circle _ r) = pi * r ^ 2
surfaceArea (Rectangle (Point x1 y1) (Point x2 y2)) = abs $ (x1 - x2) * (y1 - y2)

data Vector a = Vector a a a deriving (Show, Eq)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

vmul :: (Num t) => Vector t -> Vector t -> Vector t
(Vector x1 y1 z1) `vmul` (Vector x2 y2 z2) = Vector (x1 * x2) (y1 * y2) (z1 * z2)

smul :: (Num t) => Vector t -> t -> Vector t
(Vector x y z) `smul` a = Vector (x * a) (y * a) (z * a)


data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of -- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a 
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ "doesn't exist"
        Just (state, code) -> if state /= Taken
            then Right code
            else Left $ "Locker " ++ show lockerNumber ++ "is already taken"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node a left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

tree :: Tree Int
tree = foldr treeInsert EmptyTree [8, 6, 4, 1, 7, 3, 5]


class Truthy a where
    truthy :: a -> Bool

instance Truthy Int where
    truthy 0 = False
    truthy _ = True

instance Truthy [a] where
    truthy [] = False
    truthy _ = True
    
instance Truthy Bool where
    truthy = id