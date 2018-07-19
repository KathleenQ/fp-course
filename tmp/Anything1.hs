module Anything1 where

-- 18th Jul 2018 [1st Day]
x :: Integer
x = 99

f :: Integer -> Integer
f a = a + 10

g :: Integer -> Integer -> Integer
g a b = (a + b)  * 2

(.+.) :: Integer -> (Integer -> Integer)
(.+.) a b = (a + b) * 2

h :: Integer -> Integer -> Integer
h = \a b -> (a + b) * 2

i :: (Integer -> Integer) -> Integer
i k = k 100

j :: (Integer -> a ) -> a
j k = k 100

z :: anything -> anything
z c = c

y :: a -> b -> a
y p _ = p

oo :: Integer -> Integer
oo n = n + 9393847448939

pie = 3
data Shape = Circle Integer
            | Rectangle Integer Integer
            | Triangle Integer Integer Integer
            deriving (Eq, Show)

perimeter :: Shape -> Integer
-- perimeter = \s -> 99
perimeter = \s -> case s of
  Circle r -> r * 2 * pie
  Rectangle w h -> (w + h) * 2
  Triangle a b c -> a + b + c

perimeter2 :: Shape -> Integer
perimeter2 (Circle r) = r * 2 * pie
perimeter2 (Rectangle w h) =(w + h) * 2
perimeter2 (Triangle a b c) = a + b + c

data Three a = T a a a
  deriving (Eq, Show)

multiply :: Three Integer -> Integer
multiply (T a b c) = a * b * c

m :: (a -> b) -> Three a -> Three b
m = \q -> \e -> case e of
  T a1 a2 a3 -> T (q a1) (q a2) (q a3)

isInList :: Eq a => a -> [a] -> Bool
isInList e list =
  case list of
    [] -> False
    (x : xs) -> (e == x) || isInList e xs

data List a =
  Nil | Cons a (List a)
  deriving (Eq, Show)

addList :: List Integer -> Integer
addList = \w -> case w of
  Nil ->      0
  Cons h t -> h + addList t

-- Some more review things:
-- in `map` form ~
mapOption :: (a -> b) -> Optional a -> Optional b
map       :: (a -> b) -> List a -> List b
?         :: (a -> b) -> ?    a -> ?    b
(.)       :: (a -> b) -> (t -> a) -> (t -> b)

m            :: (a -> b) -> Three a -> Three b
flatMap      :: (a -> List b) -> List a -> List b
bindOptional :: (a -> Optional b) -> Optional a -> Optional b