module Anything3 where
-- 20th Jul 2018 [3rd Day]

-- Course "CSI 194. Introduction to Haskell" ~~
-- @ DATA61 QFPL team  (http://feedback.course.qfpl.io/) "Some more Qs"~~~

-- Lazy evaluation! (Using Java, C to show the examples)


class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

-- mappend x mempty ---> x
-- mappend mempty x ---> x
-- mappend (mappend x y) z  <->  mappend x (mappend y z)

instance Monoid (List a) where
   mappend = (++) -- x ++ (y ++ z) <-> (x ++ y) ++ z
   mempty = Nil

instance Monoid (Optional a) where
   mempty = Empty
   mappend Empty a = a
   mappend a Empty = a
   mappend (Full x) (Full y) = Full (x `mappend` y)

data Endo a = Endo (a -> a)

instance Monoid (Endo a) where
   mappend (Endo f) (Endo g) = Endo (f . g)
   mempty = Endo id
-- id.x = x; x.id = id

data Sum = Sum Int

instance Monoid Sum where
   mempty = Sum 0
   mappend (Sum x) (Sum y) = Sum (x + y)

foldMap :: blah
foldMap Nil _ = mempty
foldMap (x :. xs) f = f x `mappend` foldMap xs

sum xs =
   case foldMap xs Sum of
     Sum result -> result

fold :: Monoid m => List m -> m
fold xs = foldMap xs id

composed :: Int -> Int
composed = case foldMap [(+1), (*2), substract 3] Endo of Endo f -> f


