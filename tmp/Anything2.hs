module Anything2 where

-- 19th Jul 2018 [2nd Day]
class Equal x where
  isEqual :: x -> x -> Bool

instance Equal TwoInts where
-- isEqual :: TwoInts -> Two Ints -> Bool
  isEqual (TI a1 b1) (TI a2 b2)
    = (a1 == a2) && (b1 == b2)

instance Equal Bool where
  isEqual True True = True
  isEqual False True = False
  isEqual True False = False
  isEqual False False = False

data TwoInts = TwoInts Int Int
  deriving Show

isInList :: Equal a => a -> [a] -> Bool
isInList _ [] = False
isInList e (h:t) = (isEqual e h) || isInList e t

-- substitute every element in the list to be the input element
amapList :: b -> List a -> List b
amapList b x = map (\_ -> b) x

amapOptional :: b -> Optional a -> Optional b
amapOptional b x = amapOptional (\_ -> b) x

-- >>> amapRead 7 (+2) 88
-- 7
amapRead :: b -> (t -> a) -> (t -> b)
amapRead b x = (.) (\_ -> b)


class ThingsThatMap k where
  howtodomap :: (a -> b) -> k a -> k b

instance ThingsThatMap List where
  -- (a -> b) -> List a -> List b
  howtodomap f = foldRight ((:.) . f) Nil

-- (->) :: TYPE q -> TYPE r -> *  [:kind (->)]
-- (->) Int :: * -> *
instance ThingsThatMap ((->) t) where
  -- (a -> b) -> (t -> a) -> (t -> b)
  howtodomap a2b t2a = \t -> a2b (t2a t)

amapAnything :: ThingsThatMap k => b -> k a -> k b
amapAnything b x = howtodomap (\_ -> b) x

