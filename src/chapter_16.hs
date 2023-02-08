



replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace


twiceLifted :: (Functor f1, Functor f) =>
               f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted


thriceLifted :: (Functor f2, Functor f1, Functor f) =>
                f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP


thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted
-- f ~ []


instance Functor (Two a) where fmap f (Two a b) = Two a (f b)

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where fmap _ (First a) = (First a); fmap f (Second b) = Second (f b)



newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)
instance Functor (Constant m) where fmap _ (Constant v) = Constant v


data Wrap f a = Wrap (f a) deriving (Eq, Show)




instance Functor (Wrap f) where fmap f (Wrap fa) = Wrap (f fa)
instance Functor (Wrap f) where fmap f (Wrap fa) = Wrap (fmap f fa)
instance Functor f => Functor (Wrap f) where fmap f (Wrap a) = Wrap (fmap f a)


