
replaceThe :: String -> String
replaceThe s = unwords $ map (\x -> if x == "the" then "a" else x) $ words s


isFirstVowel :: String -> Bool
isFirstVowel (x:_)  = x `elem` "aeiou"
isFirstVowel _      = False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = countTheBeforeVowel' $ words s 
    where
        countTheBeforeVowel' []         = 0
        countTheBeforeVowel' [_]        = 0
        countTheBeforeVowel' (x:x':xs)  = countTheBeforeVowel' (x' : xs)
                                            + if x == "the" && isFirstVowel x' then 1 else 0



data Nat = Zero | Succ Nat deriving (Eq, Show)


natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ n)   = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0     = Nothing
    | n == 0    = Just Zero
    | otherwise = case integerToNat (n - 1) of 
        Just x  -> Just $ Succ x
        Nothing -> Nothing

-- Maybe lib

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a 


fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> acc ++ maybeToList x) []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if length xs' == length xs then Just xs' else Nothing 
    where xs' = catMaybes xs

-- Either lib

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> acc ++ lefts'' x) []
    where lefts'' :: Either a b -> [a]
          lefts'' (Left x)  = [x]
          lefts'' (Right _) = []
        

rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> acc ++ rights'' x) []
    where rights'' :: Either a b -> [b]
          rights'' (Left _)  = []
          rights'' (Right x) = [x]
        

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b


-- unfoldr

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
    Nothing   -> []
    Just(a, b') -> a : myUnfoldr f b'


betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just(f x, f x))

data BinaryTree a = 
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe(a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
    Nothing         -> Leaf
    Just (a', b, a'') -> Node (unfold f a') b (unfold f a'') 

treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold (\x -> if x <= 0 then Nothing else Just (x-1, x-1, x-1))


