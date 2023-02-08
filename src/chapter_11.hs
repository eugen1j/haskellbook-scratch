import Data.Char

-- Cell phone keyboard

data DaPhone = DaPhone 

convo :: [String]
convo = 
    ["Wanna play 20 questions"
    , "Haha thanks"
    , "Ok. play Lol"]

type Digit = Char

type Presses = Int

data PhoneButton = PhoneButton {digit :: Digit, letters :: String}

indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = indexOf' xs 0
    where 
        indexOf'  [] _       = -1
        indexOf'  (x':xs') idx  = case x == x' of
            True  -> idx
            False -> indexOf' xs' (idx + 1)


phoneButtons :: DaPhone -> [PhoneButton] 
phoneButtons DaPhone = [ PhoneButton '1' "1"
                       , PhoneButton '2' "abc2"
                       , PhoneButton '3' "def3"
                       , PhoneButton '4' "ghi4"
                       , PhoneButton '5' "jkl5"
                       , PhoneButton '6' "mno6"
                       , PhoneButton '7' "pqrs7"
                       , PhoneButton '8' "tuv8"
                       , PhoneButton '9' "wxyz9"
                       , PhoneButton '0' " 0"
                       , PhoneButton '#' ".,"
                       ]

shiftButton :: DaPhone -> PhoneButton
shiftButton DaPhone = PhoneButton '*' ""

findButton :: DaPhone -> Char -> [PhoneButton]
findButton ph c = filter (\b -> elem c $ letters b) $ phoneButtons ph

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps ph c 
    | isUpper c = [(digit $ shiftButton ph, 1)] ++ (reverseTaps ph $ toLower c)
    | otherwise = case findButton ph c of
        []    -> []
        (b:_) -> [(digit b, (+) 1 $ indexOf c $ letters b)]


flatten :: [[a]] -> [a]
flatten = foldr (++) []

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f = flatten . (map f)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead ph = flatMap $ reverseTaps ph

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map (\(_, p) -> p)


count :: Eq a => a -> [a] -> Int
count el = length . filter (== el)

maxOn :: Ord a => (b -> a) -> b -> b -> b
maxOn f x y 
    | f x > f y = x
    | otherwise = y 


greatestOn :: Ord a => (b -> a) -> [b] -> Maybe b
greatestOn _ [] = Nothing
greatestOn f (x:xs) = Just $ foldr (maxOn f) x xs

mostPopular :: Eq a => [a] -> Maybe a
mostPopular xs = greatestOn (flip count xs) xs

mostPopularLetter :: String -> Maybe Char
mostPopularLetter = mostPopular

mostPopularWord :: [String] -> Maybe String
mostPopularWord = mostPopular . (flatMap words)


-- Hutton's Razor

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
