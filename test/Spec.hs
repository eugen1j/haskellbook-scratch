{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

import qualified Data.Map as M
import Morse

import Test.QuickCheck
import Test.Hspec
import Test.QuickCheck.Gen
import Data.List
import Data.Char

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse, nub)
import System.Exit (exitSuccess)
import System.Random (randomRIO)




digitToWord :: Int -> String 
digitToWord n = case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    0 -> "zero"
    _ -> ""


digits :: Int -> [Int] 
digits n = reverse $ unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 10, x `div` 10)) n


wordNumber :: Int -> String 
wordNumber n = intercalate "-" $ map digitToWord $ digits n

half x = x / 2

halfIdentity = (*2) . half

-- for any list you apply sort to
-- this property should hold 
listOrdered :: (Ord a) => [a] -> Bool 
listOrdered xs =
    snd $ foldr go (Nothing, True) xs 
    where go _ status@(_, False) = status 
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x


main :: IO () 
main = hspec $ do
    describe "digitToWord" $ do
        it "returns zero for 0" $ do
            digitToWord 0 `shouldBe` "zero" 
        it "returns one for 1" $ do
            digitToWord 1 `shouldBe` "one"
    describe "digits" $ do
        it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ do
            digits 100 `shouldBe` [1, 0, 0]
    describe "wordNumber" $ do
        it "one-zero-zero given 100" $ do 
            wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001" $ do
            wordNumber 9001 `shouldBe` "nine-zero-zero-one"

    describe "identity check" $ do
        it "id equals self" $ do
            property $ \x ->  x == halfIdentity (x :: Double)

    describe "listOrdered" $ do
        it "ordered list returns true" $ do
            property $ \x -> listOrdered $ sort (x :: [Int])
    
    describe "math stuff" $ do
        it "plusAssociative" $ do
            property plusAssociative
        it "plusCommutative" $ do
            property plusCommutative
        it "reserved list" $ do
            property $ \x -> reverse (reverse x) == id (x :: [Int])
        it "8" $ do
            property dollarDefProp    
        it "9" $ do
            property $ \xs x -> x : xs == [x :: Int] ++ xs
        it "11" $ do
            property $ \x -> (read (show x)) == (x::Double)
        -- it "12" $ do
            -- property $ \x -> squareIdentity x == (x::Double)
        it "13" $ do
            property $ \x -> capitalizeWord x == twice capitalizeWord x 
                && capitalizeWord x == fourTimes capitalizeWord x
        it "14" $ do
            property $ \x -> x `elem` [Fulse, Frue]
        it "15" $ do
            quickCheck (forAll arbStr fillInCharacter_prop1)
            where 
                arbStr :: Gen String
                arbStr = sublistOf ['a'..'z']


fillInCharacter_prop1 :: String -> Bool
fillInCharacter_prop1 "" = True
fillInCharacter_prop1 word@(_:_) =
    case puzzle' of 
        Puzzle word' ((Just firstChar'):_) guessed' 
            -> word' == word && firstChar' == firstChar && guessed' == [firstChar]
        _ -> False  
    where 
        puzzle = freshPuzzle word
        firstChar = head word
        puzzle' = fillInCharacter puzzle firstChar
        

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice


square :: Num a => a -> a
square x = x * x

squareIdentity :: Double -> Double
squareIdentity = square . sqrt

dollarDefProp :: Blind (Int -> Int) -> Blind (Int -> Int) -> Int -> Bool
dollarDefProp (Blind f) (Blind g) x = (f . g) x == f (g x)

capitalizeWord :: String -> String 
capitalizeWord = fmap toUpper


data Fool = 
    Fulse
    | Frue
    deriving (Eq, Show)

instance Arbitrary Fool where
    arbitrary :: Gen Fool
    arbitrary = frequency [(1, return Fulse)
                            , (2, return Frue)]



newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList $ lines dict

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameLength :: String -> Bool
gameLength s = minWordLength < l && l < maxWordLength
    where l = length s

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList $ filter gameLength aw

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, (length wl) - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) = 
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c)  = c

incorrectGuesses :: Puzzle -> Int
incorrectGuesses (Puzzle _ filled guessed) = length guessed - (length $ nub $ filter isJust filled)

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = 
    Puzzle word newFilledInSoFar (c : s)
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar

          newFilledInSoFar = zipWith (zipper c) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that \
                     \character, pick something else."
            return puzzle
        (True, _) -> do 
            putStrLn "This character was in the word,\
                     \ filling in the word accordingly."
            return $ fillInCharacter puzzle guess
        _         -> do
            putStrLn "This character wasn't in the\
                     \ word, try again."
            return $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ _) = 
    if (incorrectGuesses puzzle) > 7
    then do 
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
        exitSuccess
    else
        return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = 
    if all isJust filledInSoFar
    then do
        putStrLn "You win"
        exitSuccess
    else 
        return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"
