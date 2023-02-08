module Main (main) where

import Control.Monad 
import Data.Monoid 
import Test.QuickCheck


type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


newtype First' a =
    First' { getFirst' :: Maybe a } 
    deriving (Eq, Show)


instance (Arbitrary a) => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        frequency [ 
            (1, return $ First' $ Just a)
            , (1, return $ First' $ Nothing)]

instance Semigroup (First' a) where 
    (<>) (First' (Just a)) _ = First' $ Just a
    (<>) _ (First' (Just a)) = First' $ Just a
    (<>) _ _ = First' Nothing
    

instance Monoid (First' a) where 
    mempty = First' Nothing

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool


main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend) 
    quickCheck (monoidLeftIdentity :: FstId) 
    quickCheck (monoidRightIdentity :: FstId)
    




