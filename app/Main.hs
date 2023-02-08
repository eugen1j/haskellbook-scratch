{-# LANGUAGE DeriveGeneric #-}

module Main (main) where


functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool; functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)



main :: IO ()
main = do
    x <- e
    putStrLn $ show x

