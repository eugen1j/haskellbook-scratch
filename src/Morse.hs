module Morse
    ( Morse
    , charToMorse
    , morseToChar
    , stringToMorse
    , letterToMorse
    , morseToLetter
    ) where

import qualified Data.Map as M

type Morse = String

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList [
    ('a',".-")
    ,('n',"-.")
    ,('b',"-...")
    ,('o',"---")
    ,('c',"-.-.")
    ,('d',"-..")
    ,('p',".--.")
    ,('q',"--.-")
    ,('e',".")
    ,('r',".-.")
    ,('f',"..-.")
    ,('s',"...")
    ,('g',"--.")
    ,('t',"-")
    ,('h',"....")
    ,('u',"..-")
    ,('i',"..")
    ,('v',"...-")
    ,('j',".---")
    ,('w',".--")
    ,('k',"-.-")
    ,('x',"-..-")
    ,('l',".-..")
    ,('y',"-.--")
    ,('m',"--")
    ,('z',"--..")
    ,('1',".----")
    ,('6',"-....")
    ,('2',"..---")
    ,('7',"--...")
    ,('3',"...--")
    ,('8',"---..")
    ,('4',"....-")
    ,('9',"----.")
    ,('5',".....")
    ,('0',"-----")]

morseToLetter :: M.Map Morse Char
morseToLetter =
    M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse = mapM charToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

