module Main (main) where

import Lib

main :: IO ()
main = do
    mostraPos posInicial
    input <- getChar
    let movimento = input
    mostraPos (identificaMov movimento posInicial)

    