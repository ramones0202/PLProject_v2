module Main (main) where

import Grid
import Mecanicas
import Movimentacao

main :: IO ()
main = do
    gameLoop grid

    