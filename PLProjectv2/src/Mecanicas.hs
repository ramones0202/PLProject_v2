module Mecanicas (
    mostraPos,
    gameLoop
) where

import Movimentacao
import Text.Printf (printf)

-- Exibe posição do sapo. Evita trabalhar com o tipo impuro IO              
mostraPos :: [String] -> IO()
mostraPos [] = putStrLn ""
mostraPos (a:as) = do 
          printf "%s\n" a
          mostraPos as 
          
-- loop "infinito" do jogo.
gameLoop :: [String] -> IO()
gameLoop ["Você ganhou!"] = putStrLn "Você ganhou!"
gameLoop tabuleiro = do 
        mostraPos tabuleiro 
        input <- getLine
        let movimento = input
        let linhaAtual = estadoLinhaAtual tabuleiro
        let gridAtualizado = identificaMov tabuleiro movimento linhaAtual
        gameLoop gridAtualizado