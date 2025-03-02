module Lib
    ( posInicial,
      mostraPos,
      movDir,
      movEsq,
      identificaMov,
      gameLoop
    ) where

import Data.List (delete)

-- exibe "Grid" inicial: um [Char] ou String com 12 caracteres, sendo:
-- 2 '|' que delimitam os limetes do jogo
-- 1 'S' que representa o Sapo
-- 2 '-' que representam espaços em disponíveis
posInicial :: String
posInicial =  "|--S--|"
              
-- Exibe posição do sapo. Evita trabalhar com o tipo impuro IO              
mostraPos :: String -> IO()
mostraPos pos = putStrLn pos

-- Move o sapo a direita.
movDir :: String -> String 
movDir (a:as) = do
        let aux = a : delete '-' (tail (reverse as))
        "|-" ++ reverse aux
        

  
--TO DO: Consertar movimentação a esquerda (tentar replicar o que foi feito na mov. a direita)
movEsq :: String -> String
movEsq (a:as) = "|-" ++ tail (reverse (delete '-' as)) ++ "|"
    
    


-- Recebe como parâmetro a direção, em Char, do movimento,
-- e a String representando a posAtual
identificaMov :: String -> String -> String
identificaMov mov posAtual
  | mov == "d" = movDir posAtual
  | mov == "e" = movEsq posAtual               
  | otherwise = "Comando inválido!"

-- loop "infinito" do jogo.
-- TO DO: atualizar posição a cada loop e identificar bug do "Comando inválido!"
gameLoop :: String -> IO()
gameLoop posAtual = do 
        mostraPos posAtual
        input <- getLine
        let movimento = input
        gameLoop(identificaMov movimento posAtual)
        

  