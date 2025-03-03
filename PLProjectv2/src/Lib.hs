module Lib
    ( linha1,
      linha2,
      linha3,
      linha4,
      linha5,
      grid,
      mostraPos,
      movDir,
      movEsq,
      identificaMov,
      gameLoop
    ) where

import Data.List (delete)
import Text.Printf (printf)

-- As linhas subsequentes são compostas por:
-- 2 '|' que delimitam os limetes do jogo
-- 5 '-' que representam espaços em disponíveis 
linha1 :: String
linha1 =  "|--S--|"

linha2 :: String
linha2 = "|-----|"

linha3 :: String
linha3 = "|-----|"

linha4 :: String
linha4 = "|-----|"

linha5 :: String
linha5 = "|-----|"

-- Representa "Grid" inicial: um [Char] ou String com 12 caracteres, sendo:
-- 2 '|' que delimitam os limetes do jogo
-- 1 'S' que representa o Sapo
-- 4 '-' que representam espaços em disponíveis
grid :: [String]
grid = [linha1, linha2, linha3, linha4, linha5]    

-- Exibe posição do sapo. Evita trabalhar com o tipo impuro IO              
mostraPos :: String -> String -> String -> String -> String -> IO()
mostraPos linha1 linha2 linha3 linha4 linha5 = printf "%s\n%s\n%s\n%s\n%s\n" linha1 linha2 linha3 linha4 linha5

-- Move o sapo a direita.
movDir :: String -> String 
movDir (a:as) = do
        let aux = a : delete '-' (tail (reverse as))
        "|-" ++ reverse aux
        
movEsq :: String -> String
movEsq (a:as) = do
        let aux = "|-" ++ tail(reverse (delete '-' as))
        a : (reverse aux)

-- TO DO: exibir o grid conforme o sapo anda pra cima
--movCima :: String -> String
--movCima posAtual posAcima =   
    
-- Recebe como parâmetro a direção, em Char, do movimento,
-- e a String representando a posAtual
identificaMov :: String -> String -> String
identificaMov mov posAtual
  | mov == "d" = movDir posAtual
  | mov == "e" = movEsq posAtual               
  | otherwise = "Comando inválido!"

-- loop "infinito" do jogo.
gameLoop :: String ->  String -> String -> String -> String -> IO()
gameLoop linha1 linha2 linha3 linha4 linha5 = do 
        mostraPos linha1 linha2 linha3 linha4 linha5
        input <- getLine
        let movimento = input
        gameLoop (identificaMov movimento linha1) linha2 linha3 linha4 linha5
        

  