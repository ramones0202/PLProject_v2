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
      encontraSapo,
      insereOrdenado,
      atualizaGridEixoX,
      atualizaGridEixoY,
      movCima,
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
mostraPos :: [String] -> IO()
mostraPos [] = putStrLn ""
mostraPos (a:as) = do 
          printf "%s\n" a
          mostraPos as 

-- Move o sapo a direita.
movDir :: [String] -> String -> [String] 
movDir tabuleiro (a:as) = do
      let aux = a : delete '-' (tail (reverse as))
      let res = "|-" ++ reverse aux
      atualizaGridEixoX tabuleiro res
        
movEsq :: [String] -> String -> [String]
movEsq tabuleiro (a:as) = do
      let aux = "|-" ++ tail(reverse (delete '-' as))
      let res = a : (reverse aux)
      atualizaGridEixoX tabuleiro res

-- TO DO: exibir o grid conforme o sapo anda pra cima
movCima :: [String] -> String -> [String]
movCima tabuleiro posAtual  = do
        let posAtual = estadoLinhaAtual tabuleiro
        atualizaGridEixoY tabuleiro posAtual 
        

insereOrdenado :: Int -> String -> [String] -> [String]
insereOrdenado index elem xs = take index xs ++ [elem] ++ drop index xs

encontraSapo :: [String] -> Int -> Int
encontraSapo [] acc = -1
encontraSapo (a:as) acc = if elem 'S' a then acc
                          else encontraSapo as (acc + 1) 

estadoLinhaAtual :: [String] -> String
estadoLinhaAtual [] = "Erro!"
estadoLinhaAtual (a:as) = if elem 'S' a then a
                          else estadoLinhaAtual as

atualizaGridEixoX :: [String] -> String -> [String]
atualizaGridEixoX tabuleiro estadoLinhaAtual
  | encontraSapo tabuleiro 0 == 0 = [estadoLinhaAtual, linha2, linha3, linha4, linha5]
  | encontraSapo tabuleiro 0 == 1 = [linha2, estadoLinhaAtual, linha3, linha4, linha5]
  | encontraSapo tabuleiro 0 == 2 = [linha2, linha2, estadoLinhaAtual, linha4, linha5]
  | encontraSapo tabuleiro 0 == 3 = [linha2, linha2, linha2, estadoLinhaAtual, linha5]
  | encontraSapo tabuleiro 0 == 4 = [linha2, linha2, linha2, linha2, estadoLinhaAtual]

atualizaGridEixoY :: [String] -> String -> [String]
atualizaGridEixoY tabuleiro estadoLinhaAtual
  | encontraSapo tabuleiro 0 == 0 = [linha2, estadoLinhaAtual, linha3, linha4, linha5]
  | encontraSapo tabuleiro 0 == 1 = [linha2, linha2, estadoLinhaAtual, linha4, linha5]
  | encontraSapo tabuleiro 0 == 2 = [linha2, linha2, linha2, estadoLinhaAtual, linha5]
  | encontraSapo tabuleiro 0 == 3 = [linha2, linha2, linha2, linha2, estadoLinhaAtual]
  | encontraSapo tabuleiro 0 == 4 = ["Você ganhou!"]

-- Recebe como parâmetro a direção, em Char, do movimento,
-- e a String representando a posAtual
identificaMov :: [String] -> String -> String -> [String]
identificaMov tabuleiro mov posAtual
  | mov == "d" = movDir tabuleiro posAtual
  | mov == "e" = movEsq tabuleiro posAtual
  | mov == "c" = movCima tabuleiro posAtual                
  | otherwise = ["Comando inválido!"]

-- loop "infinito" do jogo.
gameLoop :: [String] -> IO()
gameLoop tabuleiro = do 
        mostraPos tabuleiro 
        input <- getLine
        let movimento = input
        let linhaAtual = estadoLinhaAtual tabuleiro
        let gridAtualizado = identificaMov tabuleiro movimento linhaAtual
        gameLoop gridAtualizado
        

  