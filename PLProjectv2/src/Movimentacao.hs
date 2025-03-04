module Movimentacao (
    movDir,
    movEsq,
    encontraSapo,
    encontraColunaSapo,
    estadoLinhaAtual,
    insereOrdenado,
    atualizaGridEixoX,
    atualizaGridEixoY,
    movCima,
    identificaMov
) where

import Grid
import Data.List (delete)

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

movCima :: [String] -> String -> [String]
movCima tabuleiro posAtual  = do
        let posAtual = estadoLinhaAtual tabuleiro
        atualizaGridEixoY tabuleiro  

-- Recebe como parâmetro a direção, em Char, do movimento,
-- e a String representando a posAtual
identificaMov :: [String] -> String -> String -> [String]
identificaMov tabuleiro mov posAtual
  | mov == "d" = movDir tabuleiro posAtual
  | mov == "e" = movEsq tabuleiro posAtual
  | mov == "c" = movCima tabuleiro posAtual                
  | otherwise = ["Comando inválido!"]
        
insereOrdenado :: Int -> Char -> String -> String
insereOrdenado index elem xs = take index xs ++ [elem] ++ drop (index + 1) xs

encontraSapo :: [String] -> Int -> Int
encontraSapo [] acc = -1
encontraSapo (a:as) acc = if elem 'S' a then acc
                          else encontraSapo as (acc + 1)

encontraColunaSapo :: String -> Int-> Int
encontraColunaSapo (a:as) acc = if a == 'S' then acc
                                else encontraColunaSapo as (acc + 1)

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

atualizaGridEixoY :: [String] -> [String]
atualizaGridEixoY tabuleiro
  | encontraSapo tabuleiro 0 == 0 = [linha5, insereOrdenado (encontraColunaSapo (estadoLinhaAtual tabuleiro) 0) 'S' linha2, linha3, linha4, linha5]
  | encontraSapo tabuleiro 0 == 1 = [linha5, linha2, insereOrdenado (encontraColunaSapo (estadoLinhaAtual tabuleiro) 0) 'S' linha3, linha4, linha5]
  | encontraSapo tabuleiro 0 == 2 = [linha5, linha2, linha3, insereOrdenado (encontraColunaSapo (estadoLinhaAtual tabuleiro) 0) 'S' linha4, linha5]
  | encontraSapo tabuleiro 0 == 3 = [linha5, linha2, linha3, linha4, insereOrdenado (encontraColunaSapo (estadoLinhaAtual tabuleiro) 0) 'S' linha5]
  | encontraSapo tabuleiro 0 == 4 = ["Você ganhou!"]