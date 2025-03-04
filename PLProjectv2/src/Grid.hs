module Grid
    ( linha1,
      linha2,
      linha3,
      linha4,
      linha5,
      grid,
) where

-- As linhas subsequentes são compostas por:
-- 2 '|' que delimitam os limetes do jogo
-- 5 '-' que representam espaços em disponíveis 
linha1 :: String
linha1 =  "|--S--|"

linha2 :: String
linha2 = "|-~-~-|"

linha3 :: String
linha3 = "|~~---|"

linha4 :: String
linha4 = "|~~~--|"

linha5 :: String
linha5 = "|-----|"


-- Representa "Grid" inicial: um [Char] ou String com 12 caracteres, sendo:
-- 2 '|' que delimitam os limetes do jogo
-- 1 'S' que representa o Sapo
-- 4 '-' que representam espaços em disponíveis
grid :: [String]
grid = [linha1, linha2, linha3, linha4, linha5]    