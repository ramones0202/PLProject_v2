module Lib
    ( posInicial,
      mostraPos,
      movDir,
      movEsq,
      identificaMov,
      gameLoop
    ) where

-- exibe "Grid" inicial: um [Char] ou String com 12 caracteres, sendo:
-- 2 '|' que delimitam os limetes do jogo
-- 1 'S' que representa o Sapo
-- 5 '-' que representam espaços em disponíveis
posInicial :: String
posInicial =  "|---S---|"
              
-- Exibe posição do sapo. Evita trabalhar com o tipo impuro IO              
mostraPos :: String -> IO()
mostraPos pos = putStrLn pos


-- Move o sapo a direita.
movDir :: String -> String
movDir (a:as) = reverse (drop 1 as) ++ "|"

movEsq :: String -> String
movEsq (a:as) =  a : drop 1 as 

-- Recebe como parâmetro a direção, em Char, do movimento,
-- e a String representando a posAtual
identificaMov :: Char -> String -> String
identificaMov mov posAtual
  | mov == 'd' = movDir posAtual
  | mov == 'e' = movEsq posAtual               
  | otherwise = "Comando inválido!"

-- loop "infinito" do jogo.
-- TO DO: atualizar posição a cada loop e identificar bug do "Comando inválido!"
gameLoop :: String -> IO()
gameLoop posAtual = do 
        input <- getChar
        let movimento = input
        mostraPos (identificaMov movimento posAtual)
        gameLoop posAtual
           

  