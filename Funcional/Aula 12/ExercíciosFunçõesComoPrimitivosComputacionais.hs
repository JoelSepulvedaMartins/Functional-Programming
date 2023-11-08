module Main where

-- Exercício 1: Aplica N Vezes
-- Enunciado: Crie uma função que recebe um número inteiro n, uma função f e um valor x.
-- A função deve aplicar f n vezes em x.
aplicaNVezes :: Int -> (a -> a) -> a -> a
aplicaNVezes 0 _ x = x  -- Se n for zero, retornamos x sem modificações.
aplicaNVezes n f x = f (aplicaNVezes (n - 1) f x)  -- Aplica a função f n vezes.

-- Exercício 2: Filtragem Personalizada
-- Enunciado: Crie uma função que recebe uma função condição e uma lista.
-- A função deve retornar uma nova lista com elementos que satisfazem a condição.
filtraPersonalizado :: (a -> Bool) -> [a] -> [a]
filtraPersonalizado _ [] = []  -- Caso base: lista vazia.
filtraPersonalizado condicao (x:xs)
  | condicao x = x : filtraPersonalizado condicao xs  -- Se x satisfaz a condição, inclua-o no resultado.
  | otherwise  = filtraPersonalizado condicao xs  -- Se não, continue a filtragem.

-- Exercício 3: Cria Multiplicador
-- Enunciado: Crie uma função que recebe um número x e retorna uma nova função que multiplica seu argumento por x.
criaMultiplicador :: Num a => a -> (a -> a)
criaMultiplicador x = \y -> x * y  -- Retorna uma função que multiplica seu argumento por x.

-- Função principal para testar os exercícios
main :: IO ()
main = do
  -- Testes para o Exercício 1
  putStrLn "Exercício 1: Aplica N Vezes"
  print $ aplicaNVezes 3 (+1) 0  -- Deve retornar 3 (0 + 1 + 1 + 1)
  print $ aplicaNVezes 2 (*2) 1  -- Deve retornar 4 (1 * 2 * 2)
  
  -- Testes para o Exercício 2
  putStrLn "Exercício 2: Filtragem Personalizada"
  print $ filtraPersonalizado even [1..10]  -- Deve retornar [2,4,6,8,10]
  
  -- Testes para o Exercício 3
  putStrLn "Exercício 3: Cria Multiplicador"
  let multiplicaPorTres = criaMultiplicador 3  -- Cria uma função que multiplica seu argumento por 3
  print $ multiplicaPorTres 4  -- Deve retornar 12 (4 * 3)
