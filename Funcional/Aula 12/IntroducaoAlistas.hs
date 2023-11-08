module Main where

-- Listas em Haskell

-- Em Haskell, uma lista é uma coleção homogênea de elementos, o que significa que todos os elementos
-- em uma lista devem ser do mesmo tipo. As listas são definidas envolvendo os seus elementos entre
-- colchetes ([]) e separando-os com vírgulas.

-- 1. Criação de Listas

-- Você pode criar uma lista literalmente listando os elementos ou usando uma função de intervalo:

lista1 = [1, 2, 3, 4, 5]

lista2 = ['a', 'b', 'c', 'd']

lista3 = [1 .. 5] -- cria uma lista de 1 a 5

-- 2. Funções Básicas

-- Existem várias funções que você pode usar para trabalhar com listas. Algumas delas são:

-- `head`: retorna o primeiro elemento de uma lista.
-- `tail`: retorna uma lista sem o primeiro elemento.
-- `last`: retorna o último elemento de uma lista.
-- `init`: retorna uma lista sem o último elemento.

-- Exemplos:

exemploHead = head [1, 2, 3] -- retorna 1

exemploTail = tail [1, 2, 3] -- retorna [2, 3]

-- 3. Recursividade e Listas

-- Recursividade é usada para definir funções que trabalham com listas. Por exemplo,
-- uma função para calcular o comprimento de uma lista pode ser definida recursivamente assim:

comprimento :: [a] -> Int
comprimento [] = 0
comprimento (_ : xs) = 1 + comprimento xs

-- 4. Comprensões de Lista

-- Comprensões de lista são uma maneira de construir listas novas aplicando expressões a listas existentes.
-- Por exemplo:

exemploComprensao = [x * 2 | x <- [1 .. 10], x * 2 >= 12] -- retorna [12,14,16,18,20]

-- 5. Concatenação de Listas

-- Você pode concatenar duas listas usando o operador ++:

exemploConcatenacao = [1, 2, 3] ++ [4, 5, 6] -- retorna [1,2,3,4,5,6]

-- Exercício 1: Construa uma função que receba duas listas e retorne uma lista que seja a união das duas (sem usar o operador ++).
uniaoListas :: [a] -> [a] -> [a]
uniaoListas [] ys = ys
uniaoListas (x : xs) ys = x : uniaoListas xs ys

-- Exercício 2: Construa uma função que receba um número n e retorne uma lista com os n primeiros números pares.
nPrimeirosPares :: Int -> [Int]
nPrimeirosPares n = [x * 2 | x <- [0 .. n - 1]]

-- Exercício 3: Construa uma função que receba uma lista e retorne uma nova lista contendo todos os elementos da lista original, mas sem duplicatas.
removerDuplicatas :: Eq a => [a] -> [a]
removerDuplicatas = aux []
  where
    aux _ [] = []
    aux xs (y : ys)
      | y `elem` xs = aux xs ys
      | otherwise = y : aux (y : xs) ys

main :: IO ()
main = do
  print (head [16, 2, 3, 4, 5])
  print (tail [1, 2, 3, 4, 8])

  let lista = 1 : (2 : (3 : (4 : (78 : []))))
  print lista

  -- O operador ':' é usado para construir listas adicionando um elemento ao início de uma lista existente.
  -- 1 : (2 : (3 : (4 : (5 : []))))
  -- A expressão constrói uma lista da seguinte forma:
  -- 1 : (2 : (3 : (4 : (5 : []))))
  -- |
  -- |-> O número 1 é colocado na frente da lista que está sendo construída no restante da expressão.
  -- 2 : (3 : (4 : (5 : [])))
  -- |
  -- |-> O número 2 é colocado na frente da lista que está sendo construída no restante da expressão.
  -- 3 : (4 : (5 : []))
  -- |
  -- |-> O número 3 é colocado na frente da lista que está sendo construída no restante da expressão.
  -- 4 : (5 : [])
  -- |
  -- |-> O número 4 é colocado na frente da lista que está sendo construída no restante da expressão.
  -- 5 : []
  -- |
  -- |-> O número 5 é colocado na frente da lista vazia (representada por []).
  -- O resultado final da construção da lista com o operador ':' é a lista [1,2,3,4,5].

  -- Testando a função uniaoListas
  print (uniaoListas [1, 2, 3] [4, 5, 6])
  -- Isso deve imprimir [1, 2, 3, 4, 5, 6]

  -- Testando a função nPrimeirosPares
  print (nPrimeirosPares 5)
  -- Isso deve imprimir [0, 2, 4, 6, 8]

  -- Testando a função removerDuplicatas
  print (removerDuplicatas [1, 2, 2, 3, 3, 4])
  -- Isso deve imprimir [1, 2, 3, 4]