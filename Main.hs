-- Aluno: Alan Renato Bunese
-- Disciplina: Programação Funcional
-- Professor: Frank Alcantara
module Main (main) where

-- Módulo para função Ord.
import Data.Char

{- 1
 - Escreva uma função chamada factorialn que usando o operador range e a função foldr devolva
 - o fatorial de n.
 -}
factorialn :: Int -> Int
factorialn n
    | n < 0 = error "não existe fatorial de número negativo"
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = foldr (*) n [1 .. n - 1]

{- 2
 - Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista
 - de números reais, positivos e negativos e devolva uma lista com o quadrado de cada
 - um dos reais listados.
 -}
quadradoReal :: [Float] -> [Float]
quadradoReal n
    | null n = []
    | otherwise = map (** 2) n

{- 3
 - Usando a função map escreva uma função, chamada quadradoInteiro que recebe uma lista
 - de números inteiros, positivos e negativos e devolva uma lista com o quadrado de cada
 - um dos inteiros listados.
 -}
quadradoInteiro :: [Int] -> [Int]
quadradoInteiro n
    | null n = []
    | otherwise = map (^ 2) n

{- 3
 - Usando a função map escreva uma função, chamada comprimentoPalavras que recebe uma lista de
 - palavras e devolve uma lista com o comprimento de cada uma destas palavras.
 -}
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras n
    | null n = []
    | otherwise = map length n

{- 4
 - Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior
 - número entre 0 e 100000 que seja divisível por 29.
 -}
maiorMultiploDe29 :: Int
maiorMultiploDe29 = maximum (filter (\x -> x `mod` 29 == 0) [0 .. 100000])

{- 5
 - Usando a função filter escreva uma função, chamada maiorMultiploDe que recebe um inteiro e devolva
 - o maior número entre 0 e 100000 que seja divisível por este inteiro.
 -}
maiorMultiploDe :: Int -> Int
maiorMultiploDe n
    | n < 0 = error "não existe multiplo de número negativo"
    | otherwise = maximum (filter (\x -> x `mod` n == 0) [0 .. 100000])

{- 6
 - Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva
 - a soma dos quadrados dos itens de uma lista de números naturais de comprimento n.
 - De tal forma que: somaQuadrados = 1^2 + 2^2 + 3^2 + ... + n^2
 -}
somaQuadrados :: Int -> Int
somaQuadrados n
    | n <= 0 = error "não existe soma de quadrados de número negativo ou neutro (apenas naturais)"
    | otherwise = foldr (+) 0 (map (^ 2) [1 .. n])

{- 7
 - Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva
 - o comprimento (cardinalidade) de uma lista dada.
 -}
comprimento :: [any] -> Int
comprimento n
    | null n = 0
    | otherwise = foldl (\x _ -> x + 1) 0 n

{- 8
 - Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso
 - das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para
 - cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.
 -}
questao8 :: IO ()
questao8 = do
    -- Função flip.
    print ("flip: entrada: (>) 15 20; resultado: " ++ show (flip (>) 15 20))
    print ("flip: entrada: (>) 20 15; resultado: " ++ show (flip (>) 20 15))
    print ("flip: entrada: mod 30 20; resultado: " ++ show (flip mod 30 20))

    -- Função ord.
    print ("ord: entrada: 'a'; resultado: " ++ show (ord 'a'))
    print ("ord: entrada: 'b'; resultado: " ++ show (ord 'b'))
    print ("ord: entrada: 'c'; resultado: " ++ show (ord 'c'))

    -- Função max.
    print ("max: entrada: 15 20; resultado: " ++ show (max 15 20))
    print ("max: entrada: 20 15; resultado: " ++ show (max 20 15))
    print ("max: entrada: 15 15; resultado: " ++ show (max 15 15))

    -- Função min.
    print ("min: entrada: 15 20; resultado: " ++ show (min 15 20))
    print ("min: entrada: 20 15; resultado: " ++ show (min 20 15))
    print ("min: entrada: 15 15; resultado: " ++ show (min 15 15))

    -- Função curry.
    print ("curry: entrada: (\\(x, y) -> x + y) 15 20; resultado: " ++ show (curry (\(x, y) -> x + y) 15 20))
    print ("curry: entrada: (\\(x, y) -> x + y) 20 15; resultado: " ++ show (curry (\(x, y) -> x + y) 20 15))
    print ("curry: entrada: (\\(x, y) -> x + y) 15 15; resultado: " ++ show (curry (\(x, y) -> x + y) 15 15))

    -- Função uncurry.
    print ("uncurry: entrada: (\\x y -> x + y) (15, 20); resultado: " ++ show (uncurry (\x y -> x + y) (15, 20)))
    print ("uncurry: entrada: (\\x y -> x + y) (20, 15); resultado: " ++ show (uncurry (\x y -> x + y) (20, 15)))
    print ("uncurry: entrada: (\\x y -> x + y) (15, 15); resultado: " ++ show (uncurry (\x y -> x + y) (15, 15)))


-- Main...
main :: IO()
main = do
    -- Como querem uma questão por linha, assim será...
    print ("factorialn: entrada: 10; resultado: " ++ show (factorialn 10))
    print ("factorialn: entrada: 0; resultado: " ++ show (factorialn 0))
    print ("factorialn: entrada: 1; resultado: " ++ show (factorialn 1))

    print ("quadradoReal: entrada: [1.2, 2.5, 3.3, 4.8, 5.1]; resultado: " ++ show (quadradoReal [1.2, 2.5, 3.3, 4.8, 5.1]))
    print ("quadradoReal: entrada: [-1.1, -2.5, -3.8, -4.3, -5.9]; resultado: " ++ show (quadradoReal [-1.1, -2.5, -3.8, -4.3, -5.9]))
    print ("quadradoReal: entrada: [1.1, 2.2, 3.3, 4.4, 5.5, -1.6, -2.7, -3.8, -4.9, -5.1]; resultado: " ++ show (quadradoReal [1.1, 2.2, 3.3, 4.4, 5.5, -1.6, -2.7, -3.8, -4.9, -5.1]))

    print ("quadradoInteiro: entrada: [1, 2, 3, 4, 5]; resultado: " ++ show (quadradoInteiro [1, 2, 3, 4, 5]))
    print ("quadradoInteiro: entrada: [-1, -2, -3, -4, -5]; resultado: " ++ show (quadradoInteiro [-1, -2, -3, -4, -5]))
    print ("quadradoInteiro: entrada: [1, 2, 3, 4, 5, -1, -2, -3, -4, -5]; resultado: " ++ show (quadradoInteiro [1, 2, 3, 4, 5, -1, -2, -3, -4, -5]))

    print ("comprimentoPalavras: entrada: [\"Haskell\", \"é\", \"estranho\"]; resultado: " ++ show (comprimentoPalavras ["Haskell", "é", "estranho"]))
    print ("comprimentoPalavras: entrada: [\"Teste\", \"de\", \"comprimento\", \"de\", \"palavras\"]; resultado: " ++ show (comprimentoPalavras ["Teste", "de", "comprimento", "de", "palavras"]))

    print ("maiorMultiploDe29: resultado: " ++ show (maiorMultiploDe29))

    print ("maiorMultiploDe: entrada: 29; resultado: " ++ show (maiorMultiploDe 29))
    print ("maiorMultiploDe: entrada: 30; resultado: " ++ show (maiorMultiploDe 30))
    print ("maiorMultiploDe: entrada: 32; resultado: " ++ show (maiorMultiploDe 32))

    print ("somaQuadrados: entrada: 10; resultado: " ++ show (somaQuadrados 10))
    print ("somaQuadrados: entrada: 1; resultado: " ++ show (somaQuadrados 1))

    print ("comprimento: entrada: [1, 2, 3, 4, 5]; resultado: " ++ show (comprimento [1, 2, 3, 4, 5]))
    print ("comprimento: entrada: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]; resultado: " ++ show (comprimento [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))

    -- Finalizar com questão 8.
    questao8
