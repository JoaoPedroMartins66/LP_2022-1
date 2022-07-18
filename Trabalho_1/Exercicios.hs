-- Intruções:
-- -  Não importe nenhuma biblioteca EXCETO se na descrição do exercício estiver explícito.

module Trabalho_1.Testes.Exercicios where
import GHC.Real (notANumber)
import Graphics.Win32 (bST_CHECKED, mFS_CHECKED)
import Data.List (sort)
import Distribution.Compat.CharParsing (CharParsing(string))

-- 1) (Valor da questão: 1,0 ponto)
-- Defina uma função que retorne o maior entre quatro inteiros.
maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d
    | (a >= b) && (a >= c) && (a >= d) = a
    | (b >= c) && (b >= d) = b
    | c >= d  = c
    | otherwise = d

-- 2) (Valor da questão: 1,0 ponto)
-- Defina uma função que receba uma nota e retorne a menção do aluno.
-- Não se preocupe com a validação do input. A nota sempre será um Número entre 0.0 (inclusive) e 10.0 (inclusive).
-- Considere a seguinte tabela para tradução da menção:
-- De 9 a 10 -> "SS"
-- De 7 a 8.9 -> "MS"
-- De 5 a 6.9 -> "MM"
-- De 3 a 4.9 -> "MI"
-- De 0.1 a 2.9 -> "II"
-- De 0 -> "SR"
converterNotaParaMencao :: Float -> String
converterNotaParaMencao nota
    | nota == 0 = "SR"
    | nota < 3 = "II"
    | nota < 5 = "MI"
    | nota < 7 = "MM"
    | nota < 9 = "MS"
    | otherwise = "SS"

-- 3) (Valor da questão: 1,0 ponto)
-- defina uma função que retorna um booleano indicando se uma lista de inteiros é decrescente ou não:
isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente [n] = True
isDecrescente  (n:ns)
    | n <= head ns = False
    | otherwise = isDecrescente ns

-- 4) (Valor da questão: 2,0 pontos)
-- defina uma função que recebe uma lista de strings como entrada e computa uma lista de pares
-- de (String, Int) representando o histograma de seus elementos:
histograma :: [String] -> [(String, Int)]
histograma [] = []
histograma [st] = [(st, 1)]
histograma (st:sts) =  ( st, 1 + countSt st sts ) : histograma (filter (/=st) sts)
    where
        countSt _ [] = 0
        countSt st (l:ls)
            | st == l = 1 + countSt st ls
            | otherwise = countSt st ls




-- 5)(Valor da questão: 1,5 ponto)
-- Defina a função myZipWith que tem como parâmetros uma função binária (que tem dois parâmetros) e duas listas,
-- retornando uma lista de valores resultantes da aplicação dessa função nos elementos correspondentes dessas
-- listas:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs


-- 6) (Valor da questão: 2,0 ponto)
-- Resolva em Haskell o seguinte problema: a partir de duas notas das provas de cada aluno,
-- determinar a lista dos alunos aprovados, com suas respectivas médias. O resultado deve estar
-- ordenado crescentemente pela média aritmética das notas. A aprovação ocorre se, e somente se, tal
-- média é maior ou igual a cinco.
-- OBSERVAÇÃO: especificamente para este exercício, você pode importar as funções de ordenaçao de listas (como 'sort' ou 'sortBy') se achar necessário.
aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia [] = []
aprovadosOrdemDeMedia ((nome,nota1,nota2):as)
    | (nota1+nota2)/2 >= 5  = qSortMedia  ((nome, (nota1+nota2)/2) : aprovadosOrdemDeMedia as)
    | otherwise = qSortMedia (aprovadosOrdemDeMedia as)
    where
        qSortMedia [] = []
        qSortMedia((nome,media):as) = qSortMedia [(nm, md) | (nm, md) <- as , md <= media] ++ [(nome,media)]++ qSortMedia [(nm, md) | (nm, md) <- as , md > media]

-- 7) (Valor da questão: 1,5 ponto, sendo 0.5 ponto para cada letra)
-- Considere a representação de matrizes como lista de listas em que cada elemento da lista é uma lista
-- que representa uma linha da matriz. Com base nisso, determine as seguintes funções:
--  a) some duas matrizes
--  b) compute a transposta de duas matrizes
--  c) compute a multiplicação de duas matrizes
-- OBSERVAÇÃO: considere que os inputs são válidos (ou seja, as matrizes são válidas e as suas dimensões são compatíveis para soma e multiplicação)
somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial [] [] = []
somaMatricial (a:as) (b:bs) = myZipPlus a b : somaMatricial as bs
    where
        myZipPlus  [] _ = []
        myZipPlus  _ [] = []
        myZipPlus (a:as) (b:bs) = a + b : myZipPlus as bs


matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta [] = []
matrizTransposta m
    | null (head m) = []
    | otherwise =  map head m : matrizTransposta ( map tail m )



multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial = undefined
