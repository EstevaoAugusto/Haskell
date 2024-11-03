-- Estevao Augusto da Fonseca Santos       202310386
-- Gabriel Ferreira de Castro			   202310169
-- grupo nº 1

import Char

-- Funcoes Extras



-- 1

unicaOcorrencia :: Eq t => t -> [t] -> Bool
unicaOcorrencia a vetor
        |unicaOcorrencia1 a 0 vetor == 1 = True
        |otherwise = False
        where   unicaOcorrencia1:: Eq t => t -> Int -> [t] -> Int
                unicaOcorrencia1 a t [] = t 
                unicaOcorrencia1 a t (c:b)
                    |c == a = unicaOcorrencia1 a (t+1) b
                    |otherwise = unicaOcorrencia1 a t b


--4
remove :: Eq t => t -> [t] -> [t]
remove elemento [] = []
remove elemento (c:r)
    |elemento == c = r
    |otherwise = c:remove elemento r

--7
maiores :: (Ord t, Num t) => t -> [t] -> [t]
maiores numero [] = []
maiores numero (c:r)
    |maior c r < numero = c: maiores (numero-1) r
    |otherwise = maiores numero r
    where maior :: (Ord t, Num t) => t -> [t] -> t
          maior c [] = 0
          maior c (h:r)
            | h>c = 1 + maior c r
            | otherwise =  maior c r 


-- Q10 	

divide :: (Integral x) => [x] -> x -> ([x],[x])
divide [] a = ([],[])
divide (c:r) 0 = ([],(c:r))
divide (c:r) a = (pegaNValores (c:r) a,removerNValores (c:r) a)
	where 
		removerNValores :: (Integral x) => [x] -> x -> [x]
		removerNValores (c:r) a
			| a > 1 && r /= [] = removerNValores (r) (a-1)	
			| otherwise = r
		pegaNValores :: (Integral x) => [x] -> x -> [x]
		pegaNValores (c:r) a
			| a > 1 && r /= [] = [c]++ pegaNValores r (a-1)
			| otherwise = [c]

-- Q13

uniao :: (Eq t) => [t] -> [t] -> [t]
uniao [] [] = []
uniao [] (c2:r2) = (c2:r2)
uniao (c1:r1) [] = (c1:r1)
uniao (c1:r1) (c2:r2) = uniaoAux (c2:r2) c1 ++ uniao r1 (c2:r2)
	where uniaoAux (c2:r2) aux1 
		| r2 /= [] && aux1 /= c2 = uniaoAux r2 aux1
		| r2 == [] && aux1 /= c2 = [aux1]
		| otherwise = []
		

-- Q16

sequencia :: Int -> Int -> [Int]
sequencia 0 b = []
sequencia 1 b = [b]
sequencia a b 
	| a > 1 = b : sequencia (a-1) (b+1)

	
-- Q19

ordena :: (Ord t) => [t] -> [t]	
ordena [] = []
ordena (c:r) 
	| estaOrdenado (c:r) == False = ordena (insertionSort r c)
	| otherwise = (c:r)
	where 
		estaOrdenado :: (Ord a) => [a] -> Bool
		estaOrdenado [] = True
		estaOrdenado [x] = True
		estaOrdenado (x:y:xs)
			| x <= y    = estaOrdenado (y:xs)
			| otherwise = False
		insertionSort (x:xs) aux
			| xs /= [] && aux >= x = [x] ++ insertionSort xs aux
			| otherwise = [x] ++ aux:xs

-- Q22

rodar_esquerda :: Int -> [Int] -> [Int]
rodar_esquerda a (c:r)
	| a > 0 = rodar_esquerda (a-1) (r ++ [c])
	| otherwise = (c:r)
rodar_esquerda _ [] = []

-- Q25

--Consegui fazer ele funcionar. Desliguei o computador e testei com as configuraçoes abaixo
-- Deve ter sido algum problema na minha máquina

primeiras_maiusculas :: String -> String
primeiras_maiusculas [] = [] 
primeiras_maiusculas (c:r) 
	| c /= ' ' = toUpper c: primeiras_maiusculasAux r
	| otherwise = ' ': primeiras_maiusculas r
	where	
		primeiras_maiusculasAux [] = []
		primeiras_maiusculasAux (c:r)
			| r /= [] && c /= ' ' = toLower c: primeiras_maiusculasAux r
			| r /= [] && c == ' ' = ' ': primeiras_maiusculas r
			| otherwise = [toLower c]
			
--34
bolhaBurra ::  Ord a => [a] -> [a]
bolhaBurra vetor
    |ordenado vetor = vetor
    |otherwise = bolhaBurra (compara vetor)
    where   compara :: Ord a => [a] -> [a]
            compara [c] = [c]
            compara (numero:c:r) 
                    |numero > c = c:compara (numero:r) 
                    | otherwise = numero:compara (c:r)
            ordenado ::Ord a => [a] -> Bool
            ordenado [] = True
            ordenado [c] = True
            ordenado (c:n:r)
                    |c > n = False
                    |otherwise = ordenado (n:r)

--28
mediana ::  (Fractional t, Ord t) => [t] -> t
mediana vetor = ordAux (bolhaBurra vetor)
ordAux ::  (Fractional t, Ord t) => [t] -> t
ordAux [] = 0
ordAux vetor
    |(contNumeros vetor `mod` 2 == 0) = soma2 ((contNumeros vetor `div` 2)-1) 0 vetor
    |otherwise = soma1 ((((contNumeros vetor)+1)`div`2)-1) 0 vetor
    where   contNumeros :: Num t => [t] -> Int
            contNumeros [] = 0
            contNumeros (c:r) = 1 + contNumeros r
            soma1 :: Num t => Int -> Int -> [t] -> t
            soma1 lugar ini (c:n)
                |lugar == ini = c
                |otherwise = soma1 lugar (ini+1) n
            soma2 :: Fractional t => Int -> Int -> [t] -> t
            soma2 lugar ini (c:n:t)
                |lugar == ini = (c + n)/2
                |otherwise = soma2 lugar (ini+1) (n:t)

--31
palindromo :: [Char] -> Bool
palindromo vetor = compara (inverte vetor) vetor
    where   compara :: [Char] -> [Char] -> Bool
            compara [] [] = True
            compara (c:r) (h:t)
                |c == h = compara r t
                |otherwise = False
            inverte :: [Char] -> [Char]
            inverte [c] = [c]
            inverte [] = []
            inverte (c:r) = inverte r ++ [c]