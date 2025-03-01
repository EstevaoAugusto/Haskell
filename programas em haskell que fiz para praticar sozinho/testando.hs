-- Isso é um comentário --

duasVezes :: (a -> a) -> a -> a
duasVezes f x = f (f x)

-- [5,4,3,2,1]
-- [4,5,3,2,1]


estaOrdenado :: (Ord a) => [a] -> Bool
estaOrdenado [] = True
estaOrdenado [x] = True
estaOrdenado (x:y:xs)
	| x <= y    = estaOrdenado (y:xs)
	| otherwise = False
	
ordena [] = []
ordena (c:r) 
	| estaOrdenado (c:r) == False = ordena (insertionSort r c)
	| otherwise = (c:r)
	where insertionSort (x:xs) aux
		| xs /= [] && aux >= x = [x] ++ insertionSort xs aux
		| otherwise = [x] ++ aux:xs

menorDeDois :: (Ord t) => t -> t -> t
menorDeDois a b
	| a < b 	= a
	| otherwise = b
	
somaEntreDois ::(Integral t) => t -> t -> t
somaEntreDois a b = a + b

quantInstancias ::(Num t) => t -> [t] -> t
quantInstancias _ [] = 0
quantInstancias n (c:r)
	| n == c = 1 + quantInstancias n r
	| otherwise = quantInstancias n r

unicaOcorrencia :: (Num a) => a -> [a] -> Bool
unicaOcorrencia t [] = False
unicaOcorrencia t (c:r) = quantInstancias t (c:r) == 1 



pertence :: (Eq i) => i -> [i] -> Bool
pertence n (c:r)
	| n /= c = pertence n r
	| otherwise = True
pertence n [] = False

fatorial :: (Integral i) => i -> i
fatorial a 
	| a > 0 = a * fatorial (a-1)
	| otherwise = 1

multiEntreDois ::(Integral t) => t -> t -> t
multiEntreDois a b = a * b

remove1oElement :: (Eq a) => [a] -> a -> [a]
remove1oElement (c:r) n
	|n == c = r
	| otherwise = c: remove1oElement r n
remove1oElement [] _ = []

removeUltElement :: (Eq a) => [a] -> [a]
removeUltElement (c:r)
	| r /= [] = c: removeUltElement r
	| otherwise = []

somatorio :: (Integral a) => [a] -> a
somatorio (c:r) = c + somatorio r
somatorio [] = 0

inverter :: [a] -> [a]
inverter (c:r) = inverter r ++ [c]
inverter [] = []

-- Implementar AcumuladorDireita
-- Implementar AcumuladorEsquerda

--partes :: [t] -> [[t]]
--partes (c:r) = 

-- Define the lambda functions separately (optional, for clarity)
lambda1 :: Num a => a -> a -> a
lambda1 = \x y -> x * x + y * y

lambda2 :: Num a => a -> a -> a
lambda2 = \c d -> c * c + d * d

-- Define the 'soma' function to use these lambda functions
soma :: Num a => a -> a -> a -> a -> a
soma a b c d = lambda1 a b + lambda2 c d


--equacao2 :: (Real t) => t -> t -> t -> t
-- equacao2 a b c =  let raiz = sqrt (b*b - 4*a*c) doisA = 2*a in ((raiz-b)/doisA, ((-b)-raiz)/raiz)

-- conjunto das partes

partes :: [t] -> [[t]]
partes (c:r) =
	let partesR = partes r 
	in partesR ++ (map (c:) partesR)
partes [] = [[]]


-- base 

mudaBase :: (Integral a,Integral b) => a -> b -> String
mudaBase num base = mudaBase' num (fromIntegral base)
	where mudaBase' num base' =
		let quociente = div num base
			resto = mod num base
			algarismo n
				| n < 10 = chr (n + (ord '0')
				| otherwise = chr ((n-10) + (ord 'A'))
			inicio
				| num < base' = []
				| otherwise = mudaBase' quociente base'
		in (mudaBase quociente base) ++ [algarismo resto]