
unicaOcorrencia :: Eq t => t -> [t] -> Bool
unicaOcorrencia a vetor
        |unicaOcorrencia1 a 0 vetor == 1 = True
        |otherwise = False
        where   unicaOcorrencia1:: Eq t => t -> Int -> [t] -> Int
                unicaOcorrencia1 a t [] = t 
                unicaOcorrencia1 a t (c:b)
                    |c == a = unicaOcorrencia1 a (t+1) b
                    |otherwise = unicaOcorrencia1 a t b


remove :: Eq t => t -> [t] -> [t]
remove elemento [] = []
remove elemento (c:r)
    |elemento == c = r
    |otherwise = c:remove elemento r


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