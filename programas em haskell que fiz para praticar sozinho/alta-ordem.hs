-- Auxiliares
menorDeDois a b
    | b < a     = b
    | otherwise = a

subtracaoDeDois a b = a - b
somaDeDois a b = a + b

-- trocaParam troca os dois parâmetros de uma função
trocaParam fun a b = fun b a

-- =============================================================================================
-- COMPREENSÃO DE LISTAS (list comprehension)
-- auxiliares
lista1 = [1..10]

-- O termo compreensão é usado conforme significado da lógica aristotélica, segundo a qual, cada
-- conceito tem "extensão" e "compreensão". O primeiro é conjunto das instâncias do conceito e o
-- segundo é o conjunto dos atributos (qualidades) do conceito.
lista2 = [x*2 | x <- lista1]



-- =============================================================================================
-- FUNÇÕES DE ALTA ORDEM
-- Filtro: filtra os elementos de uma lista de acordo com uma propriedade qualquer.
filtro :: [t] -> (t -> Bool) -> [t]
filtro (c:r) p
    | p c       = c:(filtro r p)
    | otherwise = filtro r p
filtro [] _     = []

-- processarTodos: aplica uma função em todos elementos de uma lista
-- Uma função assim pode ser vista como um análogo ao laço para processar uma estrutura de dados
processarTodos :: (a -> b) -> [a] -> [b]
processarTodos f (c:r) = (f c):(processarTodos f r)
processarTodos _ []    = []

-- accumdDir: acumula um processamento numa lista (às vezes chamado de reduce)
-- Existem diferentes versões, para processamento linear: à direita e à esquerda
accumDir :: (a -> b -> b) -> b -> [a] -> b
accumDir fun ac (c:r) = fun c (accumDir fun ac r)
accumDir _   ac []    = ac

accumEsq :: (b -> a -> b) -> b -> [a] -> b
accumEsq fun ac (c:r) = accumEsq fun (fun ac c) r  --recursao comum
accumEsq _   ac []    = ac

-- O processamento pode ser em estilo de árvore binária
-- A função pares processa de dois em dois
pares :: (a -> a -> a) -> [a] -> [a]
pares fun (a:b:r) = (fun a b) : (pares fun r)
pares _   lista   = lista

accumArv :: (t -> t -> t) -> [t] -> t
accumArv _   [x] = x
accumArv fun l   = accumArv fun (pares fun l)

-- Exemplos: -----
-- accumDir (-) 0 [1,2,3] = (1-(2-(3-0))) = 2
-- accumEsq (-) 0 [1,2,3] = (((0-1)-2)-3) = -6
-- accumArv (-) [1,2,3,4,5] = ((1-2)-(3-4))-5 = -5
-- menor de uma lista:
menor (c:r) = accumDir menorDeDois c r
-- somatorio de uma lista:
somatorio lista = accumDir (+) 0 lista
-- concatenação de listas
concatena = trocaParam (accumDir (:))
-- mergesort com acumulação em arvore

-- a função merge foi apagada porque é um exercício da lista do trabalho
--mergesort l = accumArv merge (processarTodos (\a -> [a]) l)

