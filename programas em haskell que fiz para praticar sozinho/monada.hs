-- Explicação de monadas. Programa que soma dois números.


-- Função que soma dois números (é o processamento)
soma a b = a + b

-- Realizar Insertion Sort no exercicio 
-- [1,2,3,4,5] [2,1,3,4,5] [2,3,1,4,5] [2,3,4,1,5] [2,3,4,5,1]
-- 

-- O resto é interface --
-- Interface no sentido de entrada e saída de dados
-- Vamos usar os seguintes operadores de Haskell:
-- >> :: Monad m => m a ->  m b -> m b
--      recebe uma monada a e uma monada b
--      retorna a monada b
--      Podemos dizer que passa um contexto adiante, sem nenhuma informação
-- >>= :: Monad m => m a -> (a -> m b) -> m b
--      recebe uma monada a e uma função que recebe um a e retorna uma monada b
--      retorna uma monada b
--      Podemos dizer que recebe uma informação em um contexto e faz o contexto passar adiante pela
--      função que processa a informação. Para a função (segundo parâmetro), a informação está sem
--      contexto.

-- Vamos usar as seguintes funções de Haskell:
-- return :: Monad m => b -> m b
--      Podemos dizer que associa uma informação com um contexto
-- putStr :: String -> IO ()
-- getLine :: IO String


-- Função (monádica) que lê um inteiro
--lerInteiro :: String -> IO Int
lerInteiro1 prompt =
  (putStr prompt) >>
    getLine >>=
      (\valor -> return (read valor))

-- Isso seria algo como:
-- read (getLine (putStr prompt))

-- Função que le dois números e escreve a soma deles (monadas)
programa1 =
  lerInteiro1 "Entre com um valor: " >>= 
    (\a -> lerInteiro1 "Entre com outro valor: " >>=
      (\b -> putStr ("A soma é: " ++ show (soma a b))))

-- Felizmente, a linguagem oferece uma sintaxe mais legível:
lerInteiro2 prompt =
    do putStr prompt
       str <- getLine
       return (read str)

programa2 = 
    do a <- lerInteiro2 "Entre com um valor: "
       b <- lerInteiro2 "Entre com outro valor: "
       putStr ("A soma é: " ++ show (soma a b))

