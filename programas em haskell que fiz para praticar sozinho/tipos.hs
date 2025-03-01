-- Tipos parecidos com enumerados
data Logico = Verdadeiro | Falso
data DiaSemana = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado

-- Tipos parecidos com registros
-- Ponto é o nome do tipo. Coord é chamado de contrutor e equivale a uma função:
-- Coord :: Double -> Double -> Ponto. Note que os "campos" não têm nomes.
-- O tipo e o "construtor" podem ter o mesmo nome quando há só um construtor
data Ponto = Coord Double Double
    deriving Show
data Vetor = Vec Double Double Double

-- Usando a barra vertical com tipos que não são unitários, temos algo parecido com
-- herança:
data Geom = Circulo Ponto Double | Triangulo Ponto Ponto Ponto
    deriving Show

-- Agora uma função para area de FormasGeometricas
area :: Geom -> Double
area (Circulo _ raio)   = pi * raio^2
area (Triangulo 
        (Coord x1 y1)
        (Coord x2 y2)
        (Coord x3 y3))  = (abs ((x1-x3)*(y2-y1) - (x1-x2)*(y3-y1))) / 2

-- Sintaxe alternativa para tipos análogos a registros:
data Aluno = Al { nome      :: String,
                  matricula :: Integer
                } deriving (Eq, Show, Read)
-- Posso criar um valor usando as chaves também:
-- Al {nome = "Bruno", matricula = 7654321}
-- Cria automaticamente as "funções de acesso": nome e matricula

-- Podemos parametrizar tipos. Veja a árvore binária de t.
-- Note que essa é uma definição recursiva.
data Arvore t = Vazia | Noh t (Arvore t) (Arvore t)
