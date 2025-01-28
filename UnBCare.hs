module UnBCare where
import ModeloDados
import Testes
import Control.Monad (when)
{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento remedio qtd estoque = if encontrado then novoEstoque else (remedio, qtd) : estoque where
  (encontrado, novoEstoque) = percorreEstoque remedio qtd estoque

-- Percorre o Estoque e retorna uma tupla indicando se encontrou o medicamento e, caso encontrado, o novo estoque.
percorreEstoque :: Medicamento -> Quantidade -> EstoqueMedicamentos -> (Bool, EstoqueMedicamentos)
percorreEstoque remedioProc quantidade ((remedio, qtd):estoque)
  | remedioProc == remedio = (True, (remedio, quantidade + qtd):listaRec)
  | otherwise = (valorRec, (remedio, qtd) : listaRec) where
    (valorRec, listaRec) = percorreEstoque remedioProc quantidade estoque
percorreEstoque remedioProc quantidade [] = (False, [])


{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento medicamento estoque = caminhaEstoque medicamento estoque False where
  -- Função auxiliar que encapsula a flag 'encontrado'
  caminhaEstoque :: Medicamento -> EstoqueMedicamentos -> Bool -> Maybe EstoqueMedicamentos
  caminhaEstoque remedioProc [] encontrado
    | encontrado = Just []
    | otherwise = Nothing
  caminhaEstoque remedioProc ((remedio, qtd):estoque) encontrado
    | remedioProc == remedio && qtd /= 0 = fmap ((remedio, qtd - 1):) (caminhaEstoque remedioProc estoque True)
    | otherwise = fmap ((remedio, qtd):) (caminhaEstoque remedioProc estoque encontrado) -- Fmap para manipular o Maybe


{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento remedioProc [] = 0
consultarMedicamento remedioProc ((remedio, quantidade) : estoque)
  | remedioProc == remedio = quantidade
  | otherwise = consultarMedicamento remedioProc estoque

{-

   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}


-- Merge sort com base no medicamento
ordenaEstoque :: EstoqueMedicamentos -> EstoqueMedicamentos
ordenaEstoque [] = []
ordenaEstoque [x] = [x]
ordenaEstoque ls = merge (ordenaEstoque esq) (ordenaEstoque dir) where
  (esq, dir) = splitAt (div (length ls) 2) ls
  merge (x:xs) (y:ys)
    | fst x <=  fst y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
  merge [] xs = xs
  merge xs [] = xs


-- Merge Sort com função de comparação (True -> A < B, False -> A > B)
ordenaPor :: (a -> a -> Bool) -> [a] -> [a]
ordenaPor compara [] = []
ordenaPor compara [x] = [x]
ordenaPor compara ls = merge (ordenaPor compara esquerda) (ordenaPor compara direita) where
  (esquerda, direita) = splitAt (div (length ls) 2) ls
  merge (x:xs) (y:ys)
    | compara x  y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys -- Swap das variáveis
  merge [] xs = xs
  merge xs [] = xs



demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos receituario = ordenaPor chave (map demanda receituario) where
  -- Demanda de cada medicamento
  demanda :: Prescricao -> (Medicamento, Quantidade)
  demanda (medicamento, horario) = (medicamento, length horario)
  -- Chave de comparação para a ordenação
  chave a b = fst a < fst b

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

-}

receituarioValido :: Receituario -> Bool
-- Passo recursivo
receituarioValido ((remedio_a, horario_a) : (remedio_b, horario_b) : receituario) =
  -- Condições para um receituario Válido
  (remedio_a < remedio_b)
  && estaOrdenado horario_a
  && receituarioValido ((remedio_b, horario_b) : receituario)
-- Casos Base
receituarioValido ((remedio_a, horario_a) : receituario) = estaOrdenado horario_a
receituarioValido [] = True

planoValido :: PlanoMedicamento -> Bool
-- Passo recursivo
planoValido ((horario_a, remedios_a) : (horario_b, remedios_b) : planoMedicamento) =
  -- Condições para um Plano Válido
  (horario_a < horario_b)
  && estaOrdenado remedios_a
  && planoValido ((horario_b, remedios_b) : planoMedicamento)
-- Casos Base
planoValido ((horario_a, remedios_a) : planoMedicamento) = estaOrdenado remedios_a
planoValido [] = True


{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

-- Verifica se uma lista de elementos está ordenada
estaOrdenado :: Ord a => [a] -> Bool
estaOrdenado (x:y:xs) = x < y && estaOrdenado (y:xs)
estaOrdenado _ = True

-- Verifica se os medicamentos dos cuidados são válidos
cuidadoValido :: ([Medicamento],[Medicamento]) -> Bool
cuidadoValido (medicar, []) = True
cuidadoValido (medicar, med:comprar) = notElem med medicar && cuidadoValido (medicar, comprar)

-- Separa os Cuidados por construtor e desencapsula seus medicamentos: Esquerda (medicar) e direita (Comprar)
separaCuidados :: [Cuidado] -> ([Medicamento], [Medicamento])
separaCuidados [] = ([],[])
separaCuidados (cuidado : cuidados) = case cuidado of
  Comprar remedio n -> (medicar, remedio : comprar)
  Medicar remedio ->  (remedio : medicar, comprar)
  where (medicar, comprar) = separaCuidados cuidados


plantaoValido :: Plantao -> Bool
-- Caso recursivo
plantaoValido ((horario_a, cuidados_a) : (horario_b, cuidados_b) : plantao) =
  -- Condições Para um plano válido
  horario_a < horario_b
  && cuidadoValido (medicar, comprar)
  && estaOrdenado medicar
  && plantaoValido ((horario_b, cuidados_b) : plantao)
  where (medicar, comprar) = separaCuidados cuidados_a
-- Casos Base
plantaoValido ((horario_a, cuidados_a) : plantao) = cuidadoValido (medicar, comprar)
  && estaOrdenado medicar
  && plantaoValido plantao
  where (medicar, comprar) = separaCuidados cuidados_a
plantaoValido [] = True

{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

  [(Medicamento, [Horario])] -> [[(Horario, [Medicamento])]
-}

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario = mesclaLista . inverteLista

-- Achata uma lista, invertendo a correspondência das tuplas (chave, valores) e ordenando-as
inverteLista :: (Ord a, Ord b) => [(a, [b])] -> [(b, [a])]
inverteLista lista = ordenaPor chave [(valor, [chave])|(chave, valores) <- lista, valor <- valores] where
    -- Chave de comparação para ordenação da lista
    chave (a, [b]) (c, [d]) = a <= c && b <= d

-- Mescla elementos de mesma chave (1ª posição da tupla)
mesclaLista :: Eq a => [(a, [b])] -> [(a, [b])]
mesclaLista ((chave_a, valores_a):(chave_b, valores_b):ls)
  | chave_a == chave_b = mesclaLista ((chave_a, valores_a ++ valores_b) : ls)
  | otherwise = (chave_a, valores_a) : mesclaLista ((chave_b, valores_b) : ls)
mesclaLista [tupla] = [tupla]

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano = mesclaLista . inverteLista

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}


executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao plantao = aplicaCuidados  listaCuidados where
  -- Aplica sequenciamente os cuidados do plantao
  aplicaCuidados :: [Cuidado] -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
  -- Casos Base
  aplicaCuidados [Comprar med qtd] estoque = Just (comprarMedicamento med qtd estoque) -- Reaproveitamento da Q1
  aplicaCuidados [Medicar med] estoque = tomarMedicamento med estoque -- Reaproveitamento da Q2
  -- Casos Recursivos
  aplicaCuidados ((Comprar med qtd):ls) estoque = Just (comprarMedicamento med qtd estoque) >>= aplicaCuidados ls
  aplicaCuidados ((Medicar med):ls) estoque = tomarMedicamento med estoque >>= aplicaCuidados ls -- Tratamento do Maybe com operador Bind
  -- Gera uma lista sequencial com os Cuidados do plantão 
  listaCuidados = [cuidado | (horario, cuidados) <- plantao, cuidado <- cuidados]

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

-- Transforma um Plantão Válido em um Plano de medicamentos
transformaPlantao :: Plantao -> PlanoMedicamento
transformaPlantao plantao = case plantao of
  [(horario, cuidado)] -> [(horario, map desencapsula (filter ehMedicar cuidado))]
  ((horario, cuidado):cuidados) -> (horario, map desencapsula (filter ehMedicar cuidado)) : transformaPlantao cuidados
  where
    -- Filtro para selecionar somente o construtor Medicar
    ehMedicar :: Cuidado -> Bool
    ehMedicar (Medicar _ ) = True
    ehMedicar _ = False
    -- Desencapsulamento do medicamento do construtor Medicar
    desencapsula :: Cuidado -> Medicamento
    desencapsula (Medicar m) = m

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoque = isJust (executaPlantao plantao estoque) -- Uso da Q9
  &&  transformaPlantao plantao == plano where
    isJust estoque = case estoque of
      Nothing -> False
      Just _ -> True


{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}


-- Constrói um plantão válido a partir do plano
plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto ((horario, medicamentos):plano) estoque = (horario, percorreMedicamentos horario medicamentos) : plantaoCorreto plano estoque where
  -- Percorre os medicamentos, comprando-os quando não há no estoque e Medicando os pacientes
  percorreMedicamentos :: Horario -> [Medicamento] -> [Cuidado] 
  percorreMedicamentos horario (med:meds) = case tomarMedicamento med estoque of -- Uso da Q2
    Nothing -> Comprar med 5 : Medicar med  : percorreMedicamentos horario meds
    Just _ ->  Medicar med : percorreMedicamentos horario meds
  percorreMedicamentos horario [] = []
plantaoCorreto [] _ = []


{-estoque1 :: EstoqueMedicamentos
estoque1 = [(med4, 10), (med6, 5), (med7, 0)]
plano1 :: PlanoMedicamento
plano1 = [(6, [med6]), (8, [med4]), (17, [med4]), (22, [med7])]
-}

