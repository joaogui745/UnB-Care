## UnBCare: Sistema de Gerenciamento de Medicamentos

O projeto UnBCare, escrito em Haskell, tem como objetivo auxiliar na gestão de medicamentos. Ele oferece funcionalidades que simulam o processo de compra, administração e controle de estoque de medicamentos, além de permitir a geração de planos de administração e a validação da conformidade dos dados.

O arquivo `ModeloDados.hs` define os tipos de dados que representam as informações do sistema. No arquivo `UnBCare.hs`, encontra-se a implementação das funções que manipulam esses dados. Por fim, o arquivo `Testes.hs` contém uma série de dados para testar o funcionamento das funções implementadas no sistema.

Para executar o projeto UnBCare, é necessário ter um ambiente Haskell configurado com o GHC instalado. Após clonar o repositório do projeto, basta compilar o arquivo `UnBCare.hs` com o comando `ghc UnBCare.hs` e executá-lo com `./UnBCare`. O módulo `Testes.hs` contém testes para as funções do sistema, permitindo verificar seu funcionamento.
