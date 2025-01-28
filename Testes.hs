module Testes where
    import ModeloDados

    med1 :: Medicamento
    med1 = "Adera"

    med2 :: Medicamento
    med2 = "Alprazolam"

    med3 :: Medicamento
    med3 = "Donepezila"

    med4 :: Medicamento
    med4 = "Lactulona"

    med5 :: Medicamento
    med5 = "Mirtazapina"

    med6 :: Medicamento
    med6 = "Pantoprazol"

    med7 :: Medicamento
    med7 = "Patz"

    med8 :: Medicamento
    med8 = "Quetiapina"

    med9 :: Medicamento
    med9 = "Xarelto"

    estoque1 :: EstoqueMedicamentos
    estoque1 = [(med4, 10), (med6, 5), (med7, 0)]

    estoque2 :: EstoqueMedicamentos
    estoque2 = [(med4, 10), (med6, 5), (med7, 10)]

    estoque3 :: EstoqueMedicamentos
    estoque3 = [(med4, 10), (med6, 50), (med7, 10), (med8, 20)]

    plantao1 :: Plantao
    plantao1 =
        [ (6, [Medicar med6]),
        (8, [Medicar med4]),
        (17, [Medicar med4]),
        (22, [Medicar med7])
        ]   

    plantao2 :: Plantao
    plantao2 =
        [ (6, [Medicar med6]),
        (8, [Medicar med4]),
        (17, [Medicar med4, Comprar med7 30]),
        (22, [Medicar med7])
        ]

    plantao3 :: Plantao
    plantao3 =
        [ (6, [Medicar med6, Medicar med9]),
        (8, [Medicar med2, Medicar med4]),
        (17, [Medicar med4, Comprar med7 30]),
        (22, [Medicar med7])
        ]

    plantaoInvalido1 :: Plantao
    plantaoInvalido1 =
        [ (6, [Medicar med6, Medicar med9]),
        (8, [Medicar med2, Medicar med4]),
        (22, [Medicar med7]),
        -- Invalido: não está em horario crescente
        (17, [Medicar med4, Comprar med7 30])
        ]

    plantaoInvalido2 :: Plantao
    plantaoInvalido2 =
        [ (6, [Medicar med6, Medicar med9]),
        (8, [Medicar med2, Medicar med4]),
        -- Invalido: comprar o mesmo medicamento no horário
        (17, [Medicar med4, Comprar med4 30]),
        (22, [Medicar med7])
        ]

    plantaoInvalido3 :: Plantao
    plantaoInvalido3 =
        [ (6, [Medicar med6, Medicar med9]),
        -- Invalido: ordem dos medicamentos não está ordenada
        (8, [Medicar med4, Medicar med2]),
        (17, [Medicar med4, Comprar med7 30]),
        (22, [Medicar med7])
        ]

    plantaoInvalido4 :: Plantao
    plantaoInvalido4 =
        [ (6, [Medicar med6]),
        -- Invalido: comprar o mesmo medicamento no horário
        (8, [Comprar med4 20, Medicar med4, Medicar med8]),
        (17, [Medicar med4]),
        (22, [Medicar med7, Medicar med8]),
        (23, [Medicar med8])
        ]
    
    receituario1 :: Receituario
    receituario1 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), ("Potz", [22, 6, 8, 17])]

    receituario2 :: Receituario
    receituario2 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med8, [8, 22, 23])]

    plano1 :: PlanoMedicamento
    plano1 = [(6, [med6]), (8, [med4]), (17, [med4]), (22, [med7])]

    plano2 :: PlanoMedicamento
    plano2 = [(6, [med6]), (8, [med4, med8]), (17, [med4]), (22, [med7, med8]), (23, [med8])]
    

    