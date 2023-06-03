import Control.Monad.Cont (cont)
data Participante = Participante { 
    nombre :: String,
    trucos :: [Truco],
    especialidad :: Plato
} 

data Plato = Plato {
    dificultad :: Int,
    componentes :: [Componente]
}

type Componente = (String, Int)
type Truco = Plato -> Plato 

endulzar :: Int -> Truco
endulzar gramos = agregarComponente ("Azucar", gramos)

salar :: Int -> Truco
salar gramos = agregarComponente ("Sal", gramos)

darSabor :: Int -> Int -> Truco
darSabor gramosSal gramosAzucar = salar gramosSal . endulzar gramosAzucar

agregarComponente :: Componente -> Truco
agregarComponente unComponente = modificarComponente (unComponente :) 

duplicarPorcion :: Truco
duplicarPorcion = modificarComponente (map duplicarCantidad)

duplicarCantidad :: Componente -> Componente
duplicarCantidad (ingrediente,cantidad) = (ingrediente, cantidad*2)

modificarComponente :: ([Componente]->[Componente]) -> Plato -> Plato
modificarComponente unaFuncion unPlato = unPlato {componentes = unaFuncion . componentes $ unPlato}

simplificar :: Truco
simplificar unPlato
    | esUnBardo unPlato = modificarComponente (filter ((>10).snd)) $ unPlato {dificultad = 5}
    | otherwise = unPlato

esUnBardo :: Plato -> Bool
esUnBardo unPlato = length (componentes unPlato) > 5 && dificultad unPlato > 7

caracteristicasPlato :: Plato -> (Componente -> Bool) -> Bool
caracteristicasPlato unPlato caracteristica= any caracteristica (componentes unPlato) 

esVegano :: Plato -> Bool
esVegano unPlato = caracteristicasPlato unPlato (\(nombre,_)-> elem nombre ["Carne","Huevos","Lacteo"] )

esVegano' :: Plato -> Bool
esVegano' = not . any esProductoAnimal . componentes 

esProductoAnimal :: Componente -> Bool
esProductoAnimal (ingrediente, _) = elem ingrediente ["Carne", "Huevos", "Lacteo"] 

esSinTacc :: Plato -> Bool
esSinTacc unPlato = caracteristicasPlato unPlato (\(nombre,_)-> nombre == "Harina")

esComplejo :: Plato -> Bool
esComplejo unPlato
    | length (componentes unPlato) > 5 && dificultad unPlato > 7 = True
    | otherwise = False

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = any (hipertension) (componentes unPlato)

hipertension :: Componente -> Bool
hipertension (nombre, cantidad) = nombre == "Sal" && cantidad > 2

--Parte B
pepe :: Participante
pepe = Participante {nombre="Pepe Ronccino",trucos=[darSabor 2 5, simplificar, duplicarPorcion], especialidad=platoPepe}

platoPepe :: Plato
platoPepe = Plato {dificultad = 8, componentes = [("Sal", 3), ("Azucar", 5),("Carne", 5),("Lacteo", 4),("Huevo", 8),("Pimienta", 5)]}

--Parte C
cocinar :: Participante -> Plato
cocinar = aplicarTrucos

aplicarTrucos :: Participante -> Plato
aplicarTrucos unParticipante = foldr aplicarTruco (especialidad unParticipante) (trucos unParticipante) 

aplicarTruco :: Truco -> Plato -> Plato
aplicarTruco unTruco unPlato = unTruco unPlato

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = esMasSegun unPlato otroPlato dificultad (>) && esMasSegun unPlato otroPlato pesoTotal (<)

esMasSegun :: Plato -> Plato -> (Plato -> Int) -> (Int -> Int -> Bool) -> Bool
esMasSegun unPlato otroPlato comparador operadorLogico =
    comparador unPlato `operadorLogico` comparador otroPlato

pesoTotal :: Plato -> Int
pesoTotal unPlato = sum . map snd $ componentes unPlato

--participanteEstrella :: [Participante] -> Participante
--participanteEstrella listaParticipantes = foldr1 (mejorParticipante) (map cocinar listaParticipantes)

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante unParticipante otroParticipante
    | especialidad unParticipante `esMejorQue` especialidad otroParticipante = unParticipante
    | otherwise = otroParticipante