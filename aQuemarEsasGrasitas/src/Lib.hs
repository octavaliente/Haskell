import Text.Show.Functions ()

data Persona = Persona{
    edad :: Int,
    peso :: Int,
    tonificacion :: Int
} deriving Show

pancho :: Persona
pancho = Persona {edad=40, peso=120, tonificacion=1}

andres :: Persona
andres = Persona {edad=22, peso=80, tonificacion=6}

type Ejercicio = Int-> Persona -> Persona

saludable :: Persona -> Bool
saludable unaPersona = (not.estaObeso) unaPersona && estaTonificado unaPersona 

estaObeso :: Persona -> Bool
estaObeso = (>100) . peso 

estaTonificado :: Persona -> Bool
estaTonificado = (>5) . tonificacion 

quemarCalorias :: Persona -> Int -> Persona
quemarCalorias unaPersona unasCalorias
    | estaObeso unaPersona                                                     = unaPersona {peso= peso unaPersona - div unasCalorias 150}
    | (not.estaObeso) unaPersona && edad unaPersona > 30 && unasCalorias > 200 = unaPersona {peso= peso unaPersona - 1}
    | otherwise                                                                = unaPersona {peso= peso unaPersona - div unasCalorias (edad unaPersona * peso unaPersona)}

--3 a
caminata :: Ejercicio
caminata unosMinutos unaPersona = quemarCalorias unaPersona (calcularCalorias 5 unosMinutos)

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta unosMinutos unaPersona = quemarCalorias unaPersona (caloriasTotales unosMinutos 6)

caloriasTotales :: Int -> Int -> Int
caloriasTotales unosMinutos unaVelocidad
    | unosMinutos >= 5 = calcularCalorias unaVelocidad 5 + caloriasTotales (unosMinutos - 5) (unaVelocidad + 1)
    | otherwise        = calcularCalorias unaVelocidad unosMinutos

calcularCalorias :: Int -> Int -> Int
calcularCalorias unaVelocidad unosMinutos = unaVelocidad * unosMinutos

--3 b
pesas :: Ejercicio
pesas unosMinutos unaPersona  
    | unosMinutos > 10 = tonificarKilos unaPersona
    | otherwise = unaPersona

tonificarKilos :: Persona -> Persona
tonificarKilos unaPersona = tonificar (div (peso unaPersona) 10) unaPersona

--3 c
colina :: Int -> Ejercicio
colina unaInclinacion unosMinutos unaPersona = quemarCalorias unaPersona (unaInclinacion * unosMinutos * 2)

--3 d
montana :: Int -> Ejercicio
montana unaInclinacion unosMinutos = tonificar 1 . (colina (unaInclinacion + 3) (div unosMinutos 2) . colina unaInclinacion (div unosMinutos 2)) 

tonificar :: Int -> Persona -> Persona
tonificar unIncremento unaPersona = unaPersona {tonificacion = tonificacion unaPersona + unIncremento}

--4 a
type Rutina = (String, Int, [Ejercicio])

--Recursividad
realizarRutina :: Rutina -> Persona -> Persona
realizarRutina (_,_,[]) unaPersona = unaPersona
realizarRutina (unNombre ,unosMinutos, x:xs) unaPersona = realizarRutina (unNombre, unosMinutos, xs) (x unosMinutos unaPersona)

--Fold
realizarRutinaFold :: Rutina -> Persona -> Persona
realizarRutinaFold (_ ,unosMinutos, listaEjercicios) unaPersona = foldr (realizarEjercicio unosMinutos) unaPersona listaEjercicios

realizarEjercicio :: Int -> Ejercicio -> Persona -> Persona
realizarEjercicio unosMinutos unEjercicio = unEjercicio unosMinutos 

--Ej: 
{-
ghci > realizarRutinaFold (trenSuperior, 5, [montana 2, colina 1, pesas, caminata]) andres
-}

--4b
resumenRutina :: Rutina -> Persona -> (String,Int,Int)
resumenRutina unaRutina unaPersona = (nombreRutina unaRutina, efectoRutina unaRutina peso unaPersona,- efectoRutina unaRutina tonificacion unaPersona) 

nombreRutina :: Rutina -> String
nombreRutina (nombre,_,_) = nombre

efectoRutina :: Rutina -> (Persona -> Int) -> Persona -> Int
efectoRutina unaRutina funcionCantidad unaPersona = funcionCantidad unaPersona - (funcionCantidad . realizarRutinaFold unaRutina) unaPersona

--5
rutinaBuena :: [Rutina] -> Persona -> [Rutina]
rutinaBuena listaRutinas unaPersona = filter (saludable . flip realizarRutinaFold unaPersona) listaRutinas