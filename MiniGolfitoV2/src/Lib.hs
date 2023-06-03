{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.Show.Functions ()
import Distribution.SPDX (LicenseId(MPL_2_0))

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f) 
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--1
type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = setearTiro 10 (precisionTotal unaHabilidad 2 (*)) 0

madera :: Palo
madera unaHabilidad = setearTiro 100 (precisionTotal unaHabilidad 2 div) 5

hierro :: Int -> Palo
hierro n unaHabilidad = setearTiro (fuerzaJugador unaHabilidad * n) (precisionTotal unaHabilidad n div) (alturaMinima n)

alturaMinima :: Int -> Int
alturaMinima n
    | n-3 < 0 = 0
    | otherwise = n-3

precisionTotal :: Habilidad -> Int -> (Int -> Int -> Int) -> Int
precisionTotal unaHabilidad unNum operador = precisionJugador unaHabilidad `operador` unNum

setearTiro :: Int -> Int -> Int -> Tiro
setearTiro unaVelocidad unaPrecision unaAltura = UnTiro {velocidad = unaVelocidad, precision = unaPrecision, altura = unaAltura}

palos :: [Palo]
palos = [putter,madera] ++ palosHierro

palosHierro :: [Palo]
palosHierro = map (\unNum -> hierro unNum) [1..10]

--palosHierro :: [Palo]
--palosHierro = [hierro 1,hierro 2,hierro 3,hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

--2
golpe :: Palo -> Jugador -> Tiro
golpe unPalo = unPalo . habilidad 

--3
type Obstaculo = Tiro -> Tiro

tunelConRampita :: Obstaculo
tunelConRampita unTiro = obstaculoMayor (condicionTunel unTiro) (velocidad unTiro * 2) 100 0

condicionTunel :: Tiro -> Bool
condicionTunel unTiro = precision unTiro > 90 && altura unTiro == 0

laguna :: Int -> Obstaculo
laguna largoLaguna unTiro = obstaculoMayor (condicionLaguna unTiro) (velocidad unTiro) (precision unTiro) (div (altura unTiro) largoLaguna)

condicionLaguna :: Tiro -> Bool
condicionLaguna unTiro = velocidad unTiro > 80 && between 1 5 (altura unTiro)

obstaculoMayor :: Bool -> Int -> Int -> Int -> Tiro
obstaculoMayor condicion velocidad precision altura
    | condicion = setearTiro velocidad precision altura
    | otherwise = tiroNulo 

--De todas formas se para
hoyo :: Obstaculo
hoyo unTiro = tiroNulo

tiroNulo :: Tiro
tiroNulo = UnTiro {velocidad=0,precision=0,altura=0}

--4
{-
type Obstaculo = Tiro -> Tiro
type Palo = Habilidad -> Tiro
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
}
data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} 
-}
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (esUtil unJugador unObstaculo) palos 

esUtil :: Jugador -> Obstaculo -> Palo -> Bool
esUtil unJugador unObstaculo unPalo = (unObstaculo . unPalo . habilidad) unJugador /= tiroNulo

puedeSuperar :: [Obstaculo] -> Tiro -> Int
puedeSuperar listaObstaculos unTiro = length . takeWhile (pasoObstaculo unTiro) $ listaObstaculos

pasoObstaculo :: Tiro -> Obstaculo -> Bool
pasoObstaculo unTiro unObstaculo = unObstaculo unTiro /= tiroNulo

--maximoSegun f lista= foldl1 (mayorSegun f) lista
--maximoSegun me daban la funcion ya creada

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador listaObstaculos = maximoSegun (utilidadDeUnPalo unJugador listaObstaculos) palos

utilidadDeUnPalo :: Jugador -> [Obstaculo] -> Palo -> Int
utilidadDeUnPalo unJugador listaObstaculos unPalo = puedeSuperar listaObstaculos (unPalo.habilidad $ unJugador)

--5
padresPerdedores :: [(Jugador, Int)] -> [String]
padresPerdedores listaNinos = map (padre.fst) (perdedores listaNinos)

perdedores :: [(Jugador, Int)] -> [(Jugador, Int)]
perdedores listaNinos = filter (noEsGanador listaNinos) listaNinos

noEsGanador :: [(Jugador, Int)] -> (Jugador, Int) -> Bool
noEsGanador listaNinos (_,puntos) = puntajeGanador listaNinos > puntos 

puntajeGanador:: [(Jugador, Int)] -> Int
puntajeGanador listaNinos = snd . maximoSegun snd $ listaNinos