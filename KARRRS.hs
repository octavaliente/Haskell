import Text.Show.Functions

---------- Datas y Tipos ----------

data Auto = Auto {
  nombre :: Nombre,
  nafta :: Nafta,
  velocidad :: Velocidad,
  enamorade :: Enamorade,
  truco :: Truco
} deriving Show

type Nombre = String
type Nafta = Int
type Velocidad = Int
type Enamorade = String
type Truco = Carrera -> Auto -> Auto
type Distancia = Int

data Carrera = Carrera {
  vueltas :: Vueltas,
  longitudPista :: LongitudPista,
  participantes :: Participantes,
  publico :: Publico
} deriving Show

type Vueltas = Int
type LongitudPista = Int
type Participantes = [Auto]
type Publico = [String]

---------- Datas y Tipos ----------

---------- Funciones Auxiliares ----------

mapNafta :: (Nafta -> Nafta) -> Auto -> Auto
mapNafta f unAuto = unAuto { nafta = f . nafta $ unAuto }

mapVelocidad :: (Velocidad -> Velocidad) -> Auto -> Auto
mapVelocidad f unAuto = unAuto { velocidad = f . velocidad $ unAuto }

mapParticipantes :: (Participantes -> Participantes) -> Carrera -> Carrera
mapParticipantes f unaCarrera = unaCarrera { participantes = f . participantes $ unaCarrera }

minimumBy :: (Ord b) => (a -> b) -> [a] -> a
minimumBy = foldl1 . menorSegun

mejorSegun :: (Ord b) => (b -> b -> Bool) -> (a -> b) -> a -> a -> a
mejorSegun comp f a b
  | comp (f a) (f b) = a
  | otherwise        = b

menorSegun :: (Ord b) => (a -> b) -> a -> a -> a
menorSegun = mejorSegun (<)

maximumBy :: (Ord b) => (a -> b) -> [a] -> a
maximumBy = foldl1 . mayorSegun

mayorSegun :: (Ord b) => (a -> b) -> a -> a -> a
mayorSegun = mejorSegun (>)

applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf criterio funcion elemento
  | criterio elemento = funcion elemento
  | otherwise = elemento

mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf = (.) map . applyIf

---------- Funciones Auxiliares ----------

-- Punto 1

rochaMcQueen :: Auto
rochaMcQueen = Auto "rochaMcQueen" 282 0 "Ronco" deReversaRocha

biankerr :: Auto
biankerr = Auto "biankerr" 378 0 "Tincho" impresionar

gushtav :: Auto
gushtav = Auto "gushtav" 100 0 "Peti" nitro

rodra :: Auto
rodra = Auto "rodra" 153 0 "Tais" comboLoco

-- Punto 2

deReversaRocha :: Truco
deReversaRocha = mapNafta . (+) . (* 5) . longitudPista

enamoradeEstaEnElPublico :: Carrera -> Auto -> Bool
enamoradeEstaEnElPublico unaCarrera unAuto = elem (enamorade unAuto) . publico $ unaCarrera

impresionar :: Truco
impresionar unaCarrera = applyIf (enamoradeEstaEnElPublico unaCarrera) (mapVelocidad (* 2))

nitro :: Truco
nitro _ = mapVelocidad (+ 15)

comboLoco :: Truco
comboLoco unaCarrera = deReversaRocha unaCarrera . nitro unaCarrera

-- Punto 3

restarNafta :: Distancia -> Auto -> Auto
restarNafta unaDistancia unAuto = (mapNafta . (-) . (* unaDistancia) . largoNombre) unAuto unAuto

restarNaftaParticipantes :: Carrera -> Carrera
restarNaftaParticipantes unaCarrera = (mapParticipantes . map . restarNafta . longitudPista) unaCarrera unaCarrera

largoNombre :: Auto -> Int
largoNombre = length.nombre

nombreMasCortoQue :: Int -> Auto -> Bool
nombreMasCortoQue unLargo = (<= unLargo) . largoNombre

cuantoAumenta :: Auto -> Velocidad
cuantoAumenta unAuto
  | nombreMasCortoQue 5 unAuto = 15
  | nombreMasCortoQue 8 unAuto = 20
  | otherwise = 30

aumentarVelocidad :: Auto -> Auto
aumentarVelocidad unAuto = (mapVelocidad . (+) . cuantoAumenta) unAuto unAuto

aumentarVelocidadParticipantes :: Carrera -> Carrera
aumentarVelocidadParticipantes = mapParticipantes . map $ aumentarVelocidad

autoMasLento :: Carrera -> Nombre
autoMasLento = nombre . minimumBy velocidad . participantes

usarTruco :: Carrera -> Auto -> Auto
usarTruco unaCarrera unAuto = (truco unAuto) unaCarrera unAuto

esElMasLento :: Carrera -> Auto -> Bool
esElMasLento unaCarrera = (== autoMasLento unaCarrera) . nombre

usarTrucoDelMasLento :: Carrera -> Carrera
usarTrucoDelMasLento unaCarrera = mapParticipantes (mapIf (esElMasLento unaCarrera) (usarTruco unaCarrera)) unaCarrera

darVuelta :: Carrera -> Carrera
darVuelta = usarTrucoDelMasLento . aumentarVelocidadParticipantes . restarNaftaParticipantes

-- Punto 4

correrCarrera :: Carrera -> Carrera
correrCarrera unaCarrera = foldr ($) unaCarrera $ replicate (vueltas unaCarrera) darVuelta

-- Punto 5

ganador :: Carrera -> Auto
ganador = maximumBy velocidad . participantes . correrCarrera

-- Punto 6

recompetidores :: Carrera -> Participantes
recompetidores unaCarrera = filter (puedeCorrerDeNuevo unaCarrera) . participantes . correrCarrera $ unaCarrera

puedeCorrerDeNuevo :: Carrera -> Auto -> Bool
puedeCorrerDeNuevo unaCarrera unAuto = quedoConNafta unAuto || esElGanador unaCarrera unAuto

quedoConNafta :: Auto -> Bool
quedoConNafta = (> 27) . nafta

esElGanador :: Carrera -> Auto -> Bool
esElGanador unaCarrera = (== (nombre . ganador) unaCarrera) . nombre

-- Punto 7

{-
  Luego tenemos la carrera ultra suprema de las altas ligas que tiene una cantidad infinita de participantes!
  De ella queremos saber:

  a) ¿Podemos correrla?

    No. *

  b) ¿Podemos conocer el primer participante luego de 2 vueltas?

    No. *

  c) ¿Podemos dar la primera vuelta de la carrera?

    No. *

  * No puede terminar de dar una vuelta porque para lo último que hace debe buscar al auto en la última posición, lo cual no terminaría nunca.
-}

