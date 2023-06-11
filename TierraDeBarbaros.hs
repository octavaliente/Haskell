import Text.Show.Functions
import Data.Char (toUpper, isUpper)

---------- Funciones Auxiliares ----------

mapNombre :: (String -> String) -> Barbaro -> Barbaro
mapNombre f unBarbaro = unBarbaro { nombre = f . nombre $ unBarbaro }

mapFuerza :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerza f unBarbaro = unBarbaro { fuerza = f . fuerza $ unBarbaro }

mapHabilidades :: ([String] -> [String]) -> Barbaro -> Barbaro
mapHabilidades f unBarbaro = unBarbaro { habilidades = f . habilidades $ unBarbaro }

mapObjetos :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
mapObjetos f unBarbaro = unBarbaro { objetos = f . objetos $ unBarbaro }

setObjetos :: [Objeto] -> Barbaro -> Barbaro
setObjetos unosObjetos unBarbaro = mapObjetos (const unosObjetos) unBarbaro

---------- Funciones Auxiliares ----------

-- Punto 1

data Barbaro = Barbaro {
  nombre :: String,
  fuerza :: Int,
  habilidades :: [String],
  objetos :: [Objeto]
} deriving Show

type Objeto = Barbaro -> Barbaro

dave :: Barbaro
dave = Barbaro "Dave" 100 ["tejer", "escribirPoesia"] [ardilla, libroPedKing]

-- Punto 1.1

espada :: Int -> Objeto
espada pesoEspada = mapFuerza (+ pesoEspada * 2)

-- Punto 1.2

amuletoMistico :: String -> Objeto
amuletoMistico = aprenderHabilidad

aprenderHabilidad :: String -> Objeto
aprenderHabilidad habilidad = mapHabilidades (habilidad :)

-- Punto 1.3

varitaDefectuosa :: Objeto
varitaDefectuosa = aprenderHabilidad "hacerMagia" . setObjetos [varitaDefectuosa]

-- Punto 1.4

ardilla :: Objeto
ardilla unBarbaro = unBarbaro

-- Punto 1.5

cuerda :: Objeto -> Objeto -> Objeto
cuerda unObjeto otroObjeto = unObjeto . otroObjeto

-- Punto 2

megafono :: Objeto
megafono unBarbaro = mapHabilidades amplificar unBarbaro

amplificar :: [String] -> [String]
amplificar unasHabilidades = [map toUpper . concat $ unasHabilidades]

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono

libroPedKing :: Objeto
libroPedKing = undefined

-- Punto 3

type Aventura = [Evento]
type Evento = Barbaro -> Bool

-- Punto 3.1

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes unBarbaro = sabe "Escribir Poesía Atroz" unBarbaro

sabe :: String -> Barbaro -> Bool
sabe unaHabilidad unBarbaro = elem unaHabilidad . habilidades $ unBarbaro

-- Punto 3.2

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = not . tienePulgares . nombre $ unBarbaro

tienePulgares :: String -> Bool
tienePulgares "Faffy" = False
tienePulgares "Astro" = False
tienePulgares _       = True

-- Punto 3.3

type Prueba = Barbaro -> Bool

ritualesDeFechorias :: [Prueba] -> Evento
ritualesDeFechorias pruebas unBarbaro = pasa any unBarbaro pruebas

-- Punto 3.3.a

saqueo :: Prueba
saqueo unBarbaro = sabe "robar" unBarbaro && fuerza unBarbaro > 80

-- Punto 3.3.b

gritoDeGuerra :: Prueba
gritoDeGuerra unBarbaro = poderDeGrito unBarbaro > largoDeHabilidades unBarbaro

largoDeHabilidades :: Barbaro -> Int
largoDeHabilidades unBarbaro = length . concat . habilidades $ unBarbaro

poderDeGrito :: Barbaro -> Int
poderDeGrito unBarbaro = (* 4) . length . objetos $ unBarbaro

-- Punto 3.3.c

caligrafia :: Prueba
caligrafia unBarbaro = all (\unaHabilidad -> tieneMasDe3Vocales unaHabilidad && empiezaConMayuscula unaHabilidad) . habilidades $ unBarbaro

tieneMasDe3Vocales :: String -> Bool
tieneMasDe3Vocales = (> 3) . length . filter esVocal

esVocal :: Char -> Bool
esVocal char = elem char "AEIOUaeiou"

empiezaConMayuscula :: String -> Bool
empiezaConMayuscula unaPalabra = isUpper . head $ unaPalabra

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes unosBarbaros unaAventura = filter (sobrevive unaAventura) unosBarbaros

sobrevive :: Aventura -> Barbaro -> Bool
sobrevive unaAventura unBarbaro = pasa all unBarbaro unaAventura

pasa unCriterio unBarbaro unasPruebas = unCriterio ($ unBarbaro) unasPruebas

-- Punto 4

-- Punto 4.a

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (cabeza : cola)
  | elem cabeza cola = sinRepetidos cola
  | otherwise        = cabeza : sinRepetidos cola

-- Punto 4.b

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = tail $ iterate descendiente unBarbaro

descendiente :: Barbaro -> Barbaro
descendiente = utilizarObjetos . mapNombre (++ "*") . mapHabilidades sinRepetidos

utilizarObjetos :: Barbaro -> Barbaro
utilizarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro)

-- Punto 4.c

{-
  ¿Se podría aplicar sinRepetidos sobre la lista de objetos?

    No. La función sinRepetidos espera una lista de valores que sean comparables por igualdad. Dado que los objetos son funciones, estos no son Eq, por lo que no tiparía.

  ¿Y sobre el nombre de un bárbaro?

    Si. El nombre de un bárbaro es un String, lo cual es lo mismo que una lista de tipo Char. Dado que Char pertenece a Eq, tipa.
-}
