import Text.Show.Functions ()
import Data.Char (toUpper)

data Barbaro = Barbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [String],
    objetos :: [Objeto]
} deriving Show

type Objeto = Barbaro -> Barbaro

--1
dave :: Barbaro
dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

espadas :: Int -> Objeto
espadas = aumentarFuerza  

aumentarFuerza ::  Int -> Barbaro -> Barbaro
aumentarFuerza unPeso unBarbaro = unBarbaro {fuerza = fuerza unBarbaro + 2*unPeso}

amuletosMisticos :: String -> Objeto
amuletosMisticos = otorgarHabilidad  

otorgarHabilidad :: String -> Objeto
otorgarHabilidad unaHabilidad unBarbaro = unBarbaro {habilidades = unaHabilidad : habilidades unBarbaro}

varitasDefectuosas :: Objeto
varitasDefectuosas = quitarOtrosObjetos . otorgarHabilidad "magia"

quitarOtrosObjetos :: Barbaro -> Barbaro
quitarOtrosObjetos unBarbaro = unBarbaro {objetos=[varitasDefectuosas]}

ardilla :: Objeto
ardilla unBarbaro = unBarbaro

cuerda :: Objeto -> Objeto -> Objeto
cuerda unObjeto otroObjeto = unObjeto . otroObjeto   

--2 durisimo
megafono :: Objeto
megafono = potenciarBarbaro 

potenciarBarbaro :: Barbaro -> Barbaro
potenciarBarbaro unBarbaro = unBarbaro {habilidades = [concatMap pasarAMayus (habilidades unBarbaro)]}

pasarAMayus :: String -> String
pasarAMayus = map toUpper

--3
type Evento = Barbaro -> Bool

type Aventura = [Evento]

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes = tieneHabilidad "Escribir Poesia Atroz" 

tieneHabilidad :: String -> Barbaro -> Bool
tieneHabilidad unaHabilidad unBarbaro = elem unaHabilidad (habilidades unBarbaro)

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = es "Faffy" unBarbaro || es "Astro" unBarbaro

es :: String -> Barbaro -> Bool
es unNombre unBarbaro = unNombre == nombre unBarbaro

ritualDeFechorias :: Evento
ritualDeFechorias unBarbaro = saqueo unBarbaro || gritoDeGuerra unBarbaro || caligrafia unBarbaro

saqueo :: Barbaro -> Bool
saqueo unBarbaro = tieneHabilidad "robar" unBarbaro && fuerza unBarbaro > 80

gritoDeGuerra :: Barbaro -> Bool
gritoDeGuerra unBarbaro = cantLetras unBarbaro == 4 * cantObjetos unBarbaro

cantLetras :: Barbaro -> Int
cantLetras = length . habilidades . megafono

cantObjetos :: Barbaro -> Int
cantObjetos = length . objetos

caligrafia :: Barbaro -> Bool
caligrafia unBarbaro = cantVocales unBarbaro > 3 && inicioMayus unBarbaro 

cantVocales :: Barbaro -> Int
cantVocales = sumarVocales . concat . habilidades

sumarVocales :: String -> Int
sumarVocales = length . filter esVocal  

esVocal :: Char -> Bool
esVocal unCaracter = elem unCaracter ['a','e','i','o','u','A','E','I','O','U']

inicioMayus :: Barbaro -> Bool
inicioMayus unBarbaro = all iniciaConMayus (habilidades unBarbaro)

iniciaConMayus :: [Char] -> Bool
iniciaConMayus (x:xs) = x == toUpper x

--Una aventura puede ser : [invasionDeSuciosDuendes, cremalleraDelTiempo]

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes listaBarbaros unaAventura = filter (sobrevive unaAventura) listaBarbaros

sobrevive :: Aventura -> Barbaro -> Bool
sobrevive unaAventura unBarbaro = all (pasaEvento unBarbaro) unaAventura

pasaEvento :: Barbaro -> Evento -> Bool
pasaEvento unBarbaro unEvento = unEvento unBarbaro

--4
--A :'(
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (filter (/= x) xs)

--B
--listaNFS = map (\unNumero -> "jugar need for speed " ++ show unNumero) [1..]

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = 
    map (\unNumero -> (aplicarObjetos unBarbaro) {nombre = nombre unBarbaro ++ replicate unNumero '*', habilidades = sinRepetidos (habilidades unBarbaro)}) [1..] 

aplicarObjetos :: Barbaro -> Barbaro
aplicarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro)

--C
--No se podria aplicar debido al tipo de sinRepetidos que es Eq
--Se podria aplicar al ser strings