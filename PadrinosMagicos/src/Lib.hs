{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.Show.Functions ()
import Language.Haskell.TH (prim)
import System.Win32 (xBUTTON1)

data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
} deriving Show

type Habilidad = String

type Deseo = Chico -> Chico

elPipa :: Chico
elPipa = Chico {nombre="pipa",edad=34,habilidades=[""], deseos=[]}

--A
aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades listaHabilidades unChico = unChico {habilidades=listaHabilidades ++ habilidades unChico}

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = aprenderHabilidades listaNFS unChico

listaNFS :: [Habilidad]
listaNFS = map (\unNumero -> "jugar need for speed " ++ show unNumero) [1..]

serMayor :: Deseo
serMayor unChico = cambiarEdad (-edad unChico +18) unChico

type Padrino = Chico -> Chico

wanda :: Padrino
wanda unChico = cambiarEdad 1 . cumplirDeseo (primerDeseo unChico) $ unChico

primerDeseo :: Chico -> Deseo
primerDeseo = head . deseos 

cumplirDeseo :: Deseo -> Chico -> Chico
cumplirDeseo unDeseo unChico = unDeseo unChico 

cosmo :: Padrino
cosmo unChico = cambiarEdad (-div (edad unChico) 2) unChico

cambiarEdad :: Int -> Chico -> Chico
cambiarEdad unaEdad unChico = unChico {edad = edad unChico + unaEdad}

muffinMagico :: Padrino
muffinMagico unChico = foldr cumplirDeseo unChico (deseos unChico)
--B
type Condicion = Chico -> Bool

tieneHabilidad :: Habilidad -> Condicion
tieneHabilidad unaHabilidad unChico = elem unaHabilidad (habilidades unChico)

esSuperMaduro :: Condicion
esSuperMaduro unChico = esMayorDeEdad unChico && tieneHabilidad "manejar" unChico

esMayorDeEdad :: Chico -> Bool
esMayorDeEdad unChico = edad unChico >= 18

data Chica = Chica {
    nombreChica :: String,
    condicion :: Condicion
} deriving Show

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica listaChicos 
    | any (condicion unaChica) listaChicos = head (filter (condicion unaChica) listaChicos)
    | otherwise = last listaChicos

chicaEj :: Chica
chicaEj = Chica {nombreChica="Manuela", condicion= tieneHabilidad "cocinar"}

--C
infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules listaChicos = map nombre . filter tieneDeseoProhibido $ listaChicos 

tieneDeseoProhibido :: Chico -> Bool
tieneDeseoProhibido unChico = tieneHabilidadesProhibidas . habilidades . muffinMagico $ unChico

tieneHabilidadesProhibidas :: [Habilidad] -> Bool
tieneHabilidadesProhibidas listaDeHabilidades = any habilidadProhibida (take 5 listaDeHabilidades)

habilidadProhibida :: Habilidad -> Bool
habilidadProhibida unaHabilidad = elem unaHabilidad ["enamorar","matar","dominar el mundo"]

--D
{-
Se usaron listas infinitas en listaNFS
No funciona por ejemplo, serGrosoEnNeedForSpeed ya que nunca termina
Funcionaria perfectamente en infractoresDeDaRules, ya que toma las primeras 5 habilidades
-}