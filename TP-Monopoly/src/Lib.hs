import Text.Show.Functions ()

data Persona = UnaPersona {
    nombre :: Nombre,
    dinero :: Int,
    tactica :: String,
    propiedades :: [Propiedad],
    acciones :: [Accion]
} deriving Show

type Accion = (Persona -> Persona)
type Propiedad = (Nombre, Precio)
type Nombre = String
type Precio = Int

carolina :: Persona
carolina = UnaPersona "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Persona
manuel = UnaPersona "Manuel" 500 "Oferente Singular" [] [pasarPorElBanco, enojarse]

pasarPorElBanco :: Accion
pasarPorElBanco unaPersona = (cambiarTacticaACompradorCompulsivo.modificarDinero 40) unaPersona

modificarDinero :: Int -> Persona -> Persona
modificarDinero unMonto unaPersona = unaPersona {dinero = dinero unaPersona + unMonto}

cambiarTacticaACompradorCompulsivo :: Persona -> Persona
cambiarTacticaACompradorCompulsivo unaPersona = unaPersona {tactica = "Comprador Compulsivo"}

enojarse :: Accion
enojarse unaPersona = (agregarAccionGritar . modificarDinero 50) unaPersona

agregarAccionGritar :: Persona -> Persona
agregarAccionGritar unaPersona = unaPersona {acciones = acciones unaPersona ++[gritar]}

gritar :: Accion
gritar unaPersona = unaPersona {nombre = nombre unaPersona ++ "AHHHH"}

subastar :: Persona -> Propiedad -> Persona
subastar unaPersona unaPropiedad
  | tactica unaPersona == "Oferente Singular" || tactica unaPersona == "Accionista" = agregarPropiedad unaPersona unaPropiedad
  | otherwise = unaPersona

agregarPropiedad :: Persona -> Propiedad -> Persona
agregarPropiedad unaPersona propiedad = unaPersona {propiedades = propiedades unaPersona ++[propiedad], dinero= dinero unaPersona - snd propiedad}

cobrarAlquileres :: Accion
cobrarAlquileres unaPersona = unaPersona {dinero = dinero unaPersona + totalAlquileres unaPersona}

totalAlquileres :: Persona -> Int
totalAlquileres unaPersona = (sum.map obtenerPrecioPropiedad) $ propiedades unaPersona

obtenerPrecioPropiedad :: Propiedad -> Int
obtenerPrecioPropiedad (_,unPrecio) = montoASumar unPrecio

montoASumar :: Int -> Int
montoASumar unValor
  | esPropiedadCara unValor = 20
  | otherwise = 10

esPropiedadCara :: Int -> Bool
esPropiedadCara unValor = unValor >=150

pagarAAccionistas :: Accion
pagarAAccionistas unaPersona
  | tactica unaPersona == "Accionista" = modificarDinero 200 unaPersona
  | otherwise = modificarDinero (-100) unaPersona