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

subastar :: Accion
subastar unaPersona
  | tactica unaPersona == "Oferente Singular" && tactica unaPersona == "Accionista" = agregarPropiedad unaPersona propiedad
  | otherwise = unaPersona

agregarPropiedad :: Persona -> Propiedad -> Persona
agregarPropiedad unaPersona propiedad = unaPersona {propiedades = propiedades unaPersona ++[propiedad], dinero=dinero unaPersona -snd propiedad}