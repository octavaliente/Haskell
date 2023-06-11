import Text.Show.Functions()

--A
data Persona = Persona {
    nombre :: String,
    calorias :: Int,
    indiceHidratacion :: Int,
    tiempo :: Int,
    equipamientos :: [Equipamiento]
} deriving Show

type Equipamiento = String

type Ejercicio = Persona -> Persona

abdominales :: Int -> Ejercicio
abdominales repeticiones = modificarCalorias (-8*repeticiones) 

flexiones :: Int -> Ejercicio
flexiones repeticiones = modificarCalorias (-16*repeticiones) . modificarHidratacion (-calcularIndice 2 repeticiones)

levantarPesas :: Int -> Int -> Ejercicio
levantarPesas repeticiones peso unaPersona 
    | tieneEquipamiento "pesa" unaPersona = modificarCalorias (-32*repeticiones) . modificarHidratacion (-calcularIndice peso repeticiones) $ unaPersona
    | otherwise = unaPersona

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson unaPersona = unaPersona

renovarEquipo :: Persona -> Persona
renovarEquipo unaPersona = modificarEquipamientos (equipamientosNuevos unaPersona) unaPersona

volverseYoguista :: Persona -> Persona
volverseYoguista unaPersona = modificarCalorias (div (calorias unaPersona) 2) . modificarHidratacion (indiceHidratacion unaPersona) . modificarEquipamientos (["Colchoneta"]) $ unaPersona

volverseBodyBuilder :: Persona -> Persona
volverseBodyBuilder unaPersona
    | soloPesas unaPersona = modificarCalorias ((*2) . calorias $ unaPersona) . modificarEquipamientos (equipamientosBB unaPersona) $ unaPersona
    | otherwise = unaPersona 

comerUnSandwich :: Persona -> Persona
comerUnSandwich  = modificarCalorias 500 . modificarHidratacion 100 
--Funciones auxiliares punto A

calcularIndice :: Int -> Int -> Int
calcularIndice unCoeficiente repeticiones = unCoeficiente * (div repeticiones 10)

tieneEquipamiento :: String -> Persona -> Bool
tieneEquipamiento unEquipamiento unaPersona = elem unEquipamiento (equipamientos unaPersona)

modificarHidratacion :: Int -> Persona -> Persona
modificarHidratacion unaCantidad unaPersona 
    | superaLimite unaCantidad unaPersona = unaPersona {indiceHidratacion = 100}
    | otherwise = unaPersona {indiceHidratacion = indiceHidratacion unaPersona + unaCantidad}

superaLimite :: Int -> Persona -> Bool
superaLimite unaCantidad unaPersona = (unaCantidad + indiceHidratacion unaPersona) >= 100 

modificarCalorias :: Int -> Persona -> Persona
modificarCalorias unaCantidad unaPersona = unaPersona {calorias = calorias unaPersona + unaCantidad}

modificarEquipamientos :: [Equipamiento] -> Persona -> Persona
modificarEquipamientos listaEquipamientos unaPersona = unaPersona {equipamientos = listaEquipamientos}

equipamientosNuevos :: Persona -> [Equipamiento]
equipamientosNuevos unaPersona = map ("Nuevo "++) (equipamientos unaPersona)

soloPesas :: Persona -> Bool
soloPesas unaPersona = all (esPesa) (equipamientos unaPersona)

esPesa :: String -> Bool
esPesa unEquipamiento = "pesa" == unEquipamiento

equipamientosBB :: Persona -> [Equipamiento]
equipamientosBB unaPersona = map (++" BB") (equipamientos unaPersona)

juan :: Persona
juan = Persona {nombre="", calorias = 0, indiceHidratacion = 0, tiempo = 0, equipamientos = ["pesa", "pesa"]}

--B
data Rutina = Rutina {
    duracion :: Int,
    ejercicios :: [Ejercicio]
} deriving Show

-- Lo primero que tenemos que tener en cuenta es que una persona no puede
-- hacer rutinas cuya duración aproximada sea mayor que su tiempo libre

esPeligrosa :: Rutina -> Persona -> Bool
esPeligrosa = tieneCualidades estaAgotada  

esBalanceada :: Rutina -> Persona -> Bool
esBalanceada unaRutina unaPersona = tieneCualidades (estaBien.calorias $ unaPersona) unaRutina unaPersona 

--Funciones Auxiliares B

tieneCualidades :: (Persona -> Bool) -> Rutina -> Persona -> Bool
tieneCualidades unaCualidad unaRutina = unaCualidad . hacerRutina unaRutina 

estaBien :: Int -> Persona -> Bool
estaBien caloriasIniciales = terminaCon (<(div caloriasIniciales 2)) (>80)

hacerRutina :: Rutina -> Persona -> Persona
hacerRutina unaRutina unaPersona = foldr (hacerEjercicio) unaPersona (ejercicios unaRutina) 

hacerEjercicio :: Ejercicio -> Persona -> Persona
hacerEjercicio unEjercicio unaPersona = unEjercicio unaPersona

estaAgotada :: Persona -> Bool
estaAgotada = terminaCon (<50) (<10) 

terminaCon :: (Int -> Bool) -> (Int -> Bool) -> Persona -> Bool
terminaCon condicionCalorias condicionHidratacion unaPersona = (condicionCalorias . calorias $ unaPersona) && (condicionHidratacion . indiceHidratacion $ unaPersona)

--C
