module MonadExample where

-- Agregamos un validado para validar el exito o el error de una validacion
data Validado a = Exito a | Error String deriving Show


-- Con newtype definimos un nuevo tipo, el deriving lo dejamos mas adelante
newtype Oro = Oro Int
  deriving (Show)

-- Tenemos tambien los type alias...
type Dinero = Oro

-- Despues esta el personake, que debe tener un nombre para empezar
data Personaje = Personaje {
  nombre :: String,
  dinero :: Dinero,
  salud :: Int
} deriving Show

-- Ahora tenemos una funcion que nos va a validar que el nombre no sea demasiado corto 
validarNombre :: String -> Validado String
validarNombre unNombre | length unNombre < 4 = Error "El nombre es muy corto"
                       | length unNombre > 20 = Error "El nombre es muy largo"
                       | otherwise = Exito unNombre


-- La idea seria de poder crear un personaje validado...                      
construirPersonajeValidado :: Validado String -> Dinero -> Validado Personaje
construirPersonajeValidado nombreValidado = undefined                       
