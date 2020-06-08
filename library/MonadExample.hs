-- la linea de abajo es para usar el typeclass Num
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadExample(dineroPersonajeValidado, construirPersonajeValidado, validarNombre, lichKing, Personaje(..), Validado(..)) where

-- Agregamos un validado para validar el exito o el error de una validacion
data Validado a = Exito a | Error String deriving (Show, Eq)


-- Con newtype definimos un nuevo tipo, el deriving lo dejamos mas adelante
newtype Oro = Oro Int
  deriving (Show, Eq, Num)

-- Tenemos tambien los type alias...
type Dinero = Oro

-- Despues esta el personake, que debe tener un nombre para empezar
data Personaje = Personaje {
  nombre :: String,
  dinero :: Dinero,
  salud :: Int
} deriving (Show, Eq)

-- Ahora tenemos una funcion que nos va a validar que el nombre no sea demasiado corto 
validarNombre :: String -> Validado String
validarNombre unNombre | length unNombre < 4 = Error "El nombre es muy corto"
                       | length unNombre > 20 = Error "El nombre es muy largo"
                       | otherwise = Exito unNombre


-- La idea seria de poder crear un personaje validado...                      
construirPersonajeValidado :: Validado String -> Dinero -> Validado Personaje
construirPersonajeValidado nombreValidado plata = case nombreValidado of
    Exito unNombre -> Exito (Personaje unNombre plata 100)
    Error mensajeDeError -> Error mensajeDeError
                      
-- Ahora podemos tener un personaje ya predefinido...
lichKing :: Validado Personaje
lichKing = construirPersonajeValidado (validarNombre "Arthas Menethil") 1000    


obtenerDineroPersonaje :: Personaje -> Dinero
obtenerDineroPersonaje unPersonaje = (dinero unPersonaje)

-- Ahora queremos obtener 
dineroPersonajeValidado :: Validado Personaje -> Validado Dinero
dineroPersonajeValidado unPersonaje = case unPersonaje of
    Exito personajeValidado -> Exito (obtenerDineroPersonaje personajeValidado)
    Error mensajeDeError -> Error mensajeDeError