module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

type Poder = Personaje -> Personaje -> (Personaje, Personaje)

data Personaje = Personaje {
    nombre :: String, 
    poderBasico :: Poder, 
    superPoder :: Poder,
    superActivo :: Bool,
    vida :: Int 
} deriving (Show)

bolaEspinosa :: Poder 
bolaEspinosa atacante defensor = (atacante, defensor { vida = max 0 (vida defensor - 1000) } )

lluviaDeTuercasSanadoras :: Poder 
lluviaDeTuercasSanadoras atacante aliado = (atacante, aliado {vida = vida aliado + 800} )

lluviaDeTuercasDañinas :: Poder 
lluviaDeTuercasDañinas atacante defensor = (atacante, defensor {vida = vida defensor `div` 2} )

granadaDeEspinas :: Int -> Poder
granadaDeEspinas radio atacante defensor
    | radio > 3 && vida defensor < 800 =
        (atacante,
         defensor {nombre = nombre defensor ++ " - Espina estuvo aquí", superActivo = False, vida = 0 })
    | radio > 3 =
        (atacante, defensor {nombre = nombre defensor ++ " - Espina estuvo aquí"})
    | otherwise = bolaEspinosa atacante defensor

torretaCurativa :: Poder 
torretaCurativa atacante aliado = (atacante, aliado { superActivo = True, vida = vida aliado * 2 })

atacarConSuper :: Poder
atacarConSuper atacante defensor
  | superActivo atacante = 
        (uncurry (poderBasico atacante) . superPoder atacante atacante) defensor
    | otherwise = (atacante, defensor)

estanEnLasUltimas :: [Personaje] -> [String]
estanEnLasUltimas personajes = map nombre (filter vidaMenorA800 personajes)

vidaMenorA800 :: Personaje -> Bool
vidaMenorA800 personaje = vida personaje < 800

espina :: Personaje
espina = Personaje {
    nombre = "Espina",
    poderBasico = bolaEspinosa,
    superPoder = granadaDeEspinas 5,
    superActivo = True,
    vida = 4800
}

pamela :: Personaje
pamela = Personaje {
    nombre = "Pamela",
    poderBasico = lluviaDeTuercasSanadoras,
    superPoder = torretaCurativa,
    superActivo = False,
    vida = 9600
}