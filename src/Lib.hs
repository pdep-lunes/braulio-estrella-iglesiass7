module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

type Poder = Personaje -> Personaje -> (Personaje, Personaje)

data Personaje = Personaje {
    nombre :: String 
    poderBasico :: Poder 
    superPoder :: Poder
    superActivo :: Bool
    vida :: Int 
} deriving (Show)

bolaEspinosa :: Poder 
bolaEspinosa atacante defensor = (atacante defensor { vida = max 0 (vida defensor - 1000) } )

lluviaDeTuercasSanadoras :: Poder 
lluviaDeTuercasSanadoras atacante aliado = (atacante, aliado {vida = vida aliado + 800} )

lluviaDeTuercasDañinas :: Poder 
lluviaDeTuercasDañinas atacante defensor = (atacante, defensor {vida = vida defensor div 2} )

granadaDeEspinas :: Int -> Poder 
granadaDeEspinas radio atacante defensor
| radio > 3 && radio defensor < 800 = (atacante { nombre = nombre atacante + “ estuvo aquí” }, defensor {superActivo = False, vida = 0})


| radio > 3 = (atacante {nombre = nombre atacante ++ "estuvo aqui"}, snd (bolaEspinosa atacante defensor))

| oterwhise = bolaEspinosa atacante defensor

torretaCurativa :: Poder 
torretaCurativa atacante aliado = (atacante, aliado { superActivo = True, vida = vida aliado * 2 })
