import Text.Show.Functions
import Data.List

data Heroe = Heroe {
    nombre :: String,
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto]
} deriving (Show, Eq)

jero = Heroe "Jeronimo" "ProMas" 1500 [inteligencia]
julian = Heroe "Julian" "Laburante" 950 [inteligencia, celular, laLanzaDelOlimpo]
oriana = Heroe "Oriana" "laPro" 499 [inteligencia,calculadora]
octavio = Heroe "Octavio" "Dormilon" 50 [inteligencia, celular, laLanzaDelOlimpo, xiphos]

inteligencia = Artefacto "Inteligencia" 200
calculadora = Artefacto "Calculeitor" 20
celular = Artefacto "celular" 400


cambiarNombre :: String -> Heroe -> Heroe
cambiarNombre nuevoNombre heroe = 
    heroe {nombre = nuevoNombre}

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto nuevoEpiteto heroe = 
    heroe {epiteto = nuevoEpiteto}

cambiarReconocimiento :: (Int -> Int) -> Heroe -> Heroe 
cambiarReconocimiento funcion heroe = 
    heroe {reconocimiento = funcion . reconocimiento $ heroe}

cambiarArtefactosDelHeroe :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
cambiarArtefactosDelHeroe funcion heroe = 
    heroe {artefactos = funcion  (artefactos heroe)}


data Artefacto = Artefacto {
    nombreArtefacto :: String,
    rareza :: Int
} deriving (Show, Eq)

cambiarRareza funcion artefacto = artefacto {rareza = funcion . rareza $ artefacto }

xiphos = Artefacto "Xiphos" 50
laLanzaDelOlimpo = Artefacto "la lanza del Olimpo" 100

--2)

pasarALaHistoria heroe
    | (reconocimiento heroe) > 1000 = cambiarEpiteto "El mítico" heroe
    | reconocimiento heroe < 500 && reconocimiento heroe > 100 = 
        (cambiarEpiteto "Hoplita" . cambiarArtefactosDelHeroe (xiphos :)) heroe
    | reconocimiento heroe >= 500 = 
        (cambiarEpiteto "El magnífico" . cambiarArtefactosDelHeroe (laLanzaDelOlimpo :)) heroe
    | otherwise = heroe

--3)

encontrarUnArtefacto :: Artefacto -> Heroe -> Heroe
encontrarUnArtefacto artefacto = 
    cambiarArtefactosDelHeroe (artefacto :) . cambiarReconocimiento (+ (rareza artefacto))

escalarElOlimpo :: Heroe -> Heroe
escalarElOlimpo heroe = 
    cambiarReconocimiento (+500) . cambiarArtefactosDelHeroe filtrarArtefactos . cambiarArtefactosDelHeroe (map (cambiarRareza (*3))) $ heroe

filtrarArtefactos :: [Artefacto] -> [Artefacto]
filtrarArtefactos lista = filter  ((1000 <). rareza) lista

ayudarACruzarLaCalle :: Int -> Heroe -> Heroe
ayudarACruzarLaCalle cantidadDeCuadras heroe = 
    cambiarEpiteto ("Gros" ++ replicate cantidadDeCuadras 'o') heroe

data Bestia = Bestia {
    nombreBestia :: String,
    debilidad :: Heroe -> Bool
} deriving (Show)

type Debilidad = Heroe -> Bool


leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon de Nemea" (matarAlLeonDeMenea) 


matarAlLeonDeMenea :: Debilidad
matarAlLeonDeMenea heroe = length (epiteto heroe) >= 20



