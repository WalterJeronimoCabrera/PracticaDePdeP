import Text.Show.Functions
import Data.List

data Heroe = Heroe {
    nombre :: String,
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: Labor
} deriving (Show)

type Labor = [Tarea]
type Tarea = Heroe -> Heroe

jero = Heroe "Jeronimo" "ProMas" 100 [] []
julian = Heroe "Julian" "Laburante" 101 [] []
oriana = Heroe "Oriana" "laPro" 499 [inteligencia,calculadora] [cocer]
octavio = Heroe "Octavio" "Dormilon" 50 [inteligencia, celular, laLanzaDelOlimpo, xiphos] []

cocer :: Tarea
cocer heroe = cambiarArtefactosDelHeroe (maquinaDeCocer :) heroe

maquinaDeCocer = Artefacto "Tatakae" 350 
inteligencia = Artefacto "Inteligencia" 200
calculadora = Artefacto "Calculeitor" 20
celular = Artefacto "celular" 400


cambiarNombre :: String -> Tarea
cambiarNombre nuevoNombre heroe = 
    heroe {nombre = nuevoNombre}

cambiarEpiteto :: String -> Tarea
cambiarEpiteto nuevoEpiteto heroe = 
    heroe {epiteto = nuevoEpiteto}

cambiarReconocimiento :: (Int -> Int) -> Tarea 
cambiarReconocimiento funcion heroe = 
    heroe {reconocimiento = funcion . reconocimiento $ heroe}

cambiarArtefactosDelHeroe :: ([Artefacto] -> [Artefacto]) -> Tarea
cambiarArtefactosDelHeroe funcion heroe = 
    heroe {artefactos = funcion  (artefactos heroe)}

cambiarTarea :: ([Tarea] -> [Tarea]) -> Tarea
cambiarTarea funcion heroe = heroe {tareas = funcion . tareas $ heroe}

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

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto artefacto = 
    cambiarArtefactosDelHeroe (artefacto :) . cambiarReconocimiento (+ (rareza artefacto))

escalarElOlimpo :: Tarea
escalarElOlimpo = 
    cambiarReconocimiento (+500) . 
    cambiarArtefactosDelHeroe (elRelampagoDeZeus :) .
     cambiarArtefactosDelHeroe (filtrarArtefactos . map (cambiarRareza (*3)))

filtrarArtefactos :: [Artefacto] -> [Artefacto]
filtrarArtefactos lista = filter  ((1000 <). rareza) lista

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cantidadDeCuadras = 
    cambiarEpiteto ("Gros" ++ replicate cantidadDeCuadras 'o')

data Bestia = Bestia {
    nombreBestia :: String,
    debilidad :: Heroe -> Bool
} deriving (Show)

type Debilidad = Heroe -> Bool

matarUnaBestia bestia heroe
    | puedeMatar bestia heroe = cambiarEpiteto ("El asesino de " ++ (nombreBestia bestia))

puedeMatar bestia heroe = (debilidad bestia) heroe

-- 4)

pistola :: Artefacto
pistola = Artefacto "Pistola" 1000

elRelampagoDeZeus :: Artefacto
elRelampagoDeZeus = Artefacto "El relámpago de Zeus" 500

heracles = Heroe "Heracles" "Guardián del   Olimpo" 700 [pistola, elRelampagoDeZeus] [cocer]

-- 5)

leonDeMenea :: Bestia
leonDeMenea = Bestia "Leon de Nemea" ((>= 20) . length . epiteto ) 


--matarAlLeonDeMenea :: Tarea
matarAlLeonDeMenea heroe =  debilidad leonDeMenea $ heroe
--
-- 6)

hacerUnaTarea tarea = tarea . cambiarTarea (tarea :)

-- 7)

presumir heroe1 heroe2
    | reconocimiento heroe1    < reconocimiento heroe2    = (heroe2, heroe1)
    | reconocimiento heroe1    > reconocimiento heroe2    = (heroe1, heroe2)
    | sumatoriaDeRareza heroe1 < sumatoriaDeRareza heroe2 = (heroe2, heroe1)
    | sumatoriaDeRareza heroe1 > sumatoriaDeRareza heroe2 = (heroe1, heroe2)
    | otherwise                                           = presumir (hacerLaTareaDelOtro heroe1 heroe2) (hacerLaTareaDelOtro heroe2 heroe1) 


hacerLaTareaDelOtro heroe1 heroeQueRealizaLaTareaDelOtro =  realizarUnaLabor (tareas heroe1) heroeQueRealizaLaTareaDelOtro
 
sumatoriaDeRareza :: Heroe -> Int
sumatoriaDeRareza heroe = sum . (map rareza) $ (artefactos heroe)

listaDeArtefactos = [elRelampagoDeZeus, calculadora, inteligencia]
listaDeTareas = [cocer, escalarElOlimpo, ayudarACruzarLaCalle 8, encontrarUnArtefacto calculadora]
-- 8)


-- 9)

realizarUnaLabor :: Labor -> Heroe -> Heroe
realizarUnaLabor labor heroe = foldr hacerUnaTarea heroe labor
