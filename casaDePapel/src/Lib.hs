import Text.Show.Functions()
import Data.List()


data Ladron = Ladron {
    nombre :: String,
    habilidades :: [String],
    armas :: [Arma]
} deriving (Show)

cambiarNombre :: String -> Ladron -> Ladron
cambiarNombre nuevoNombre ladron = ladron {nombre = nuevoNombre }

cambiarHabilidades :: ([String] -> [String]) -> Ladron -> Ladron
cambiarHabilidades funcion ladron = ladron {habilidades = funcion . habilidades $ ladron}

cambiarArmas :: ([Arma] -> [Arma]) -> Ladron -> Ladron
cambiarArmas funcion ladron = ladron {armas = funcion . armas $ ladron}

data Rehen = Rehen {
    nombreRehen :: String,
    nivelComplot :: Int,
    nivelMiedo :: Int,
    planes :: [Plan]    
} deriving (Show)

cambiarNombreRehen :: String -> Rehen -> Rehen
cambiarNombreRehen nuevoNombre rehen = rehen {nombreRehen = nuevoNombre}

disminuirNivelComplot :: (Int -> Int) -> Rehen -> Rehen
disminuirNivelComplot funcion rehen = rehen {nivelComplot = funcion . nivelComplot $ rehen}

aumentarNivelMiedo :: Int -> Rehen -> Rehen
aumentarNivelMiedo valor rehen = rehen {nivelMiedo = (nivelMiedo rehen) + valor}

cambiarPlan :: ( [Plan] -> [Plan] ) -> Rehen -> Rehen
cambiarPlan funcion rehen = rehen {planes = funcion . planes $ rehen}

-- Armas

type Arma = Rehen -> Rehen

pistola :: Int -> Arma
pistola calibre rehen = disminuirNivelComplot ((5 * calibre)+) . aumentarNivelMiedo (3 * (cantidadSegun nombreRehen rehen) ) $ rehen

--cantidadSegun :: (Rehen a, Ladron a) => (a -> Int) ->  -> Int
cantidadSegun funcion alguien = length . funcion $ alguien

ametralladora :: Int -> Arma
ametralladora balas rehen = disminuirNivelComplot (`div` 2) . aumentarNivelMiedo (balas ) $ rehen

-- Planes

type Plan = Ladron -> Ladron

esconderse :: Plan
esconderse ladron = cambiarArmas (quitarCantidadSegunDiv habilidades 3 ladron) ladron

atacarAlLadron :: Rehen -> Plan
atacarAlLadron compa ladron = cambiarArmas (quitarCantidadSegunDiv nombreRehen 10 compa) ladron


--quitarCantidadSegunDiv :: (Ladron a, Rehen a) => (a -> a) -> Int -> a
quitarCantidadSegunDiv funcion valor alguien = drop . ( `div` valor ) . (cantidadSegun funcion) $ alguien

-- 1) 

tokio :: Ladron
tokio = Ladron "Tokio" ["trabajo psicológico", "entrar en moto"] [pistola 9, ametralladora 30]

profesor :: Ladron
profesor = Ladron "Profesor" ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelante"] []

pablo :: Rehen
pablo = Rehen "Pablo" 40 30 [esconderse] 

arturito :: Rehen
arturito = Rehen "Arturito" 70 50 [esconderse, atacarAlLadron pablo]

--2)

esInteligente :: Ladron -> Bool
esInteligente ladron = (>2) . cantidadSegun habilidades $ ladron

--3)

conseguirArma :: Arma -> Ladron -> Ladron
conseguirArma arma ladron = cambiarArmas (arma :) ladron

--4)

-- Intimidar

disparos :: Ladron -> Rehen ->Rehen
disparos ladron rehen =  foldr (maxNivelMiedo) (rehen) (map ( $ rehen) (armas ladron))

maxNivelMiedo :: Rehen -> Rehen -> Rehen
maxNivelMiedo rehen1 rehen2 
    | nivelMiedo rehen1 > nivelMiedo rehen2 = rehen1
    | otherwise = rehen2

hacerseElMalo :: Ladron -> Rehen -> Rehen
hacerseElMalo ladron rehen
    | nombre ladron == "Berlín" = aumentarNivelMiedo (cantidadSegun (concat . habilidades ) ladron) rehen
    | nombre ladron == "Rio"    = disminuirNivelComplot ( 20 +) rehen
    | otherwise                 = aumentarNivelMiedo 10 rehen

intimidar :: (Ladron -> Rehen -> Rehen) -> Ladron -> Rehen -> Rehen
intimidar tecnicaDeIntimmidacion ladron rehen = tecnicaDeIntimmidacion ladron rehen

-- 5)

calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas ladron grupoDeRehenes = map (disparos ladron) (rehenesIntimidados grupoDeRehenes)

rehenesIntimidados :: [Rehen] -> [Rehen]
rehenesIntimidados grupoDeRehenes = filter ((60 <) . nivelComplot) grupoDeRehenes

listaDeRehenes = [pablo, arturito]

listaDeLadrones = [profesor, tokio]

-- 6)

puedeEscapar :: Ladron -> Bool
puedeEscapar ladron = any ((== "disfrazarse de") . (take 14)) (habilidades ladron)

-- 7)
laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal unosLadrones unosRehenes = 
    promedio (map nivelComplot unosRehenes) > (promedio (map nivelMiedo unosRehenes)) * sum (map (cantidadSegun armas) unosLadrones) 

promedio :: [Int] -> Int
promedio lista = div (sum lista) (length lista)

--8)

seRebelan :: [Rehen] -> Ladron -> Ladron
seRebelan unosRehenes ladron = foldr (ejecutarPlan) (ladron) (concatMap planes (map (disminuirNivelComplot (10 -)) unosRehenes))

ejecutarPlan :: Plan -> Ladron -> Ladron
ejecutarPlan plan ladron = plan ladron 

-- 9)

todosSeRebelan :: [Rehen] -> [Ladron] -> [Ladron]
todosSeRebelan unosRehenes unosLadrones = map (seRebelan unosRehenes) unosLadrones

todosSeArman :: [Ladron] -> [Ladron]
todosSeArman unosLadrones = map (cambiarArmas ( ametralladora 45 :)) unosLadrones

planValencia:: [Rehen] -> [Ladron] -> Int
planValencia unosRehenes unosLadrones = ( * 1000000) . sum . map (cantidadSegun armas) . todosSeRebelan unosRehenes . todosSeArman $ unosLadrones

--10) se genera un overflout 
-- 11) 