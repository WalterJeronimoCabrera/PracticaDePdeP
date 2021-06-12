import Text.Show.Functions
import Data.List
import Data.Char


{-Punto 1

Se sabe que los bárbaros tienen nombre, fuerza, habilidades y objetos, que los ayudarán más adelante en su lucha contra el mal. Por ejemplo: 

dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]
-}

data Barbaro = Barbaro { 
    nombre :: String,
    fuerza :: Int,
    habilidades :: [String],
    objetos :: [Objeto]
} deriving (Show)

dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]
jero = Barbaro "Jero**" 1000 ["nada", ""] [ardilla]
astro = Barbaro "Astro" 100 ["nadar"] [ardilla, espada 10]
julian = Barbaro "Julian" 50 ["Escribir Poesía Atroz"] [ardilla]


{-
Se pide definir los siguientes objetos y definir algunos bárbaros de ejemplo
1.Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso. -}

type Objeto = Barbaro -> Barbaro

espada :: Int -> Objeto
espada peso barbaro = barbaro {fuerza = fuerza barbaro + 2 * peso }
--espada peso (Barbaro nombre fuerza habilidades objetos) = Barbaro nombre (fuerza + 2 * peso) habilidades objetos


{-2.Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.-}

agregarHabilidad :: String -> Objeto
agregarHabilidad habilidad barbaro = barbaro {habilidades = habilidad : habilidades barbaro }


amuletosMisticos :: String -> Objeto 
amuletosMisticos habilidad barbaro = agregarHabilidad habilidad barbaro
--amuletosMisticos habilidad (Barbaro nombre fuerza habilidades objetos) = Barbaro nombre fuerza (habilidad : habilidades) objetos

{-3.Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.-}

varitasDefectuosas :: Objeto
varitasDefectuosas barbaro = vaciarObjetos . agregarHabilidad "hacerMagia" $ barbaro
--varitasDefectuosas (Barbaro nombre fuerza habilidades objetos) = Barbaro nombre fuerza (agregarHabilidad "hacerMagia") []

vaciarObjetos :: Objeto
vaciarObjetos barbaro = barbaro {objetos = []}

{-4.Una ardilla, que no hace nada.-}

ardilla :: Objeto 
ardilla = id

{-5.Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos. 
-}

cuerda :: Objeto -> Objeto -> Objeto
cuerda = (.)



--Punto 2
--El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas. 
{-*Main> megafono dave
Barbaro "Dave" 100 ["TEJERESCRIBIRPOESIA"] [<function>,<function>]
Sabiendo esto, definir al megafono, y al objeto megafonoBarbarico, 
que está formado por una cuerda, una ardilla y un megáfono. -}


megafono :: Objeto
megafono barbaro = barbaro {habilidades = [map toUpper (concat (habilidades barbaro))] }

--megafono (Barbaro nombre fuerza habilidades objetos) = 
  --  Barbaro nombre fuerza ([map toUpper (concat habilidades)]) objetos

megafonoBarbarico :: Objeto
megafonoBarbarico barbaro = cuerda ardilla megafono $barbaro



--Punto 3 - Aventuras 

type Eventos = Barbaro -> Bool
type Aventura = [Eventos]
--Los bárbaros suelen ir de aventuras por el reino luchando contra las fuerzas del mal, pero ahora que tienen nuestra ayuda, quieren que se les diga si un grupo de bárbaros puede sobrevivir a cierta aventura.  Una aventura se compone de uno o más eventos, por ejemplo:

--1.invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”


invasionDeSuciosDuendes :: Eventos
invasionDeSuciosDuendes barbaro = tieneHabilidad "Escribir Poesía Atroz" barbaro


tieneHabilidad :: String -> Eventos
tieneHabilidad habilidad barbaro = elem habilidad (habilidades barbaro)

--2.cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los bárbaros llamados Faffy y Astro no tienen pulgares, los demás sí. 

cremalleraDelTiempo :: Eventos
cremalleraDelTiempo barbaro = noTienePulgares barbaro


--nombre barbaro "Faffy" = 

noTienePulgares :: Eventos
noTienePulgares barbaro
  | (nombre barbaro) == "Faffy" = True
  | (nombre barbaro) == "Astro" = True
  | otherwise = False

{-ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes: 
saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus habilidades. El poder necesario para aprobar es 4 veces la cantidad de objetos del bárbaro.
caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) si sus habilidades contienen más de 3 vocales y comienzan con mayúscula.
Sabiendo esto, se pide:
Definir los eventos, modelar las aventuras y dar un ejemplo. 
Definir la función sobrevivientes que tome una lista de bárbaros y una aventura, y diga cuáles bárbaros la sobreviven (es decir, pasan todas las pruebas)-}

ritualDeFechorias :: Barbaro -> [Eventos] -> Bool
ritualDeFechorias barbaro pruebas = any (sobrevive barbaro) pruebas

sobrevive :: Barbaro -> Eventos -> Bool
sobrevive barbaro prueba = prueba barbaro

--sobrevivientes :: [Barbaro] -> Eventos -> [Barbaro]
sobrevivientes listaDeBarbaros aventuras = foldr ((:). map aventuras) [] listaDeBarbaros
--filter aventura listaDeBarbaros sobrevivientes [dave, jero, julian, astro] ritualDeFechorias


{-Punto 4 - Dinastía

A - Los bárbaros se marean cuando tienen varias habilidades iguales. Por todo esto, nos piden desarrollar una función que elimine los elementos repetidos de una lista (sin utilizar nub ni nubBy)

> sinRepetidos [1,2,3,4,4,5,5,6,7]
[1,2,3,4,5,6,7]

Nota: Puede usarse recursividad para este punto.-}

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs)
  | elem x xs = sinRepetidos xs
  | otherwise = x : sinRepetidos xs


{-B - Los bárbaros son una raza muy orgullosa, tanto que quieren saber cómo van a ser
 sus descendientes y asegurarse de que los mismos reciban su legado.
El descendiente de un bárbaro comparte su nombre, y un asterisco por cada generación.
Por ejemplo "Dave*", "Dave**" , "Dave***" , etc. 
Además, tienen en principio su mismo poder, habilidades sin repetidos, y
los objetos de su padre, pero antes de pasar a la siguiente generación, utilizan 
(aplican sobre sí mismos) los objetos. Por ejemplo, el hijo de Dave será equivalente a:
(ardilla.varitasDefectuosas) (Barbaro "Dave*" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas])

Definir la función descendientes, que dado un bárbaro nos de sus infinitos descendientes. -}


--C. Pregunta: ¿Se podría aplicar sinRepetidos sobre la lista de objetos? ¿Y sobre el nombre de un bárbaro? ¿Por qué?

-----------------------------------------------------------------------------------------------------------