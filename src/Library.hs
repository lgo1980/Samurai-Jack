module Library where
import PdePreludat

-- Aku: Hace mucho tiempo, en una tierra distante, yo, Aku, el Amo de la oscuridad, desencadené un mal indescriptible.
-- Pero un tonto guerrero samurái que blande una espada mágica, decidió oponerse a mí. Antes del último golpe, abrí un
-- portal en el tiempo y lo envié al futuro, donde mi maldad es la ley. Ahora, el tonto intenta volver al pasado y 
-- alejarse del futuro que soy yo... ¡Aku!

-- Un grupo de fans de Samurai Jack, entusiasmados con las recientes noticias de la continuación de la serie, nos ha encargado
-- un sistema para simular que podría ocurrir en las nuevas temporadas de la serie de dibujos animados teniendo en cuenta todo 
-- lo que sabemos de las primeras temporadas. Decidimos modelar la información de la siguiente forma:

data Personaje = Personaje {
    nombre :: String,
    salud :: Number,
    elementos :: [Elemento],
    anioPresente :: Number 
} deriving (Show, Eq, Ord)

data Elemento = Elemento {
    tipo :: String,
    ataque :: Personaje -> Personaje,
    defensa :: Personaje -> Personaje
} deriving (Show, Eq, Ord)

-- Lo esperado es poder usar el efecto de ataque de un elemento sobre el rival y el de defensa sobre el personaje que lo tiene. 
-- En caso de que no se indique cuál es el efecto defensivo o el ofensivo, significa que no se altera de ninguna forma al personaje recibido.

-- 1)   Empecemos por algunas transformaciones básicas:
--      a) mandarAlAnio: lleva al personaje al año indicado.
mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {
    anioPresente = anio
}
--      b) meditar: le agrega la mitad del valor que tiene a la salud del personaje.
meditar :: Personaje -> Personaje
meditar personaje = modificarSaludPersonaje ((div 2 . salud) personaje) personaje

--      c) causarDanio: le baja a un personaje una cantidad de salud dada.
causarDanio :: Number -> Personaje -> Personaje
causarDanio danio = modificarSaludPersonaje (danio * (-1))

modificarSaludPersonaje :: Number -> Personaje -> Personaje
modificarSaludPersonaje saludAAgregar personaje =  personaje {
    salud = max 0 (salud personaje + saludAAgregar)
}

--  Hay que tener en cuenta al modificar la salud de un personaje que ésta nunca puede quedar menor a 0.
--  Importante: no repetir lógica.

-- 2) Queremos poder obtener algo de información extra sobre los personajes. Definir las siguientes funciones:
--      a) esMalvado, que retorna verdadero si alguno de los elementos que tiene el personaje en cuestión es de tipo “Maldad”.
esMalvado :: Personaje -> Bool
esMalvado = any ((== "Maldad").tipo).elementos

--      b) danioQueProduce :: Personaje -> Elemento -> Float, que retorne la diferencia entre la salud inicial del personaje y 
--          la salud del personaje luego de usar el ataque del elemento sobre él.
danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce personaje elemento = deltaSegun salud personaje (ataque elemento personaje)

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

--      c) enemigosMortales que dado un personaje y una lista de enemigos, devuelve la lista de los enemigos que pueden llegar 
--          a matarlo con un solo elemento. Esto sucede si luego de aplicar el efecto de ataque del elemento, el personaje queda con salud igual a 0.
enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (quedaSaludVacia personaje) enemigos

quedaSaludVacia :: Personaje -> Personaje -> Bool
quedaSaludVacia personaje enemigo = any (estaMuerto personaje) (elementos enemigo)

estaMuerto :: Personaje -> Elemento -> Bool
estaMuerto personaje = (== 0).danioQueProduce personaje

-- 3) Definir los siguientes personajes y elementos:
--      a) Definir concentracion de modo que se pueda obtener un elemento cuyo efecto defensivo sea aplicar meditar tantas veces como el nivel
--          de concentración indicado y cuyo tipo sea "Magia".
concentracion :: Number -> Elemento
concentracion nivel = generarElemento "Magia" id meditar

generarElemento :: String -> (Personaje -> Personaje) -> (Personaje -> Personaje) -> Elemento
generarElemento tipoEle ataqueEle defensaEle = Elemento {
    tipo = tipoEle,
    ataque = ataqueEle,
    defensa = defensaEle
}
--      b) Definir esbirrosMalvados que recibe una cantidad y retorna una lista con esa cantidad de esbirros (que son elementos de tipo “Maldad”
--          cuyo efecto ofensivo es causar un punto de daño).
esbirrosMalvados :: Number -> [Elemento]
esbirrosMalvados cantidadEsbirros = replicate cantidadEsbirros (generarElemento "Maldad" (causarDanio 1) id)
-- esbirrosMalvados cantidadEsbirros
--     | cantidadEsbirros == 0     = []
--     | otherwise                 = [generarElemento "Maldad" (causarDanio 1) id] ++ esbirrosMalvados (cantidadEsbirros - 1)

--      c) Definir jack de modo que permita obtener un personaje que tiene 300 de salud, que tiene como elementos concentración nivel 3 y una 
--          katana mágica (de tipo "Magia" cuyo efecto ofensivo es causar 1000 puntos de daño) y vive en el año 200.
generarPersonaje :: String -> Number -> [Elemento] -> Number -> Personaje
generarPersonaje nombrePer saludPer elementosPer anioPer= Personaje {
    nombre = nombrePer,
    salud = saludPer,
    elementos = elementosPer,
    anioPresente = anioPer 
}
jack :: Personaje
jack = generarPersonaje "Jack" 300 [concentracion 3, generarElemento "Magia" (causarDanio 1000) id] 200
--      d) Definir aku :: Int -> Float -> Personaje que recibe el año en el que vive y la cantidad de salud con la que debe ser construido. 
--          Los elementos que tiene dependerán en parte de dicho año. Los mismos incluyen:
--          i) Concentración nivel 4
--          ii) Tantos esbirros malvados como 100 veces el año en el que se encuentra.
--          iii) Un portal al futuro, de tipo “Magia” cuyo ataque es enviar al personaje al futuro (donde el futuro es 2800 años después 
--              del año indicado para aku), y su defensa genera un nuevo aku para el año futuro correspondiente que mantenga la salud que 
--              tenga el personaje al usar el portal.
aku :: Number -> Number -> Personaje
aku anio cantidadSalud = generarPersonaje "Aku" cantidadSalud (generarElementosAku anio) anio

generarElementosAku :: Number -> [Elemento]
generarElementosAku anio = concentracion 4 : esbirrosMalvados (100 * anio)

portalAlFuturoDesde anio = Elemento "Magia" (mandarAlAnio anioFuturo) (aku anioFuturo.salud)
  where anioFuturo = anio + 2800
  
-- 4) Finalmente queremos saber cómo puede concluir la lucha entre Jack y Aku. 
--    Para ello hay que definir la función luchar :: Personaje -> Personaje -> (Personaje, Personaje) donde se espera que si el primer personaje
--    (el atacante) está muerto, retorne la tupla con el defensor primero y el atacante después, en caso contrario la lucha continuará invirtiéndose
--    los papeles (el atacante será el próximo defensor) luego de que ambos personajes se vean afectados por el uso de todos los elementos del atacante.

--    O sea que si luchan Jack y Aku siendo Jack el primer atacante, Jack se verá afectado por el poder defensivo de la concentración y Aku se verá afectado 
--    por el poder ofensivo de la katana mágica, y la lucha continuará con Aku (luego del ataque) como atacante y con Jack (luego de la defensa) como defensor.
luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor 
    | salud atacante == 0   = (defensor,atacante)
    | otherwise             = luchar (aplicarAtaque defensor) (aplicarAtaque atacante)

aplicarAtaque :: Personaje -> Personaje
aplicarAtaque personaje = foldr (ataque) personaje (elementos personaje)
-- luchar :: Personaje -> Personaje -> (Personaje, Personaje)
-- luchar atacante defensor
--  |salud atacante == 0 = (defensor, atacante)
--  |otherwise = luchar proximoAtacante proximoDefensor
--  where proximoAtacante = usarElementos ataque defensor (elementos atacante)
--        proximoDefensor = usarElementos defensa atacante (elementos atacante)

-- -- Abstraemos cómo hacer para usar uno de los efectos de un conjunto de elementos sobre un personaje
-- usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
-- usarElementos funcion personaje elementos = foldl afectar personaje (map funcion elementos)

-- afectar personaje funcion = funcion personaje


-- 5) Inferir el tipo de la siguiente función:
-- f :: a -> (Number -> a) -> a -> [a] -> [b]
-- f x y z
--     | y 0 == z = map (fst.x z)
--     | otherwise = map (snd.x (y 0))
