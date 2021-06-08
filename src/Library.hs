module Library where
import PdePreludat

--Kalinin Alexander

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Number,
  superficie :: Number,
  precio :: Number,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior



ruso=Depto{
 ambientes= 3
,superficie= 80
, precio= 7500
, barrio=    "Palermo"
}

lean=Depto{
 ambientes= 2
,superficie= 180
, precio= 7200
, barrio=    "Palermo"
}

deptosDeEjemplo = [
 Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]

 --Punto 1)
 --a)


mayor:: Ord a => (t -> a) -> t -> t -> Bool
mayor funcion aValor  =  (funcion aValor >). funcion 

menor:: Ord a => (t -> a) -> t -> t -> Bool
menor funcion aValor  =  (funcion aValor <). funcion 

listaRandom = ["aa","aaaaa","aaa","a"]


--b)
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

--"EJEMPLO" ordenarSegun (menor (length)) ["asd","as"] -> ["as","asd"] c


--Punto 2)

--a)
ubicadoEn :: [Barrio]->Requisito
ubicadoEn listaBarrios depto = any (estaElBarrio depto)  listaBarrios

estaElBarrio :: Depto->Barrio->Bool
estaElBarrio  depto  = (barrio depto ==) 

--b)

cumpleRango :: (Depto->Number)->Number->Number->Requisito
cumpleRango funcion n1 n2  =  between n1 n2 . funcion 


--Punto 3)

--a)


--Filtrar todos los q cumplen la condicion para despues comparar la cantidad incial de requisitos con la cantidad que filtre
cumpleBusqueda :: Busqueda->Requisito
cumpleBusqueda requisitos  =   (==length requisitos ).length . filtrarLasQueCumplen requisitos 


filtrarLasQueCumplen :: Busqueda->Depto->Busqueda
filtrarLasQueCumplen requisitos depto = filter (condicionDeBusqueda depto) requisitos

condicionDeBusqueda :: Depto->Requisito->Bool
condicionDeBusqueda depto requisito = requisito depto

--b)

buscar :: Busqueda->(Depto->Depto->Bool)->[Depto]->[Depto]
buscar busqueda criteriOrd  =  ordenarSegun criteriOrd . cumplirBusqueda busqueda  


cumplirBusqueda :: Busqueda->[Depto]->[Depto]
cumplirBusqueda busqueda  = filter (filtrarLasQueCumplenBusqueda busqueda) 


filtrarLasQueCumplenBusqueda :: Busqueda->Depto->Bool
filtrarLasQueCumplenBusqueda busqueda departamento = all (condicionDeBusqueda departamento) busqueda

--Falta agregar al caso C que el ambiente sea menor a x numero
--c) buscar ([ubicadoEn ["Palermo","Recoleta"], cumpleRango (ambientes) 0 3, cumpleRango (precio) 0 6000 ]) (mayor (superficie ))  deptosDeEjemplo


--Punto 4)

mailsDePersonasInteresadas :: Depto->[Persona]->[Mail]
mailsDePersonasInteresadas depto personas = map mail (filtrarPersonasQueCumplan depto personas ) --Obtener la lista de mails

filtrarPersonasQueCumplan :: Depto->[Persona]->[Persona]
filtrarPersonasQueCumplan depto personas = filter (cumplirRequisitosPersonas depto) personas --Filtrar las personas que cumplan con alguna de sus Busquedas

cumplirRequisitosPersonas :: Depto->Persona->Bool
cumplirRequisitosPersonas depto persona = any  (cumplirLaBusqueda depto) (busquedas persona) --Fijarse que algun requisito de la lista de busqueda se cumpla  [Requisito1, Rq2, Rq3]

cumplirLaBusqueda :: Depto->Busqueda->Bool
cumplirLaBusqueda depto requisitos=  cumpleBusqueda requisitos  depto  --Comprar bar si los requisitos se cumplen [ubicado en ["palermo","recoleta"]]


augustoo = Persona {
    mail = "augustocrack@gmail.com",
    busquedas = [busquedaEj]
}

leann = Persona {
    mail = "leancrack@gmail.com",
    busquedas = [busquedaEj,busquedaEj2]
}

busquedaEj = [ubicadoEn ["Palermo","Recoleta"],cumpleRango ambientes 0 2, cumpleRango precio 0 6000]
busquedaEj2 = [ubicadoEn ["Villa Urquiza"],cumpleRango ambientes 0 5, cumpleRango precio 0 4000]

deptoEj2 = Depto 3 20 3500 "Villa Urquiza"



------------------------resolucion dif---------------------------------------------------------

{-
mailsDePersonasInteresadas :: Depto -> [Persona] -> [String]
mailsDePersonasInteresadas depto = map (mailDePersonaInteresada depto)

mailDePersonaInteresada :: Depto -> Persona -> String
mailDePersonaInteresada depto persona
    | all (==[]) (cumpleAlgunaBusqueda depto persona) = "No esta interesada"
    | otherwise = mail persona

cumpleAlgunaBusqueda :: Depto -> Persona -> [[Depto]]
cumpleAlgunaBusqueda depto persona = map (deptosQueCumplenBusqueda [depto]) (busquedas persona)
-}
