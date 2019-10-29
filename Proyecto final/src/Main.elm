module Main exposing (..)

import Canvas
import CanvasColor as Color exposing (Color)
import Html exposing (Html,div,button,text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Browser

type Puntoses = Snowflake | Sierpinsky | Puntos 

type alias Modelo = {fract : Puntoses, repte : Int}

modeloprincipal : Modelo
modeloprincipal = {fract = Puntos , repte = 0}

type Mensaje = Fract Puntoses | Repte Int 

refrescar : Mensaje -> Modelo -> Modelo
refrescar mensaje modelo = case mensaje of 
                             Fract l -> {fract = l, repte = 0}
                             Repte l -> {fract = modelo.fract, repte = 
                                  if l == 1 then (modelo.repte) + 1 else 
                                  if modelo.repte == 0 then modelo.repte 
                                  else (modelo.repte) - 1}

puntos3 = [
            (300, 45),
            (500, 425),
            (100, 425),(300, 45),(300, 45)]

--Programacion funcion Snowflake 
coordenadas1 x y = case (x , y) of  --Coordenadas que corresponderan al primer punto de o los triangulos que se dibujaran
                    ((x1, y1), (x2, y2)) -> (((x1 + (1/2) * x2) / (1 + (1/2))), ((y1 + (1/2) * y2) / (1 + (1/2)))) 

coordenadas2 x y = case (x , y) of 
                    ((x1, y1), (x2, y2)) -> (((x1 + (2 * x2)) / (3)), ((y1 + (2 * y2)) / (3))) 

coordenadasomaximo : (Float , Float) -> (Float , Float)     --Coordenadas del punto que se situara a 60 grados del punto "coordenadas1" y a 60 grados del punto "coordenadas2"
coordenadasomaximo x = case x of
                    (x1 , y1) -> (((x1 * (cos(degrees 60))) - y1 * (sin(degrees 60))), ((x1 * sin(degrees 60) + y1 * cos(degrees 60))))

trasladar x y = case (x, y) of
                    ((x1, y1), (x2, y2)) -> ((x1 + x2), (y1 + y2))

rotarcoordenadasos : (Float, Float) -> (Float, Float) -> (Float, Float)
rotarcoordenadasos x y = case (x, y) of
                    ((x1, y1), (x2, y2)) -> trasladar (coordenadasomaximo ((x1 - x2), (y1 - y2))) (x2, y2)

triangulose listascoordenadasos = case listascoordenadasos of 
                     x1::x2::xs -> x1::(coordenadas1 x1 x2)::(rotarcoordenadasos (coordenadas1 x1 x2) (coordenadas2 x1 x2))::(coordenadas2 x1 x2)::(triangulose (x2::xs))
                     _ -> []

snowflake2 n listas = if n == 0 then listas else snowflake2 (n - 1) (triangulose listas)

snowflake n = snowflake2 n puntos3   --Funcion requerida

--Programacion triangulos funcion: Sierpinsky 
puntos : List (Float , Float)    --Declaracion de los primeros puntos 
puntos = [
            (300, 200),
            (400, 400),
            (200, 400)]

puntoss : List (List (Float , Float))
puntoss = [[
            (300, 200),
            (400, 400),
            (200, 400)]]

coordenadasoMedio : (Float , Float) -> (Float, Float) -> (Float , Float)
coordenadasoMedio x y = case (x,y) of 
            ((x1 , y1),(x2 , y2)) -> ((x2+x1)/2,(y2+y1)/2)

trianguloinv : List (Float, Float) -> List (Float, Float)
trianguloinv inv = case inv of 
            x1::x2::x3::xs -> (coordenadasoMedio x1 x2)::(coordenadasoMedio x1 x3)::(coordenadasoMedio x3 x2)::[]
            _ -> []

standar1 : List (Float , Float) -> List (Float , Float)
standar1 lm = case lm of 
            x1::x2::x3::xs -> x1::(coordenadasoMedio x1 x2)::(coordenadasoMedio x3 x1)::[]
            _ -> []

standar2 : List (Float , Float) -> List (Float , Float)
standar2 lm = case lm of 
            x1::x2::x3::xs -> x2::(coordenadasoMedio x1 x2)::(coordenadasoMedio x2 x3)::[]
            _ -> []

standar3 : List (Float , Float) -> List (Float , Float)
standar3 lm = case lm of 
            x1::x2::x3::xs -> x3::(coordenadasoMedio x3 x1)::(coordenadasoMedio x2 x3)::[]
            _ -> []

pasos : Int -> List (Float, Float) -> List(List (Float , Float))
pasos h n = case (h , n) of 
            (0 , []) -> []
            (0 , x::xs) -> []
            (hs , x::xs) -> (x::xs)::(trianguloinv (x::xs))::(pasos (hs - 1)(standar1(x::xs)))++(pasos (hs - 1)(standar2(x::xs)))++(pasos (hs - 1)(standar3(x::xs)))
            (_ , []) -> []

sierpinsky x = if x == 0 then puntoss else pasos x puntos


--esta funcion dibuja las lineas que conectaran las coordendas del poligono, dada una lista de coordendas

dibujar triangulo context =
    let
        acc (x,y) s = s |> Canvas.lineTo x y  --Conecta las lineas del punto x al punto y
    in
        case triangulo of
                (x0,y0)::xs ->
                    List.foldl acc (context |> Canvas.moveTo x0 y0) ((x0,y0)::xs) 
                    |> Canvas.lineTo x0 y0
                _ -> context

trazartriangulo : List (List (Float, Float)) -> Canvas.Commands -> Canvas.Commands
trazartriangulo triangulo context = case triangulo of 
    [] -> context
    x::xs -> dibujar x (trazartriangulo xs context)
-- Esta funcion genera el Html

ver : Modelo -> Html Mensaje
ver modelo = div[style "background" "HSL(150, 20%, 8%)"] --Colores del fondo
    [div[
        style "display" "flex", style "justify-content" "left", style "align-items" "left"
        ]
    [button [onClick (Fract Snowflake), style "height" "80px" , style"width" "110px",style "background" "HSL(219, 41%, 56%)",style "color" "HSL(177, 0%, 0%)"] [text "Dibujar SNOWFLAKE"], --Se declaran los botones y las funciones que estos tedran, ademas se declaran sus propiedades
     button [onClick (Fract Sierpinsky), style "height" "80px" , style"width" "110px",style "background" "HSL(69, 70%, 56%)",style "color" "HSL(177, 0%, 0%)"] [text "Dibujar SIERPINSKI"],
     button [onClick (Fract Puntos), style "height" "80px" , style"width" "110px",style "background" "HSL(1, 82%, 55%)",style "color" "HSL(177, 0%, 0%)"] [text "Desde 0"]],
    
    div[style "display" "flex", style "justify-content" "center", style "align-items" "center"]
    [button [onClick (Repte 0), style "height" "70px" , style"width" "100px",style "background" "HSL(0, 70%, 56%)",style "color" "HSL(177, 0%, 0%)"] [Html.text "-"],
     button [onClick (Repte (1)), style "height" "70px" , style"width" "100px",style "background" "HSL(107, 70%, 56%)",style "color" "HSL(177, 0%, 0%)"] [Html.text "+"]],
   
   div[style "display" "flex", 
         style "justify-content" "center", 
         style "align-items" "center"]
    [
    let     --Tamano del cuadrado
        width = 600
        height = 550
        poligono = case (modelo.fract) of 
            Snowflake -> dibujar (snowflake (modelo.repte))
            Sierpinsky -> trazartriangulo (sierpinsky (modelo.repte))
            Puntos -> dibujar puntos
    in
        Canvas.element
            width    --Elementos dentro del cuadrado
            height
            [ style "border" "9px solid white",
              style "background""HSL(107, 70%, 0%)"
            ]
            (
                Canvas.empty
                |> Canvas.beginPath
                |> Canvas.clearRect 0 0 width height
                |> Canvas.lineWidth 3
                |> Canvas.fillStyle (Color.hsla 0 0 0 0.7)
                |> Canvas.fillRect 0 0 width height
                |> Canvas.strokeStyle (Color.hsl 15 0 100)
                |> poligono
                |> Canvas.stroke 
            )
    ] 
    
    ]

main =
    Browser.sandbox
        { init = modeloprincipal
        , view = ver
        , update = refrescar
        }


