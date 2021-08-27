-- https://www.crazygames.com/game/same-game
module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Random

main : Program () Model Msg
main = Browser.element {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
    }

-- Model

tableHeight : Int
tableHeight = 10

tableWidth : Int
tableWidth = 20

type CellColor = Red | Green | Blue | Yellow | Empty

type alias Model = {
    colors : List (List CellColor)
    }

type Msg =
    RandomizeColors |
    SetColors (List (List CellColor)) |
    ClickColor Int Int

init : () -> (Model, Cmd Msg)
init _ = update RandomizeColors (Model (List.repeat tableHeight (List.repeat tableWidth Empty)))

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RandomizeColors -> (
        model,
        Random.generate SetColors (randomColors tableHeight tableWidth)
        )
    SetColors colors -> (
        Model colors,
        Cmd.none
        )
    ClickColor x y -> (
        model,
        Cmd.none
        )

randomColor : Random.Generator CellColor
randomColor = Random.uniform Red [Green, Blue, Yellow]

splitList : Int -> List a -> List (List a)
splitList len l = case l of
    [] -> []
    list -> List.take len list :: splitList len (List.drop len list)

makeTable : Int -> List CellColor -> List (List CellColor)
makeTable width list = splitList width list

randomColors : Int -> Int -> Random.Generator (List (List CellColor))
randomColors height width = Random.map (makeTable width) (Random.list (height * width) randomColor)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- View

view : Model -> Html Msg
view model = fullTable model.colors

colorName : CellColor -> String
colorName color = case color of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
    Yellow -> "yellow"
    Empty -> "white"

cellStyles : CellColor -> List (String, String)
cellStyles color = [
    ("width", "40px"),
    ("height", "40px"),
    ("background-color", colorName color),
    ("border-width", "2px"),
    ("border-color", colorName color),
    ("border-style", "groove")
    ]

combineStyles : List (String, String) -> List (Attribute Msg)
combineStyles styles = List.map (\(key, value) -> style key value) styles

clickHandler : Int -> Int -> Attribute Msg
clickHandler x y = onClick (ClickColor x y)

tableCell : Int -> Int -> CellColor -> Html Msg
tableCell x y color = div ((clickHandler x y) :: combineStyles (cellStyles color)) [ text "" ]

getRows : Int -> List CellColor -> List (Html Msg)
getRows y colors = List.indexedMap (\x color -> tableCell x y color) colors

tableRow : Int -> List CellColor -> Html Msg
tableRow y colors = div (combineStyles [("display", "flex")]) (getRows y colors)

fullTable : List (List CellColor) -> Html Msg
fullTable allColors = div [ class "game" ] (List.indexedMap (\y color -> (tableRow y color)) allColors)
