-- https://www.crazygames.com/game/same-game
module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random

main = Browser.element {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
    }

-- Model

tableHeight = 10
tableWidth = 20

type Color = Red | Green | Blue | Yellow | Empty

type alias Model = {
    colors : List (List Color)
    }

type Msg =
    RandomizeColors |
    SetColors (List (List Color))

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

randomColor : Random.Generator Color
randomColor = Random.uniform Red [Green, Blue, Yellow]

splitList : Int -> List a -> List (List a)
splitList len l = case l of
    [] -> []
    list -> List.take len list :: splitList len (List.drop len list)

makeTable : Int -> Int -> List Color -> List (List Color)
makeTable height width list = splitList width list

randomColors : Int -> Int -> Random.Generator (List (List Color))
randomColors height width = Random.map (makeTable height width) (Random.list (height * width) randomColor)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- View

view : Model -> Html Msg
view model = fullTable model.colors

colorClass : Color -> String
colorClass color = case color of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
    Yellow -> "yellow"
    Empty -> "empty"

tableCell : Color -> Html Msg
tableCell color = div [ class "cell", class (colorClass color) ] [ text "" ]

tableRow : List Color -> Html Msg
tableRow colors = div [ class "row" ] (List.map tableCell colors)

fullTable : List (List Color) -> Html Msg
fullTable allColors = div [ class "game" ] (List.map tableRow allColors)
