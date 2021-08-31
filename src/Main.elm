-- https://www.crazygames.com/game/same-game
module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Random
import Set exposing (Set)
import List.Extra exposing (unique)
import Html exposing (a)

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
    width: Int,
    height: Int,
    colors : List (List CellColor)
    }

type Msg =
    RandomizeColors |
    SetColors (List (List CellColor)) |
    ClickColor Int Int

createEmptyModel : Int -> Int -> Model
createEmptyModel width height = {
    width = width,
    height = height,
    colors = (List.repeat height (List.repeat width Empty))
    }

init : () -> (Model, Cmd Msg)
init _ = update RandomizeColors (createEmptyModel tableWidth tableHeight)

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RandomizeColors -> (
        model,
        Random.generate SetColors (randomColors tableHeight tableWidth)
        )
    SetColors colors -> (
        { model | colors = colors },
        Cmd.none
        )
    ClickColor x y -> (
        recalculate x y model,
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

at :  Int -> List a -> Maybe a
at n xs = List.head (List.drop n xs)

getColor : Int -> Int -> Model -> CellColor
getColor x y { colors } = Maybe.withDefault Empty (Maybe.andThen (at x) (at y colors))

type Direction = Up | Right | Down | Left

getDirectionCoords : Int -> Int -> Model -> Direction -> Maybe (Int, Int)
getDirectionCoords x y model direction = case direction of
    Up -> if x == 0 then Nothing else Just (x, y-1)
    Right -> if x == model.width - 1 then Nothing else Just (x+1, y)
    Down -> if y == model.height - 1 then Nothing else Just (x, y+1)
    Left -> if x == 0 then Nothing else Just (x-1, y)

catMaybes : List (Maybe a) -> List a
catMaybes ms =
    case ms of
        [] -> []
        (m :: xs) -> case m of
           Nothing -> catMaybes xs
           Just x -> x :: catMaybes xs

getCellNeighbors : Int -> Int -> Model -> List (Int, Int)
getCellNeighbors x y model = catMaybes [
    getDirectionCoords x y model Up,
    getDirectionCoords x y model Right,
    getDirectionCoords x y model Down,
    getDirectionCoords x y model Left
    ]

getAllNeighbors : Model -> List (Int, Int) -> List (Int, Int)
getAllNeighbors model cells =
    let neighborLists = List.map (\(x, y) -> getCellNeighbors x y model) cells
        neighbors = List.concat neighborLists in
    List.Extra.unique neighbors

findEqualNeighbors : CellColor -> Model -> List (Int, Int) -> Set (Int, Int) -> List (Int, Int)
findEqualNeighbors color model found seen =
    let neighbors = getAllNeighbors model found
        allSeen = Set.union seen (Set.fromList neighbors)
        unseenNeighbors = List.filter (\cell -> not (Set.member cell seen)) neighbors
        sameColor = List.filter (\(x, y) -> getColor x y model == color) unseenNeighbors in
        if List.length sameColor > 0
        then List.concat [found, (findEqualNeighbors color model sameColor allSeen)]
        else found

recalculate : Int -> Int -> Model -> Model
recalculate x y model =
    let color = getColor x y model
        neighbors = findEqualNeighbors color model [(x, y)] (Set.singleton (x, y))
        output = Debug.log "neighbors" neighbors in
    model

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
