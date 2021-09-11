-- https://www.crazygames.com/game/same-game
module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Random
import Set exposing (Set)
import List.Extra
import Task
import Time

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
    ClickColor Int Int |
    RemoveCells (List (Int, Int)) |
    ApplyGravity

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
        model,
        recalculate x y model
        )
    RemoveCells cells -> (
        removeCells model cells,
        Task.perform (\_ -> ApplyGravity) Time.here
        )
    ApplyGravity -> (
        applyGravity model,
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
    Up -> if y < 1 then Nothing else Just (x, y-1)
    Right -> if x > model.width - 1 then Nothing else Just (x+1, y)
    Down -> if y > model.height - 1 then Nothing else Just (x, y+1)
    Left -> if x < 1 then Nothing else Just (x-1, y)

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

handleNeighbors : List (Int, Int) -> Cmd Msg 
handleNeighbors cells =
    if List.length cells > 1
    then Task.perform (\_ -> RemoveCells cells) Time.here
    else Cmd.none

recalculate : Int -> Int -> Model -> Cmd Msg
recalculate x y model =
    let color = getColor x y model
        neighbors = findEqualNeighbors color model [(x, y)] (Set.singleton (x, y)) in
    handleNeighbors neighbors

removeCell : Int -> Int -> Model -> Model
removeCell x y model = case at y model.colors of
    Nothing -> model
    Just originalRow -> let updatedColors = List.Extra.setAt y (List.Extra.setAt x Empty originalRow) model.colors in
        { model | colors = updatedColors }

removeCells : Model -> List (Int, Int) -> Model
removeCells model cells = case cells of
   [] -> model
   ((x, y) :: xs) -> removeCells (removeCell x y model) xs

columnIsEmpty : Int -> Model -> Bool
columnIsEmpty x model =
    let indices = List.range 0 (model.width - 1)
        colors = List.map (\y -> getColor x y model) indices in
    List.all (\color -> color == Empty) colors

removeCellAndSlideLeft : Int -> List CellColor -> List CellColor
removeCellAndSlideLeft x colors = List.concat [
    List.take x colors,
    List.drop (x+1) colors,
    [Empty]
    ]

removeColumn : Int -> Model -> Model
removeColumn x model = { model |
    colors = List.map (removeCellAndSlideLeft x) model.colors 
    }

hasLaterNonEmptyColumns : Int -> Model -> Bool
hasLaterNonEmptyColumns x model = List.any (\idx -> not (columnIsEmpty idx model)) (List.range (x+1) (model.width - 1))

removeColumnIfEmpty : Int -> Model -> Maybe Model
removeColumnIfEmpty x model =
    if (x < model.width) && (columnIsEmpty x model) && (hasLaterNonEmptyColumns x model)
    then Just (removeColumn x model)
    else Nothing

removeFirstEmptyColumn : Int -> Int -> Model -> Model
removeFirstEmptyColumn x numRemoved model =
    case removeColumnIfEmpty x model of
       Nothing -> if x == model.width - 1 - numRemoved then model else removeFirstEmptyColumn (x+1) numRemoved model
       Just updatedModel -> removeFirstEmptyColumn x (numRemoved+1) updatedModel

removeEmptyColumns : Model -> Model
removeEmptyColumns model = removeFirstEmptyColumn 0 0 model

fillEmptyCellsVertically : Model -> Model
fillEmptyCellsVertically model = model

applyGravity : Model -> Model
applyGravity model = removeEmptyColumns (fillEmptyCellsVertically model)

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
    ("border-style", "groove"),
    ("cursor", "pointer")
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
