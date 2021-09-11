module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Random
import List.Extra
import Process
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

type GameState = Ready | Busy | GameOver

type alias Model = {
    width: Int,
    height: Int,
    colors: List (List CellColor),
    score: Int,
    state: GameState
    }

type Msg =
    RandomizeColors |
    SetColors (List (List CellColor)) |
    ClickColor Int Int |
    RemoveCells (List (Int, Int)) |
    ApplyVerticalGravity |
    ApplyHorizontalGravity |
    CheckGameState

createEmptyModel : Int -> Int -> Model
createEmptyModel width height = {
    width = width,
    height = height,
    colors = (List.repeat height (List.repeat width Empty)),
    score = 0,
    state = Busy
    }

init : () -> (Model, Cmd Msg)
init _ = update RandomizeColors (createEmptyModel tableWidth tableHeight)

-- Update

delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
    |> Task.andThen (always <| Task.succeed msg)
    |> Task.perform identity

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RandomizeColors -> (
        model,
        Random.generate SetColors (randomColors model.height model.width)
        )
    SetColors colors -> (
        { model | colors = colors, state = Ready },
        Cmd.none
        )
    ClickColor x y -> (
        { model | state = Busy },
        clickColor x y model
        )
    RemoveCells cells -> (
        updateScore (removeCells model cells) cells,
        delay 50 ApplyVerticalGravity
        )
    ApplyVerticalGravity -> (
        applyVerticalGravity model,
        delay 100 ApplyHorizontalGravity
        )
    ApplyHorizontalGravity -> (
        applyHorizontalGravity model,
        Task.perform (\x -> CheckGameState) Time.here
        )
    CheckGameState -> (
        checkGameState model,
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

hasEqualNeighbors : Int -> Int -> Model -> Bool
hasEqualNeighbors x y model =
    let color = getColor x y model
        neighbors = getCellNeighbors x y model
        neighborColors = List.map (\(otherX, otherY) -> getColor otherX otherY model) neighbors in
    color /= Empty && List.any (\otherColor -> color == otherColor) neighborColors

clickColorGraph : List (Int, Int) -> Cmd Msg 
clickColorGraph cells =
    if List.length cells > 1
    then delay 100 (RemoveCells cells)
    else Cmd.none

clickColor : Int -> Int -> Model -> Cmd Msg
clickColor x y model = case model.state of
    Busy -> Task.perform (\_ -> CheckGameState) Time.now
    GameOver -> Cmd.none
    Ready -> let color = getColor x y model in
             if color == Empty
             then Cmd.none
             else clickColorGraph (findEqualNeighbors color model [(x, y)] (Set.singleton (x, y)))

updateCell : Int -> Int -> CellColor -> Model -> Model
updateCell x y color model = case at y model.colors of
    Nothing -> model
    Just originalRow -> let updatedColors = List.Extra.setAt y (List.Extra.setAt x color originalRow) model.colors in
        { model | colors = updatedColors }

removeCell : Int -> Int -> Model -> Model
removeCell x y model = updateCell x y Empty model

removeCells : Model -> List (Int, Int) -> Model
removeCells model cells = case cells of
   [] -> model
   ((x, y) :: xs) -> removeCells (removeCell x y model) xs

getColumn : Int -> Model -> List CellColor
getColumn x model =
    let indices = List.range 0 (model.width - 1) in
    List.map (\y -> getColor x y model) indices

updateColumn : Int -> List CellColor -> Model -> Model
updateColumn x newColumn model =
    let indexedColors = (List.indexedMap (\y color -> (y, color)) newColumn) in
    List.foldl (\(y, color) updatedModel -> updateCell x y color updatedModel) model indexedColors

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

columnIsEmpty : Int -> Model -> Bool
columnIsEmpty x model = List.all (\color -> color == Empty) (getColumn x model)

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

applyHorizontalGravity : Model -> Model
applyHorizontalGravity model = removeFirstEmptyColumn 0 0 model

getContractedColumn : Int -> Model -> Maybe (List CellColor)
getContractedColumn x model =
    let column = getColumn x model
        nonEmpty = List.filter (\color -> color /= Empty) column
        emptyCount = model.height - List.length nonEmpty in
    if emptyCount == 0
    then Nothing
    else Just (List.concat [List.repeat emptyCount Empty, nonEmpty])

contractColumn : Int -> Model -> Model
contractColumn x model =
    case getContractedColumn x model of
       Nothing -> model
       Just contractedColumn -> updateColumn x contractedColumn model

applyVerticalGravity : Model -> Model
applyVerticalGravity model = List.foldl contractColumn model (List.range 0 (model.width - 1))

getScore : List (Int, Int) -> Int
getScore list = (List.length list - 1) ^ 2

updateScore : Model -> List (Int, Int) -> Model
updateScore model cells = { model | score = model.score + getScore cells }

isGameOver : Model -> Bool
isGameOver model =
    let xs = List.range 0 (model.width - 1)
        ys = List.range 0 (model.height - 1)
        indices = List.concatMap (\y -> List.concatMap (\x -> [(x, y)]) xs) ys in
    not (List.any (\(x, y) -> hasEqualNeighbors x y model) indices)

checkGameState : Model -> Model
checkGameState model =
    if isGameOver model
    then { model | state = GameOver }
    else { model | state = Ready }

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- View

view : Model -> Html Msg
view model = pageContainer model

combineStyles : List (String, String) -> List (Attribute Msg)
combineStyles styles = List.map (\(key, value) -> style key value) styles

colorName : CellColor -> String
colorName color = case color of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
    Yellow -> "yellow"
    Empty -> "white"

borderColorName : CellColor -> String
borderColorName color = case color of
    Red -> "darkred"
    Green -> "darkgreen"
    Blue -> "darkblue"
    Yellow -> "olive"
    Empty -> "white"

cellStyles : CellColor -> List (String, String)
cellStyles color = [
    ("width", "40px"),
    ("height", "40px"),
    ("background-color", colorName color),
    ("border", "2px solid " ++ borderColorName color),
    ("cursor", "pointer")
    ]

clickHandler : Int -> Int -> Attribute Msg
clickHandler x y = onClick (ClickColor x y)

tableCell : Int -> Int -> CellColor -> Html Msg
tableCell x y color = div ((clickHandler x y) :: combineStyles (cellStyles color)) [ text "" ]

getRows : Int -> List CellColor -> List (Html Msg)
getRows y colors = List.indexedMap (\x color -> tableCell x y color) colors

tableRow : Int -> List CellColor -> Html Msg
tableRow y colors = div (combineStyles [("display", "flex")]) (getRows y colors)

tableStyles : List (String, String)
tableStyles = [
    ("border", "1px solid black"),
    ("padding", "5px"),
    ("background-color", "white")
    ]

fullTable : List (List CellColor) -> Html Msg
fullTable allColors = div (combineStyles tableStyles) (List.indexedMap (\y color -> (tableRow y color)) allColors)

gameTitle : Html Msg
gameTitle = div [style "font-size" "40px"] [text "SameGame"]

scoreContainerStyles : List (String, String)
scoreContainerStyles = [
    ("margin-left", "auto"),
    ("margin-top", "auto"),
    ("padding", "2px")
    ]

scoreContainer : Model -> Html Msg
scoreContainer model =
    let scoreText = "Score: " ++ String.fromInt model.score
        fullText = if model.state == GameOver then scoreText ++ " (game over)" else scoreText in
    div (combineStyles scoreContainerStyles) [text fullText]

headerStyles : List (String, String)
headerStyles = [
    ("display", "flex"),
    ("flex-direction", "row"),
    ("color", "white")
    ]

header : Model -> Html Msg
header model = div (combineStyles headerStyles) [gameTitle, scoreContainer model]

gameContainerStyles : List (String, String)
gameContainerStyles = [
    ("display", "flex"),
    ("flex-direction", "column")
    ]

gameContainer : Model -> Html Msg
gameContainer model = div (combineStyles gameContainerStyles) [
    header model,
    fullTable model.colors
    ]

pageContainerStyles : List (String, String)
pageContainerStyles = [
    ("width", "100vw"),
    ("height", "100vh"),
    ("display", "flex"),
    ("justify-content", "center"),
    ("align-items", "center"),
    ("background-color", "#202020"),
    ("font-family", "helvetica")
    ]

pageContainer : Model -> Html Msg
pageContainer model = div (combineStyles pageContainerStyles) [gameContainer model]