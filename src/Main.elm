module Main exposing (..)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import Set exposing (Set)
import Random
import Process
import Task
import Time

import Logic exposing (..)
import Model exposing (..)
import Styles exposing (..)

main : Program () Model Msg
main = Browser.element {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view >> toUnstyled
    }

-- Model

tableHeight : Int
tableHeight = 10

tableWidth : Int
tableWidth = 20

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
        createEmptyModel tableWidth tableHeight,
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

randomColors : Int -> Int -> Random.Generator (List (List CellColor))
randomColors height width = Random.map (makeTable width) (Random.list (height * width) randomColor)

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

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- View

view : Model -> Html Msg
view model = pageContainer model

cellClickHandler : Int -> Int -> Attribute Msg
cellClickHandler x y = onClick (ClickColor x y)

tableCell : Int -> Int -> CellColor -> Html Msg
tableCell x y color = styled div (cellStyles color) [cellClickHandler x y] [ text "" ]

getRows : Int -> List CellColor -> List (Html Msg)
getRows y colors = List.indexedMap (\x color -> tableCell x y color) colors

tableRow : Int -> List CellColor -> Html Msg
tableRow y colors = styled div tableRowStyles [] (getRows y colors)

fullTable : List (List CellColor) -> Html Msg
fullTable allColors = styled div tableStyles [] (List.indexedMap (\y color -> (tableRow y color)) allColors)

gameTitle : Html Msg
gameTitle = styled div gameTitleStyles [] [text "SameGame"]

restartClickHandler : Attribute Msg
restartClickHandler = onClick RandomizeColors

restartButton : Html Msg
restartButton = styled button (restartButtonHoverStyle :: restartButtonStyles) [restartClickHandler] [text "Restart"]

scoreContainer : Model -> Html Msg
scoreContainer model =
    let scoreText = "Score: " ++ String.fromInt model.score
        fullText = if model.state == GameOver then scoreText ++ " (game over)" else scoreText in
    styled div scoreContainerStyles [] [text fullText, restartButton]

header : Model -> Html Msg
header model = styled div headerStyles [] [gameTitle, scoreContainer model]

gameContainer : Model -> Html Msg
gameContainer model = styled div gameContainerStyles [] [
    header model,
    fullTable model.colors
    ]

pageContainer : Model -> Html Msg
pageContainer model = styled div pageContainerStyles [] [gameContainer model]