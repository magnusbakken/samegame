module Model exposing (..)

type CellColor = Red | Green | Blue | Yellow | Empty

type GameState = Ready | Busy | GameOver

type alias Model = {
    width: Int,
    height: Int,
    colors: List (List CellColor),
    score: Int,
    state: GameState
    }