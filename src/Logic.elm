module Logic exposing (..)

import List.Extra
import Set exposing (Set)

import Model exposing (..)
import Utils exposing (..)

makeTable : Int -> List CellColor -> List (List CellColor)
makeTable width list = splitList width list

getColor : Int -> Int -> Model -> CellColor
getColor x y { colors } = Maybe.withDefault Empty (Maybe.andThen (at x) (at y colors))

type Direction = Up | Right | Down | Left

getDirectionCoords : Int -> Int -> Model -> Direction -> Maybe (Int, Int)
getDirectionCoords x y model direction = case direction of
    Up -> if y < 1 then Nothing else Just (x, y-1)
    Right -> if x > model.width - 1 then Nothing else Just (x+1, y)
    Down -> if y > model.height - 1 then Nothing else Just (x, y+1)
    Left -> if x < 1 then Nothing else Just (x-1, y)

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
