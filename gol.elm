type Board = [[Status]]
type Coordinate = (Int, Int)
data Status = Alive | Dead

-- cartProd : [a] -> [b] -> [(a, b)]
-- f : [a] -> [(b -> (a, b)]
-- f [1, 2] = [(b -> (1, b)), (b -> (2, b)]
-- g : [f'] b -> [(a, b)]
-- g (f [1, 2]) 3 = [(1,3), (2,3)]
-- h : g -> lb -> [[(a, b)]]
-- h g [3, 4] = [[(1,3), (2,3)], [(1,4), (2,4)]]
-- foldr (++) [] : [[(a,b)]] -> [(a,b)]

pCartProd : [a] -> [b] -> [[(a, b)]]
pCartProd la lb = let f la' = map (\x -> (\y -> (x, y))) la'
                      g lf b = map (\f' -> f' b) lf
                      h g lb' = map (\b -> g b) lb'
                in
                     h (g (f la)) lb

cartProd : [a] -> [b] -> [(a, b)]
cartProd la lb = foldr (++) [] <| pCartProd la lb


isAlive : Status -> Bool
isAlive = (==) Alive

boardColumnCount : Board -> Int
boardColumnCount board = maximum <| map length board

getNodeAt : Board -> Coordinate -> Status
getNodeAt board coordinate =
    let maxRow = length board
        maxColumn = boardColumnCount board
        row = (fst coordinate)
        column = (snd coordinate)
    in
        if | (row < 0) || (row >= maxRow) -> Dead
           | (column < 0) || (column >= maxColumn) -> Dead
           | otherwise -> head <| drop column <| head <| drop row board

getNeighborsOf : Board -> Coordinate -> [Status]
getNeighborsOf board coordinate =
    [0..3] ++ [5..8] |> map (\offset ->
        let row = (fst coordinate) + (offset // 3 - 1)
            column = (snd coordinate) + (offset % 3 - 1)
        in
            getNodeAt board (row, column))

countAliveNeighbors : Board -> Coordinate -> Int
countAliveNeighbors board coordinate = length
    <| filter isAlive
    <| getNeighborsOf board coordinate

conwayRules : Board -> Coordinate -> Status
conwayRules board coordinate =
    let livingNeighbors = countAliveNeighbors board coordinate
    in
        if | livingNeighbors == 2 -> getNodeAt board coordinate
           | livingNeighbors == 3 -> Alive
           | otherwise -> Dead


iterateBoard : Board -> Board
iterateBoard board =
    let rowCount = length board
        columnCount = boardColumnCount board
    in
        pCartProd [0..rowCount-1] [0..columnCount-1] |> map (\row ->
            row |> map (\coordinate -> conwayRules board coordinate))

-- RENDERING

g = [[Dead, Alive, Dead, Dead, Dead],
     [Dead, Alive, Dead, Dead, Dead],
     [Dead, Alive, Dead, Dead, Dead],
     [Dead, Dead, Dead, Dead, Dead],
     [Dead, Dead, Dead, Dead, Dead]]

height = 400
width = 400

drawBoard : Board -> Element
drawBoard board =
    let rowCount = length board
        columnCount = boardColumnCount board
        cellSize = width / (toFloat columnCount)
        xOffset = -(width - cellSize) / 2
        yOffset = (height - cellSize) / 2
    in
        collage height width
            (cartProd [0..rowCount-1] [0..columnCount-1] |> map (\coordinate ->
                let status = getNodeAt board coordinate
                    cellColor = (if status == Alive then lightGrey else charcoal)
                    x = xOffset + (toFloat (snd coordinate)) * cellSize
                    y = yOffset - (toFloat (fst coordinate)) * cellSize
                in
                    square cellSize
                        |> filled cellColor
                        |> move (x, y)))


main : Signal Element
main = drawBoard <~ (foldp (\time -> iterateBoard) g (every second))
