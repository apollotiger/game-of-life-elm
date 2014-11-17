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
        [0..rowCount-1] |> map (\row ->
            [0..columnCount-1] |> map (\column ->
                conwayRules board (row, column)))

-- RENDERING

{-g = [[Dead, Alive, Dead, Dead, Dead],
     [Dead, Alive, Dead, Dead, Dead],
     [Dead, Alive, Dead, Dead, Dead],
     [Dead, Dead, Dead, Dead, Dead],
     [Dead, Dead, Dead, Dead, Dead]]-}

g = [[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Alive,Dead,Alive,Dead,Dead],[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Alive,Dead,Dead,Alive,Dead,Dead,Dead],[Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead],[Alive,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Alive,Dead,Dead,Alive,Dead,Dead,Alive,Dead,Dead],[Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead],[Dead,Alive,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],[Dead,Alive,Dead,Alive,Dead,Alive,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead],[Dead,Alive,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Alive,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead],[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Alive],[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead],[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Alive,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],[Dead,Dead,Alive,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead],[Dead,Dead,Dead,Alive,Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Alive,Alive,Alive,Dead],[Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead],[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],[Dead,Dead,Dead,Alive,Dead,Alive,Alive,Alive,Dead,Dead,Dead,Alive,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead],[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Alive,Dead,Alive],[Alive,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Alive,Alive,Dead],[Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Alive,Dead,Alive,Dead,Dead,Dead,Dead],[Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Alive,Dead,Dead]]

height = 600
width = 600

drawBoard : Board -> Element
drawBoard board =
    let rowCount = length board
        columnCount = boardColumnCount board
        cellSize = width / (toFloat columnCount)
        xOffset = -(width - cellSize) / 2
        yOffset = (height - cellSize) / 2
    in
        collage height width
            ((cartProd [0..rowCount-1] [0..columnCount-1] |> map (\coordinate ->
                let status = getNodeAt board coordinate
                    cellColor = (if status == Alive then lightGrey else charcoal)
                    x = xOffset + (toFloat (snd coordinate)) * cellSize
                    y = yOffset - (toFloat (fst coordinate)) * cellSize
                in
                    [square cellSize
                        |> filled cellColor
                        |> move (x, y),
                     square cellSize
                        |> outlined (solid black)
                        |> move (x, y)]))
            |> foldr (++) [])


-- TESTS
{-
type Test = {description: String, initialBoard: Board, passes: (Board -> Bool)}
type TestResult = (Test, Bool)

runTest : Test -> TestResult
runTest test = (test, test.passes test.initialBoard)

expectBoard : Board -> (Board -> Bool)
expectBoard expectedBoard = (\board -> expectedBoard == iterateBoard board)

showTests : [Test] -> Element
showTests tests = flow down (map runTest tests |> map (\testResult ->
    let test = fst testResult
        result = if (snd testResult) then "Success" else "Failed"
    in
        plainText <| test.description ++ ": " ++ result))

tests = [
    {description="A cell with 0 living neighbors dies",
     initialBoard=[[Alive, Dead, Dead], [Dead, Dead, Dead], [Dead, Dead, Dead]],
     passes=(\board -> Dead == conwayRules board (0, 0))},
    {description="A cell with 1 living neighbor dies",
     initialBoard=[[Alive, Alive, Dead], [Dead, Dead, Dead], [Dead, Dead, Dead]],
     passes=(\board -> Dead == conwayRules board (0, 0))},
    {description="A live cell with 2 living neighbors lives",
     initialBoard=[[Dead, Alive, Dead], [Alive, Dead, Alive], [Dead, Dead, Dead]],
     passes=(\board -> Alive == conwayRules board (0, 1))},
    {description="A dead cell with 2 living neighbors is dead",
     initialBoard=[[Dead, Dead, Dead], [Alive, Dead, Alive], [Dead, Dead, Dead]],
     passes=(\board -> Dead == conwayRules board (0, 1))},
    {description="A live cell with 3 living neighbors lives",
     initialBoard=[[Dead, Alive, Dead], [Alive, Alive, Alive], [Dead, Dead, Dead]],
     passes=(\board -> Alive == conwayRules board (0, 1))},
    {description="A dead cell with 3 living neighbors becomes alive",
     initialBoard=[[Dead, Dead, Dead], [Alive, Alive, Alive], [Dead, Dead, Dead]],
     passes=(\board -> Alive == conwayRules board (0, 1))},
    {description="A live cell with 4 living neighbors dies",
     initialBoard=[[Alive, Alive, Dead], [Alive, Alive, Alive], [Dead, Dead, Dead]],
     passes=(\board -> Dead == conwayRules board (0, 1))},
    {description="A live cell with 5 living neighbors dies",
     initialBoard=[[Alive, Alive, Alive], [Alive, Alive, Alive], [Dead, Dead, Dead]],
     passes=(\board -> Dead == conwayRules board (0, 1))},
    {description="A live cell with 6 living neighbors dies",
     initialBoard=[[Alive, Alive, Alive], [Alive, Alive, Alive], [Alive, Dead, Dead]],
     passes=(\board -> Dead == conwayRules board (1, 1))},
    {description="A live cell with 7 living neighbors dies",
     initialBoard=[[Alive, Alive, Alive], [Alive, Alive, Alive], [Alive, Alive, Dead]],
     passes=(\board -> Dead == conwayRules board (1, 1))},
    {description="A live cell with 8 living neighbors dies",
     initialBoard=[[Alive, Alive, Alive], [Alive, Alive, Alive], [Alive, Alive, Alive]],
     passes=(\board -> Dead == conwayRules board (1, 1))},
    {description="A basic oscillator oscillates",
     initialBoard=[[Dead, Alive, Dead], [Dead, Alive, Dead], [Dead, Alive, Dead]],
     passes=(expectBoard [[Dead, Dead, Dead], [Alive, Alive, Alive], [Dead, Dead, Dead]])},
    {description="The (0, 1) cell in the oscillator dies",
     initialBoard=[[Dead, Alive, Dead], [Dead, Alive, Dead], [Dead, Alive, Dead]],
     passes=(\board -> Dead == conwayRules board (0, 1))},
    {description="The (1, 0) cell in the oscillator is born",
     initialBoard=[[Dead, Alive, Dead], [Dead, Alive, Dead], [Dead, Alive, Dead]],
     passes=(\board -> Alive == conwayRules board (1, 0))},
    {description="The (1, 1) cell in the oscillator lives",
     initialBoard=[[Dead, Alive, Dead], [Dead, Alive, Dead], [Dead, Alive, Dead]],
     passes=(\board -> Alive == conwayRules board (1, 1))},
    {description="The (1, 2) cell in the oscillator is born",
     initialBoard=[[Dead, Alive, Dead], [Dead, Alive, Dead], [Dead, Alive, Dead]],
     passes=(\board -> Alive == conwayRules board (1, 2))},
    {description="The (2, 1) cell in the oscillator dies",
     initialBoard=[[Dead, Alive, Dead], [Dead, Alive, Dead], [Dead, Alive, Dead]],
     passes=(\board -> Dead == conwayRules board (2, 1))}
     ]
-}

main : Signal Element
main = drawBoard <~ (foldp (\time -> iterateBoard) g (every <| 400 * millisecond))
