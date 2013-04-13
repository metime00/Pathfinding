// http://www.policyalmanac.org/games/aStarTutorial.htm
open System
open System.Collections.Generic
open System.Threading
open Square

let preset = "+++++++++++++++
+++++++++OO++++
+++++++++O+++++
+++++OOOOO+++++
++OOO+++O++++++
OOO++O++O++++++
+++OOOOOO++++++
+++O+++O+++++++
OOO+OO+O+++++++
++OOOO+O+++++++
++++++OO+++++++
+OOOOOOO+OOOOOO
+++++++O+++++++
OOOOOO+OOOOOOO+
+++++++++++++++"

let board =
    let roo = new System.Random ()
    let tmp = Array2D.zeroCreate 15 15
    let input = preset.Split '\n'
    for i = 0 to 14 do
        for j = 0 to 14 do
            tmp.[i,j] <- new Square ()
            //if input.[i].[j] = 'O' then tmp.[i,j].State <- SquareState.Wall
            if roo.Next(100) > 85 then tmp.[i,j].State <- SquareState.Wall
    tmp

let boundsCheck x y =
    if x < 15 && x >= 0 && y < 15 && y >= 0 then true
    else false

///returns the heuristic value, with dX and dY being the destination coordinates
let heuristic x y dX dY =
    (abs (x - dX) + abs (y - dY)) * 10
    //int (sqrt (float ((dX - x) * (dX - x) + (dY - y) * (dY - y)))) * 10

let drawBoard () =
    Console.Clear ()
    for i = 0 to Array2D.length1 board - 1 do
        for j = 0 to Array2D.length2 board - 1 do
            Console.ResetColor ()
            match board.[i,j].State with
            | SquareState.Player -> printf "@"
            | SquareState.Wall -> printf "O"
            | SquareState.Path ->
                if board.[i,j].IsPath then
                    Console.ForegroundColor <- ConsoleColor.Blue
                    printf "#"
                else
                    Console.ForegroundColor <- ConsoleColor.Red
                    printf "#"
            | SquareState.Floor -> printf "+"
        printf "\n"

let pathFind x0 y0 x1 y1 =
    board.[x0,y0].Distance <- 0
    let mutable cur = (x0, y0)
    let dest = (x1, y1)
    let visited = new List<(int * int)>()

    //evaluates the adjacent squares to the current
    while (cur <> dest) do
        let x = fst cur
        let y = snd cur
        for i = -1 to 1 do
            for j = -1 to 1 do
                if boundsCheck (x + i) (y + j) && (board.[x + i, y + j].Distance = 10000 && not board.[x + i, y + j].Visited && board.[x + i, y + j].State <> SquareState.Wall) then
                    board.[x + i, y + j].Distance <- board.[x,y].Distance + 10
                    board.[x + i, y + j].H <- heuristic (x + i) (y + j) x1 y1
        board.[x,y].State <- SquareState.Path
        board.[x,y].Visited <- true
        visited.Add cur

        //checks for the lowest cost square next
        let mutable nX = 0
        let mutable nY = 0
        let mutable n = 10000
        for i = 0 to Array2D.length1 board - 1 do
            for j = 0 to Array2D.length2 board - 1 do
                if board.[i, j].Cost () < n && boundsCheck i j && not board.[i, j].Visited then
                    nX <- i
                    nY <- j
                    n <- board.[nX, nY].Cost ()
        cur <- (nX, nY)

    board.[x0,y0].State <- SquareState.Player
    board.[x0,y0].IsPath <- true
    board.[x1,y1].State <- SquareState.Player
    board.[x1,y1].IsPath <- true

    //marks the least cost path
    let mutable leasDis = board.[x1,y1].Distance
    let mutable leasX = x1
    let mutable leasY = y1
    while leasDis > 0 do
        let x = leasX
        let y = leasY
        for i = -1 to 1 do
            for j = -1 to 1 do
                if boundsCheck (x + i) (y + j) && board.[x + i, y + j].Visited && board.[x + i, y + j].Distance < leasDis then
                    leasDis <- board.[x + i, y + j].Distance
                    leasX <- x + i
                    leasY <- y + j
        board.[leasX, leasY].IsPath <- true
    ()

pathFind 0 0 10 8
drawBoard ()
Console.ReadKey true |> ignore