module Square

type SquareState =
    | Player
    | Wall
    | Path
    | Floor

type Square () =
    let mutable isPath = false
    let mutable visited = false
    let mutable distance = 10000
    let mutable h = 0
    let mutable state = SquareState.Floor

    //+ is floor, O is wall, # is path, @ is player
    member this.State
        with get () = state
        and set (value) = state <- value

    member this.IsPath
        with get () = isPath
        and set (value) = isPath <- value

    member this.Cost () = h + distance

    member this.Distance
        with get () = distance
        and set (value) = distance <- value

    member this.H 
        with get () = h
        and set (value) = h <- value
    
    member this.Visited
        with get () = visited
        and set (value) = visited <- value