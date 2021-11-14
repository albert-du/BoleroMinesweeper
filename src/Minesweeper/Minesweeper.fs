namespace Minesweeper

open System
open System.Collections.Generic

// Domain Model

type CellType =
    | Mine
    | Empty
    | Near of int

type CellStatus =
    | Hidden of flagged: bool
    | Uncovered 

type Cell = { Type: CellType; Status: CellStatus }

type MinesweeperGrid = 
    { Cells: Cell [,]
      Uncovered: int
      Flagged: int
      Mines: int }
    member this.Width = 1 + this.Cells.GetUpperBound 0
    member this.Height = 1 + this.Cells.GetUpperBound 1

type GameResult =
    | Win
    | Loss

module MinesweeperGrid =

    let countUncovered cells = 
        let mutable count = 0
        for i in cells do
            if i.Status = Uncovered && i.Type <> Mine then
                count <- count + 1
        count

    let mines (grid: MinesweeperGrid) = seq {
        for i = 0 to grid.Width - 1 do
            for j = 0 to grid.Height - 1 do
                if grid.Cells.[i,j].Type = Mine then
                    grid.Cells.[i,j]
    }

    let flagCount (grid: MinesweeperGrid) = grid.Flagged

    let flag x y (grid: MinesweeperGrid) =
        match grid.Cells.[x,y].Status with
        | Hidden flagged ->
            if flagged || grid.Flagged < grid.Mines then
                { grid with 
                    Cells = Array2D.updateAt x y {grid.Cells.[x,y] with Status = Hidden (not flagged)} grid.Cells
                    Flagged = grid.Flagged + if flagged then -1 else 1 }
            else grid
        | Uncovered -> grid

    let isLost (cells: #seq<struct (int * int)>) (grid: MinesweeperGrid) =
        cells
        |> Seq.map (fun struct (x,y) -> grid.Cells.[x,y] )
        |> Seq.exists (fun x -> x.Status = Uncovered && x.Type = Mine)

    let isWon (grid: MinesweeperGrid) =
        grid.Uncovered = grid.Width * grid.Height - grid.Mines

    let adjacents struct (x,y) = seq {
        for dx = -1 to 1 do
            for dy = -1 to 1 do
                struct (x + dx, y + dy)
    }

    let adjacent x1 y1 struct(x2, y2) = 
        abs(x1 - x2) <= 1 && abs(y1 - y2) <= 1

    let verify width height mines =
        if width < 5 then Error "Width cannot be less than 5"
        elif height < 5 then Error "Height cannot be less than 5"
        elif mines < 1 then Error $"Mines cannot be less than 1"
        elif mines > width * height - 9 then Error $"Mines cannot be greater than {width * height - 9} for this grid size"
        else Ok ()

    let uncover x y grid =
        let cells = grid.Cells
        let mutable toUncover = ResizeArray()
        let inspected = HashSet()
        let newGrid =
            let cells = 
                match cells.[x,y].Status with
                | Hidden flagged ->
                    if flagged then cells
                    else
                        // Recursively find the cells to uncover
                        let rec loop xy =
                            let struct(x,y) = xy
                            if inspected.Add xy && cells.[x, y].Type = Empty then
                                let a = 
                                    adjacents xy
                                    |> Seq.filter (fun struct(x,y) -> x >= 0 && y >= 0 && x < grid.Width && y < grid.Height && cells.[x,y].Type <> Mine && cells.[x,y].Status <> Hidden true)
                                toUncover.AddRange a
                                Seq.filter (fun struct(x,y) -> cells.[x,y].Type = Empty ) a
                                |> Seq.iter loop 
                            else ()
                        loop struct(x,y)
                        toUncover.Add struct(x,y)
                        toUncover <- ResizeArray(Seq.distinct toUncover)
                        // Build a new grid with cells uncovered
                        toUncover
                        |> Seq.fold (fun g struct (x,y) -> Array2D.updateAt x y { cells.[x,y] with Status = Uncovered } g) cells 
                | Uncovered -> cells 
            { grid with Cells = cells; Uncovered = cells |> Seq.cast<Cell> |> countUncovered }
        let result =
            if isLost toUncover newGrid then ValueSome Loss
            elif isWon newGrid then ValueSome Win
            else ValueNone
        {| Grid = newGrid; Result = result |}

    let generate seed width height mines x y =
        // Check arguments
        match verify width height mines with
        | Error x -> Error x
        | _ when x < 0 || x >= width -> Error "x out of bounds"
        | _ when y < 0 || y >= height -> Error "y out of bounds"
        | _ ->
            let random = let r = Random seed in r.Next

            // Generate a list of valid mine locations
            let mines' =
                let rec loop = function
                    | [] -> loop [ struct (random width, random height) ]
                    | acc when acc.Length = mines -> acc
                    | acc ->
                        let location = struct (random width, random height)
                        if adjacent x y location || List.contains location acc then 
                            loop acc
                        else 
                            loop (location :: acc)
                loop []
            // Find all the cells next to the mines
            let adjacentCells =  
                List.map adjacents mines'
                |> Seq.concat
                |> Seq.countBy id
                |> dict
            // Build a minesweeper grid
            { Cells =
                Array2D.init width height (fun x y -> 
                    { Type = 
                        let xy = struct(x, y)
                        if List.contains xy mines' then
                            Mine
                        else
                            match adjacentCells.TryGetValue xy with
                            | true, x -> Near x
                            | false, _ -> Empty
                      Status = Hidden false } )
              Flagged = 0;
              Uncovered = 0;
              Mines = mines }
            |> Ok