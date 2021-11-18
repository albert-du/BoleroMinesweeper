module Minesweeper.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Minesweeper

type Model =
    { Width: int
      Height: int
      Mines: int
      GameStatus: GameResult voption
      Time: int
      Grid: MinesweeperGrid voption
      Error: string option
      Random: Random }

type Message =
    | Tick
    | Reset of width: int * height: int * mines: int 
    | SetWidth of int
    | SetHeight of int
    | SetMineCount of int
    | Uncover of xy: struct(int * int)
    | Flag of xy: struct(int * int)

let init random =
    { Width = 9
      Height = 9
      Mines = 10
      GameStatus = ValueNone
      Time = 0
      Grid = ValueNone 
      Error = None
      Random = random }, Cmd.none

let update message model =
    match message with
    | Tick ->
        if model.Grid.IsSome && model.GameStatus.IsNone then
            { model with Time = model.Time + 1 }, Cmd.none
        else model, Cmd.none
    | Reset(w, h, m) ->
        match MinesweeperGrid.verify w h m with
        | Ok () -> 
            { model with Error = None; Width = w; Height = h; Mines = m; Grid = ValueNone; GameStatus = ValueNone; Time = 0 }, Cmd.none
        | Error message -> 
            { model with Grid = ValueNone; Error = Some message; GameStatus = ValueNone}, Cmd.none
    
    | SetWidth w -> model, Reset(w, model.Height, model.Mines) |> Cmd.ofMsg
    
    | SetHeight h -> model, Reset(model.Width, h, model.Mines) |> Cmd.ofMsg

    | SetMineCount m -> model, Reset(model.Width, model.Height, m) |> Cmd.ofMsg

    | Uncover struct (x,y) ->
        match model.Grid with
        | ValueSome grid ->
            // Uncover a location
            let result = MinesweeperGrid.uncover x y grid
            { model with Grid = ValueSome result.Grid; GameStatus = result.Result }, Cmd.none

        | ValueNone -> 
            // Generate the grid
            match MinesweeperGrid.generate (model.Random.Next 10000) model.Width model.Height model.Mines x y with
            | Ok grid -> { model with Grid = ValueSome grid }, Uncover(x,y) |> Cmd.ofMsg // Call Uncover again
            | Error message -> { model with Error = Some message }, Cmd.none

    | Flag struct(x,y) ->
        match model.Grid with 
        | ValueSome grid -> { model with Grid = MinesweeperGrid.flag x y grid |> ValueSome}, Cmd.none 
        | ValueNone -> model, Cmd.none


let controlBar model reset =
    let winFaces = [|"😀"; "😅"; "😂"; "🥳"; "😎"|]
    let lostFaces = [|"😱"; "🤕"; "🥵"; "🥶"; "😨"; "💀"|]
    div [ attr.id "control-bar" ] [
        div [ attr.id "flag-counter" ] [ 
            textf "%03i" (model.Mines - ValueOption.mapDefault (fun x -> x.Flagged) 0 model.Grid )
        ]
        div [ 
            attr.id "reset-button" 
            on.click (ignore >> reset)
        ] [
            match model.GameStatus with
            | ValueNone -> "🙂"
            | ValueSome Win -> winFaces.[model.Time % winFaces.Length]
            | ValueSome Loss -> lostFaces.[model.Time % lostFaces.Length]
            |> text
        ]
        div [ attr.id "timer-count" ] [ 
            textf "%03i" model.Time
        ]
    ]

let sizeBar model reset =
    div [ attr.id "size-bar" ] [
        select [ 
            on.change <| fun x ->
                match string x.Value with 
                | "Easy" -> reset (9,9,10)
                | "Medium" -> reset (16,16,40) 
                | _ -> reset (30,16,99) 
        ] [
            option [] [ text "Easy" ]
            option [] [ text "Medium" ]
            option [] [ text "Hard" ]
        ]
    ]

let nearColor = function
    | 1 -> "#0000ff"
    | 2 -> "#008000"
    | 3 -> "#ff0000"
    | 4 -> "#000080"
    | 5 -> "#800000"
    | 6 -> "#008080"
    | 7 -> "#000000"
    | _ -> "#808080"

let loadedCell (status: GameResult voption) (size: int) cell xy dispatch =
    th [
        attr.width size; attr.height size;
        attr.classes [
            match cell.Status with 
            | Hidden false when status.IsNone -> "cell-hidden"; "clickable"
            | Hidden _ -> "cell-hidden"; 
            | Uncovered -> "cell"
            #if DEBUG
            $"DEBUG-{cell.Type}".Replace(' ','-')
            #endif
        ]
        if status.IsNone then
            on.click (fun _ -> Uncover xy |> dispatch )
            on.contextmenu (fun _ -> Flag xy |> dispatch )
    ] [
        match status with 
        | ValueSome Loss ->
            match cell.Type with 
            | Mine ->
                p [ attr.style $"font-size:{float size * 0.6}px" ] [ text "💣" ]
            | Near x when cell.Status = Uncovered ->
                p [ attr.style $"color:{nearColor x}; font-size:{float size * 0.8}px" ] [ textf "%i" x ]
            | _ when cell.Status = Hidden true ->
                p [ attr.style $"font-size:{float size * 0.55}px" ] [ text "🚩" ]
            | _ -> ()
        | _ ->
            // The game has not ended
            match cell.Status with 
            | Uncovered ->
                match cell.Type with
                | Near x ->  p [ attr.style $"color:{nearColor x}; font-size:{float size * 0.8}px" ] [ textf "%i" x ]
                | _ -> ()
            | Hidden true -> 
                p [ attr.style $"font-size:{float size * 0.55}px" ] [ text "🚩" ]
            | Hidden _ -> ()

    ]

let unloadedCell size xy dispatch = 
    th [
        attr.width size; attr.height size;
        attr.classes ["cell-hidden"; "clickable"]
        on.click (fun _ -> Uncover xy |> dispatch)
    ] []

let grid (model: Model) dispatch =
    let size = 600 / model.Width

    table [
        attr.classes ["minesweeper-grid"]; 
        attr.width 600 
    ] [
        let cell = 
            match model.Grid with
            | ValueSome g ->
                fun x y -> loadedCell model.GameStatus size g.Cells.[x,y] struct(x, y) dispatch
            | ValueNone -> 
                fun x y -> unloadedCell size struct (x, y) dispatch
        forEach [0 .. model.Height - 1] <| fun row ->
            tr [attr.``class`` "row" ] [
                forEach [0 .. model.Width - 1] <| fun col ->
                    cell col row
            ]
    ]

let view model dispatch =
    div [attr.id "game"] [
        controlBar model (fun () -> Reset(model.Width,model.Height,model.Mines) |> dispatch)
        grid model dispatch
        sizeBar model  (Reset >> dispatch)
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()
    
    let t = new System.Timers.Timer(Interval = 1000.)
    do t.Start()

    let timer _ =
        let sub dispatch = 
            t.Elapsed.Add(fun _ -> dispatch Tick)
        Cmd.ofSub sub

    override _.Program =
        Program.mkProgram (fun _ -> Random() |> init) update view
        |> Program.withSubscription timer
