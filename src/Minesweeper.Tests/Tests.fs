module Tests

open System
open Expecto
open Minesweeper
open Minesweeper.MinesweeperGrid

let seed = 2021

[<Tests>]
let ``Minesweeper Grid tests`` =
  testList "all tests" [
    testList "argument exception tests" [
      test "providing a height less than 5 should fail" {
          Expect.isError (generate seed 5 4 2 2 2) "should return Error"
      }
      test "providing a width less than 5 should fail" {
          Expect.isError (generate seed 4 5 2 2 2) "should return Error"
      }
      test "providing a mine count greater than the width * height - 1 should fail" {
          Expect.isError (generate seed 5 5 25 2 2) "should return Error"
      }
      test "providing a mine count less than or equal to 0 should fail" {
          Expect.isError (generate seed 5 5 0 2 2) "should return Error"
      }
      test "providing a starting x less than to 0 should fail" {
          Expect.isError (generate seed 5 5 2 -1 2) "should return Error"
      }
      test "providing a starting y less than to 0 should fail" {
          Expect.isError (generate seed 5 5 2 2 -1) "should return Error"
      }
      test "providing a starting x greater than the width should fail" {
          Expect.isError (generate seed 5 5 2 6 2) "should return Error"
      }
      test "providing a starting y greater than the height should fail" {
          Expect.isError (generate seed 5 5 2 2 6) "should return Error"
      }
    ]
    testList "generation tests" [
      test "grid generation should be repeatable" {
        let expected = generate seed 5 5 9 2 2
        let actual = generate seed 5 5 9 2 2
        Expect.isOk actual "grid should generate"
        Expect.equal actual expected "generated grids should match"
      }
      test "grid generation should be repeatable 2" {
        let seed = let r = Random() in r.Next 100000
        let r1 () = let r = Random seed in r.Next 100000
        let r2 () = let r = Random seed in r.Next 100000
        
        for _ in 1 .. 10 do 
            let expected = generate (r1()) 10 10 10 3 2
            let actual = generate (r2()) 10 10 10 3 2
            Expect.isOk actual "grid should generate"
            Expect.equal actual expected "generated grids should match"
      }
      test "grid generation should be deterministic" {
        let expected = 
          array2D [
            [Near 1; Near 2; Mine; Mine; Near 3]
            [Near 3; Mine; Near 5; Mine; Mine]
            [Mine; Mine; Near 4; Near 3; Mine]
            [Mine; Mine; Near 2; Near 1; Near 1]
            [Near 2; Near 2; Near 1; Empty; Empty] ]
          |> Ok
        let actual = 
          generate seed 5 5 10 4 4
          |> Result.map (fun x -> x.Cells |> Array2D.map (fun x -> x.Type))
        Expect.equal actual expected "generated grid should match"
      }
      test "grid generation should have all hidden" {
        let actual = 
          match generate seed 5 5 10 4 4 with
          | Ok x ->  
             x.Cells
             |> Array2D.map (fun x -> x.Status)
          | Error _ -> array2D [||]

        Expect.all (Seq.cast<CellStatus> actual) (fun x -> x = Hidden false) "generated grid should only have hidden cells"
      }
      test "grid generation should avoid the first clicked cell" {
        let expected = 
          array2D [
            [Mine; Mine; Mine; Mine; Near 1]
            [Mine; Mine; Near 5; Near 3; Near 2]
            [Mine; Mine; Near 3; Near 1; Mine]
            [Mine; Mine; Near 5; Near 4; Near 3]
            [Mine; Mine; Mine; Mine; Mine] ]
          |> Ok
        let actual = 
            generate seed 5 5 16 2 3
            |> Result.map (fun x -> x.Cells |> Array2D.map (fun x -> x.Type))

        Expect.equal actual expected "generated grid should not have a mine at 2 3"
      }
      test "grid generation should avoid the first clicked cell 2" {
        let random = let r = Random() in Random(r.Next 100000)
        for _ = 1 to 20 do
          let x = random.Next 9
          let y = random.Next 9
          let seed = random.Next 10000000
          let actual =
            generate seed 10 10 50 x y 
            |> Result.map (fun x -> x.Cells |> Array2D.map (fun x -> x.Type))
            |> function
            | Ok x -> x
            | Error _ -> array2D [||]
          Expect.isTrue (actual.[x,y] <> Mine) "generated grid shouldn't place a mine on the first move"
      }
      test "non square grids should generate" {
          let expected =
            { Cells =
               array2D [
                [{ Type = Empty
                   Status = Hidden false }; { Type = Empty
                                              Status = Hidden false };
                 { Type = Near 1
                   Status = Hidden false }; { Type = Mine
                                              Status = Hidden false };
                 { Type = Near 3
                   Status = Hidden false }; { Type = Near 2
                                              Status = Hidden false }]
                [{ Type = Near 1
                   Status = Hidden false }; { Type = Near 1
                                              Status = Hidden false };
                 { Type = Near 1
                   Status = Hidden false }; { Type = Near 3
                                              Status = Hidden false };
                 { Type = Mine
                   Status = Hidden false }; { Type = Mine
                                              Status = Hidden false }]
                [{ Type = Mine
                   Status = Hidden false }; { Type = Near 1
                                              Status = Hidden false };
                 { Type = Empty
                   Status = Hidden false }; { Type = Near 3
                                              Status = Hidden false };
                 { Type = Mine
                   Status = Hidden false }; { Type = Mine
                                              Status = Hidden false }]
                [{ Type = Near 1
                   Status = Hidden false }; { Type = Near 1
                                              Status = Hidden false };
                 { Type = Near 1
                   Status = Hidden false }; { Type = Near 3
                                              Status = Hidden false };
                 { Type = Mine
                   Status = Hidden false }; { Type = Near 4
                                              Status = Hidden false }]
                [{ Type = Empty
                   Status = Hidden false }; { Type = Empty
                                              Status = Hidden false };
                 { Type = Near 1
                   Status = Hidden false }; { Type = Mine
                                              Status = Hidden false };
                 { Type = Near 3
                   Status = Hidden false }; { Type = Mine
                                              Status = Hidden false }]]
              Uncovered = 0
              Flagged = 0
              Mines = 9 }
            |> Ok
          let actual = generate seed 5 6 9 2 2
          Expect.isOk actual "grid should generate"
          Expect.equal actual expected "generated grids should match"
      }
      test "grid generation width should match" {
          let expected = 
            generate seed 5 7 9 2 2
            |> Result.map (fun x -> x.Width)
          let actual = Ok 5
          Expect.isOk actual "grid should generate"
          Expect.equal actual expected "width should match"
      }
      test "grid generation height should match" {
          let expected =
            generate seed 5 7 9 2 2
            |> Result.map (fun x -> x.Height)
          let actual = Ok 7
          Expect.isOk actual "grid should generate"
          Expect.equal actual expected "height should match"
      }   
    ]
    testList "grid module tests" [
      test "getting flag count of newly generated grid should return 0 " {
        let rand = Random()
        let actual =
          Seq.init 10 (fun _ -> 
            match generate (rand.Next 10000) 10 10 24 2 3 with
            | Ok x -> flagCount x
            | Error _ -> -1)
           
        Expect.allEqual actual 0 "newly generated grid shouldn't have any flags"
      }
      test "getting flag count of newly generated grid should return 0 2" {
        let actual = 
          match generate seed 5 5 16 2 3 with
          | Ok x -> flagCount x
          | Error _ -> -1
           
        Expect.equal actual 0 "newly generated grid shouldn't have any flags"
      }
      test "uncovering a hidden cell should uncover it and subsequent empty cells" {
        let expected =
            [ [ { Type = Mine; Status = Hidden false }
                { Type = Near 2; Status = Hidden false }
                { Type = Mine; Status = Hidden false }
                { Type = Mine; Status = Hidden false }
                { Type = Near 2; Status = Hidden false } ]
              [ { Type = Near 2; Status = Hidden false } 
                { Type = Near 3; Status = Uncovered }
                { Type = Near 2; Status = Uncovered }
                { Type = Near 4; Status = Uncovered }
                { Type = Mine; Status = Hidden false } ]
              [ { Type = Mine; Status = Hidden false }
                { Type = Near 2; Status = Uncovered }
                { Type = Empty; Status = Uncovered }
                { Type = Near 2; Status = Uncovered }
                { Type = Mine; Status = Hidden false } ]
              [ { Type = Mine; Status = Hidden false }
                { Type = Near 3; Status = Uncovered }
                { Type = Near 1; Status = Uncovered }
                { Type = Near 3; Status = Uncovered } 
                { Type = Near 3; Status = Hidden false } ]
              [ { Type = Mine; Status = Hidden false } 
                { Type = Near 2; Status = Hidden false }
                { Type = Near 1; Status = Hidden false } 
                { Type = Mine; Status = Hidden false }
                { Type = Mine; Status = Hidden false } ] ]
            |> array2D
        let actual = 
          generate seed 5 5 10 2 2
          |> Result.map (MinesweeperGrid.uncover 2 2)
          |> Result.map (fun x -> x.Grid.Cells)
          |> function
          | Ok x -> x
          | Error _ -> array2D []
        Expect.equal actual expected "newly generated grid shouldn't have any flags"
      }
    ]
  ]
  
