namespace Minesweeper

/// The non-changing type of cell.
type CellType =
    | Mine
    | Empty
    | Near of int

/// The visibility of the cell, and whether its flagged.
type CellStatus =
    | Hidden of flagged: bool
    | Uncovered 

/// Represents one square on a minesweeper grid
type Cell = { Type: CellType; Status: CellStatus }

/// Represents a 2D array of cells
type MinesweeperGrid = 
    { Cells: Cell [,]
      Uncovered: int
      Flagged: int
      Mines: int }
    with member Width : int
         member Height : int

type GameResult =
    | Win
    | Loss

/// Functions to generate and non destructively modify the grid.
module MinesweeperGrid =

    /// Returns the number of flagged cells in a grid.
    val flagCount : MinesweeperGrid -> int
    
    /// Flags/unflags a coordinate.
    /// Returns the same grid if it can't be flagged/unflagged. 
    val flag : x: int -> y: int -> MinesweeperGrid -> MinesweeperGrid

    /// Uncovers a coordinate.
    /// Returns a new grid with cells uncovered and whether the game has been won or lost
    val uncover : x: int -> y: int -> MinesweeperGrid -> 
            {| Grid: MinesweeperGrid; Result: GameResult voption |}

    /// <summary>
    /// Verfies that a minesweeper grid with, width, height, mine count, is possible.
    /// </summary>
    /// <remarks>
    /// The width and the height cannot be less than 5 
    /// The mine count must be between 1 and the width * height - 1
    /// The x value must be between 0 and the width - 1
    /// The y value must be between 0 and the height - 1
    /// </remarks>
    val verify: width: int -> height: int -> mines: int -> Result<unit,string>

    /// <summary>
    /// Generates a minesweeper grid with a given RNG, width, height, mine count, and starting position.
    /// </summary>
    /// <remarks>
    /// The width and the height cannot be less than 5 
    /// The mine count must be between 1 and the width * height - 1
    /// The x value must be between 0 and the width - 1
    /// The y value must be between 0 and the height - 1
    /// Does not uncover the first square, must call `MinesweeperGrid.uncover` separately.
    /// </remarks>
    val generate: seed: int -> width: int -> height: int -> mines: int -> x: int -> y: int -> Result<MinesweeperGrid,string>