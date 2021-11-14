namespace Minesweeper

module Array2D =
    let updateAt (x: int) (y: int) (value: 'a) (arr: 'a [,]) =
        let arr = Array2D.copy arr
        arr.[x,y] <- value
        arr

module ValueOption =
    let mapDefault mapping value voption =
        ValueOption.map mapping voption
        |> ValueOption.defaultValue value