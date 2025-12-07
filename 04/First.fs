module Day4_1

let frame input =
    let sides =
        [ for i in input do
              yield "." + i + "." ]

    let empty =
        [ for _ in 0 .. (sides[0].Length - 1) do
              yield "." ]
        |> String.concat ""

    [ empty ] @ sides @ [ empty ]


let check (frame: string list) (row: int) (column: int) =
    let mutable count = -1

    for i in [ -1 .. 1 ] do
        for j in [ -1 .. 1 ] do
            if frame[row + i][column + j] = '@' then
                count <- count + 1

    let reachable = if count < 4 then 1 else 0

    reachable


let count (frame: string list) =
    let mutable sum = 0

    for i in 1 .. (frame.Length - 2) do
        for j in 1 .. (frame[0].Length - 2) do
            if frame[i][j] = '@' then
                sum <- sum + check frame i j

    sum


let first =
    let input = Reader.ReadLines "04\\input.txt"

    let result = input |> frame |> count

    printfn $"4-1 solution: {result}"
