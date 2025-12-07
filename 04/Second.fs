module Day4_2

let frame (input: string seq) =
    let sides =
        [| for i in input do
               yield "." + i + "." |]
        |> Array.map (fun x -> Seq.toArray x)

    let empty =
        [| for _ in 0 .. (sides[0].Length - 1) do
               yield '.' |]

    Array.concat [ [| empty |]; sides; [| empty |] ]


let rec propagate (frame: char array array byref) row column =
    let mutable sum = 0

    for i in [ -1 .. 1 ] do
        for j in [ -1 .. 1 ] do
            if frame[row + i][column + j] = '@' then
                sum <- sum + check &frame (row + i) (column + j)

    sum

and check (frame: char array array byref) (row: int) (column: int) =
    let mutable count = -1

    for i in [ -1 .. 1 ] do
        for j in [ -1 .. 1 ] do
            if frame[row + i][column + j] = '@' then
                count <- count + 1

    let result =
        if count < 4 then
            frame[row][column] <- '.'
            1 + propagate &frame row column
        else
            0

    result


let count (frame: char array array byref) =
    let mutable sum = 0

    for i in 1 .. (frame.Length - 2) do
        for j in 1 .. (frame[0].Length - 2) do
            if frame[i][j] = '@' then
                sum <- sum + check &frame i j

    sum


let second =
    let input = Reader.ReadLines "04\\input.txt"

    let mutable framed = input |> frame

    let result = count &framed

    printfn $"4-1 solution: {result}"
