module Day3_2

let highest (bank: string) =
    let mutable high = 0

    for i in [ 1 .. (bank.Length - 1) ] do
        if bank[i] |> int > (bank[high] |> int) then
            high <- i

    high

let parser (line: string) (indexes: int array) =
    let mutable sum = 0.0

    for i in [ 0 .. (indexes.Length - 1) ] do
        sum <- sum + (line[indexes[i]] |> string |> double) * 10.0 ** (12 - 1 - i |> double)

    sum


let second =
    let mutable sum: uint64 = 0UL
    let input = Reader.ReadLines "03\\input.txt"

    for line in input do
        let indexes = Array.init 12 (fun n -> n)

        indexes[0] <- highest line[0 .. (line.Length - 12)]

        for i in [ 1 .. (indexes.Length - 1) ] do
            indexes[i] <-
                indexes[i - 1]
                + 1
                + highest line[(1 + indexes[i - 1]) .. (line.Length - 12 + i)]

        let value = parser line indexes

        sum <- sum + (value |> uint64)

    printfn $"3-2 solution: {sum}"
