module Day3_1

let highest (bank: string) =
    let mutable high = 0

    for i in [ 1 .. (bank.Length - 1) ] do
        if bank[i] |> int > (bank[high] |> int) then
            high <- i

    high

let parser char1 char2 =
    (char1 |> string |> int) * 10 + (char2 |> string |> int) //me ne vergogno :D

let first =
    let mutable sum = 0
    let input = Reader.ReadLines "03\\input.txt"

    for line in input do
        let index1 = highest line[0 .. (line.Length - 2)]
        let index2 = index1 + 1 + highest line[(index1 + 1) .. (line.Length - 1)]
        sum <- sum + parser line[index1] line[index2]

    printfn $"3-1 solution: {sum}"
