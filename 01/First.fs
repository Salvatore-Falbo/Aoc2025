module Day1_1

let first =
    let mutable sum = 50
    let mutable counter = 0

    for line in Reader.ReadLines "01\\input.txt" do
        let value = line[1..] |> int
        sum <- (sum + value * if line[0] = 'L' then -1 else 1) % 100

        if sum = 0 then
            counter <- counter + 1

    printfn $"1-1 solution: {counter}"
