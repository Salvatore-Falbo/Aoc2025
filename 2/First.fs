module Day2_1

let first =
    let mutable sum: uint64 = 0UL
    let input = Reader.ReadLines "2\\input.txt" |> Seq.head

    for line in input.Split(",") do
        let [| startString; stopString |] = line.Split("-")
        let start = startString |> uint64
        let stop = stopString |> uint64

        for value in [ start..stop ] do
            let digits = (value |> float |> log10 |> uint64) + 1UL

            if digits % 2UL = 0UL then
                let valuef = value |> double
                let expf = digits / 2UL |> double

                if valuef / 10.0 ** expf |> floor = valuef % 10.0 ** expf then
                    sum <- sum + value

    printfn $"2-1 solution: {sum}"
