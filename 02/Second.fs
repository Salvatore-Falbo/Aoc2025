module Day2_2

let digits (number: float) = 1.0 + (number |> log10 |> floor)

let dividers (number: float) =
    let digits = number |> digits

    [| for divider in [ 1 .. (digits |> int) / 2 ] do
           if digits % (divider |> float) = 0 then
               yield divider |> float |]

let second =
    let mutable sum: uint64 = 0UL
    let input = Reader.ReadLines "02\\input.txt" |> Seq.head

    for line in input.Split(",") do
        let [| startString; stopString |] = line.Split("-")
        let start = startString |> uint64
        let stop = stopString |> uint64

        for value in [ start..stop ] do
            let digits = value |> float |> digits
            let mutable found = false

            for divider in value |> float |> dividers do
                if not found then
                    let mutable reduced = value |> float
                    let sub = reduced / 10.0 ** (digits - divider) |> floor

                    for i in [ 1 .. digits / divider |> int ] do
                        reduced <- reduced - sub * 10.0 ** (digits - divider * (i |> float))

                    if reduced = 0 then
                        sum <- sum + value
                        found <- true



    printfn $"2-2 solution: {sum}"
