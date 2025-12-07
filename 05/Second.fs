module Day5_2

type Range =
    { mutable Start: uint64
      mutable Stop: uint64 }

let sorter (first: Range) (second: Range) =
    if first.Start = second.Start then
        (if first.Stop < second.Stop then -1 else 1)
    else
        (if first.Start < second.Start then -1 else 1)

let simplify (param: Range list) =
    let mutable result = [| param[0] |]
    let mutable index = 0

    //aggregate overlaps
    for range in param[1..] do
        if result[index].Start <= range.Start && range.Start <= result[index].Stop then
            if result[index].Stop < range.Stop then
                result[index].Stop <- range.Stop
        else
            result <- Array.concat [ result; [| range |] ]
            index <- index + 1

    result |> Array.toList

let second =
    let input = Reader.ReadLines "05\\input.txt"

    let sequence = Seq.toList input

    let ranges, items =
        List.splitAt (List.findIndex (fun x -> x = "") sequence) sequence

    let mutable freshness = []

    for range in ranges do
        let [| startString; stopString |] = range.Split("-")[0..1]
        let start = startString |> uint64
        let stop = stopString |> uint64
        freshness <- freshness @ [ { Start = start; Stop = stop } ]

    freshness <- List.sortWith sorter freshness |> simplify


    let mutable count = 0UL

    for range in freshness do
        count <- count + range.Stop - range.Start + 1UL

    printfn $"5-2 solution {count}"
