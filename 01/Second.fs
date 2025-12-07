module Day1_2

let second =
    let mutable sum = 50
    let mutable counter = 0

    for line in Reader.ReadLines "01\\input.txt" do
        let fileValue = line[1..] |> int
        counter <- counter + fileValue / 100
        let signedValue = fileValue % 100 * if line[0] = 'L' then -1 else 1
        let pre = sum

        if signedValue <> 0 then
            sum <- sum + signedValue

            if pre <> 0 && (sum <= 0 || sum >= 100) then
                counter <- counter + 1

            sum <- (sum + 100) % 100

    printfn $"1-2 solution: {counter}"
