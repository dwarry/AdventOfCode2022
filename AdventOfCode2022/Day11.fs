module Day11

open Common

type Monkey = {items: int64 array; operation: (int64->int64); test: (int64 -> bool); recipientIndexes: (int * int) }

let parseMonkey (lines: string list): Monkey = 
    let parseItems (s: string) = 
        let parts = s.Split(": ")[1]
        parts.Split(", ") |> Array.map int64

    let parseOperation (s: string): (int64 -> int64) =
        let parts = s.Split("= ")
        let [| x; op; y |] = parts[1].Split(' ')

        (fun v ->
            let x2 = if x = "old" then v else (int64 x)
            let y2 = if y = "old" then v else (int64 y)
            match op with
                | "+" -> x2 + y2
                | "-" -> x2 - y2
                | "*" -> x2 * y2
                | "/" -> x2 / y2
                | _   -> failwithf "Unexpected expression %s" s)

    let parseTest (s: string) = 
        let parts = s.Split("by ")
        let denominator = int64 parts[1]
        (fun (v: int64) -> (v % denominator) = 0)

    let parseRecipient (s: string) = 
        let parts = s.Split("monkey ")
        parts[1] |> int

    match lines with
    | _::l1::l2::l3::l4::l5::_ -> 
        {
            items = parseItems l1
            operation = parseOperation l2
            test = parseTest l3
            recipientIndexes = (parseRecipient l4, parseRecipient l5)
        }
    | _ -> failwithf "Unexpected batch of lines: %A" lines


let processRound (monkeys: Monkey array) (counts: int64 array) = 
    
    let processMonkey (i: int) (monkey: Monkey) = 
                
        Array.set counts i (counts[i] + (int64 (Array.length monkey.items)))

        monkey.items |>
            Array.iter (fun item -> 
                            let newScore = ((monkey.operation item) / 3L)
                            let recipientIndex = if (monkey.test newScore) then (fst monkey.recipientIndexes) else (snd monkey.recipientIndexes)
                            let recipient = monkeys[recipientIndex] 
                            let newRecipient = {recipient with items = Array.append recipient.items [|newScore|] }
                            Array.set monkeys recipientIndex newRecipient
                            let newMonkey = {monkey with items = Array.empty<int64>}
                            Array.set monkeys i newMonkey)

    monkeys 
        |> Array.iteri processMonkey 


let processRound2 (monkeys: Monkey array) (counts: int64 array) = 
    
    let processMonkey (i: int) (monkey: Monkey) = 
                
        Array.set counts i (counts[i] + (int64 (Array.length monkey.items)))

        monkey.items |>
            Array.iter (fun item -> 
                            let newScore = (monkey.operation item)
                            let recipientIndex = if (monkey.test newScore) then (fst monkey.recipientIndexes) else (snd monkey.recipientIndexes)
                            let recipient = monkeys[recipientIndex] 
                            let newRecipient = {recipient with items = Array.append recipient.items [|newScore|] }
                            Array.set monkeys recipientIndex newRecipient
                            let newMonkey = {monkey with items = Array.empty<int64>}
                            Array.set monkeys i newMonkey)

    monkeys 
        |> Array.iteri processMonkey 


let run (path: string) =
    let monkeys = 
        System.IO.File.ReadLines(path)
        |> chunkAtEmptyLines
        |> Seq.map parseMonkey
        |> Seq.toArray

    let counts = Array.create (Array.length monkeys) 0L

    let monkeys1 = Array.copy monkeys

    for rnd in 1..20 do 
        processRound monkeys1 counts

    let part1 = 
        counts |> Array.mapi (fun i x -> (i, x))
               |> Array.sortByDescending snd
               |> (fun res -> (snd res[0]) * (snd res[1]))
    
    printf "Part 1: %d\n" part1
    
    let monkeys2 = Array.copy monkeys
    let counts2 = Array.create (Array.length monkeys) 0L

    for rnd in 1..10_000 do 
        processRound2  monkeys2 counts2
    
    let part2 = 
        counts2 |> Array.mapi (fun i x -> (i, x))
                |> Array.sortByDescending snd
                |> (fun res -> (snd res[0]) * (snd res[1]))


    printf "Part 2: %d\n" part2
    
    ()
