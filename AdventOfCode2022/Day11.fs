module Day11

open Common

type Monkey = {
    items: int array
    operation: (int->int64)
    testDenominator: int 
    recipientIndexes: (int * int) 
}

let parseMonkey (lines: string list): Monkey = 
    let parseItems (s: string) = 
        let parts = s.Split(": ")[1]
        parts.Split(", ") |> Array.map int

    let parseOperation (s: string): (int -> int64) =
        let parts = s.Split("= ")
        let [| x; op; y |] = parts[1].Split(' ')

        (fun v ->
            let x2 = if x = "old" then (int64 v) else (int64 x)
            let y2 = if y = "old" then (int64 v) else (int64 y)
            match op with
                | "+" -> x2 + y2
                | "-" -> x2 - y2
                | "*" -> x2 * y2
                | "/" -> x2 / y2
                | _   -> failwithf "Unexpected expression %s" s)

    let parseTest (s: string) = 
        let parts = s.Split("by ")
        int parts[1]
        

    let parseRecipient (s: string) = 
        let parts = s.Split("monkey ")
        parts[1] |> int

    match lines with
    | _::l1::l2::l3::l4::l5::_ -> 
        {
            items = parseItems l1
            operation = parseOperation l2
            testDenominator = parseTest l3
            recipientIndexes = (parseRecipient l4, parseRecipient l5)
        }
    | _ -> failwithf "Unexpected batch of lines: %A" lines


let processRound (monkeys: Monkey array) (counts: int array) = 
    
    
    let processMonkey (i: int) (monkey: Monkey) = 
                
        Array.set counts i (counts[i] + Array.length monkey.items)

        monkey.items |>
            Array.iter (fun item -> 
                            let newScore = int32 ((monkey.operation item) / 3L)
                            let recipientIndex = if (newScore % monkey.testDenominator = 0) then (fst monkey.recipientIndexes) else (snd monkey.recipientIndexes)
                            let recipient = monkeys[recipientIndex] 
                            let newRecipient = {recipient with items = Array.append recipient.items [|newScore|] }
                            Array.set monkeys recipientIndex newRecipient
                            let newMonkey = {monkey with items = Array.empty<int>}
                            Array.set monkeys i newMonkey)

    monkeys 
        |> Array.iteri processMonkey 

let processRound2 (monkeys: Monkey array) (counts: int array) = 

    let modulus = //System.Int32.MaxValue |> int64 |> (+) 1L 
        monkeys 
        |> Seq.map (fun x -> x.testDenominator) 
        |> Set.ofSeq 
        |> Set.fold (fun acc x -> acc * x) 1 
        |> int64
    
    let processMonkey (i: int) (monkey: Monkey) = 
                
        Array.set counts i (counts[i] + Array.length monkey.items)

        monkey.items |>
            Array.iter (fun item -> 
                            let newScore = int32 ((monkey.operation item) %  modulus)
                            let recipientIndex = if (newScore % monkey.testDenominator = 0) then (fst monkey.recipientIndexes) else (snd monkey.recipientIndexes)
                            let recipient = monkeys[recipientIndex] 
                            let newRecipient = {recipient with items = Array.append recipient.items [|newScore|] }
                            Array.set monkeys recipientIndex newRecipient
                            let newMonkey = {monkey with items = Array.empty<int>}
                            Array.set monkeys i newMonkey)

    monkeys 
        |> Array.iteri processMonkey 


let run (path: string) =
    let monkeys = 
        System.IO.File.ReadLines(path)
        |> chunkAtEmptyLines
        |> Seq.map parseMonkey
        |> Seq.toArray

    let counts = Array.create (Array.length monkeys) 0

    let part1Monkeys = Array.copy monkeys

    for rnd in 1..20 do 
        processRound part1Monkeys counts

    let part1 = 
        counts |> Array.mapi (fun i x -> (i, x))
               |> Array.sortByDescending snd
               |> (fun res -> (snd res[0]) * (snd res[1]))

    printf "Part 1: %d\n" part1

    let counts2 = Array.create (Array.length monkeys) 0

    for rnd in 1..10_000 do
        processRound2 monkeys counts2

    let part2 = 
        counts2 |> Array.mapi (fun i x -> (i, x))
                |> Array.sortByDescending snd
                |> (fun res -> (bigint (snd res[0])) * (bigint (snd res[1])))

    printf "Round 2: %A" part2
    ()
