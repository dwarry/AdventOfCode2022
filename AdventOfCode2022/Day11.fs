module Day11

open Common

type Monkey = {items: int array; operation: (int->int); test: (int -> bool); recipientIndexes: (int * int) }

let parseMonkey (lines: string list): Monkey = 
    let parseItems (s: string) = 
        let parts = s.Split(": ")[1]
        parts.Split(", ") |> Array.map int

    let parseOperation (s: string): (int -> int) =
        let parts = s.Split("= ")
        let [| x; op; y |] = parts[1].Split(' ')

        (fun v ->
            let x2 = if x = "old" then v else (int x)
            let y2 = if y = "old" then v else (int y)
            match op with
                | "+" -> x2 + y2
                | "-" -> x2 - y2
                | "*" -> x2 * y2
                | "/" -> x2 / y2
                | _   -> failwithf "Unexpected expression %s" s)

    let parseTest (s: string) = 
        let parts = s.Split("by ")
        let denominator = int parts[1]
        (fun (v: int) -> (v % denominator) = 0)

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


let processRound (monkeys: Monkey array) (counts: int array) = 
    
    let processMonkey (i: int) (monkey: Monkey) = 
                
        Array.set counts i (counts[i] + Array.length monkey.items)

        monkey.items |>
            Array.iter (fun item -> 
                            let newScore = ((monkey.operation item) / 3)
                            let recipientIndex = if (monkey.test newScore) then (fst monkey.recipientIndexes) else (snd monkey.recipientIndexes)
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

    for rnd in 1..20 do 
        processRound monkeys counts

    let part1 = 
        counts |> Array.mapi (fun i x -> (i, x))
               |> Array.sortByDescending snd
               |> (fun res -> (snd res[0]) * (snd res[1]))

    printf "Part 1: %d\n" part1
    ()
