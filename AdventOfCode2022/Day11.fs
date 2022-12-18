module Day11

open Common

type Monkey = {items: int list; operation: (int->int); test: (int -> bool); recipientIndexes: (int * int) }

let parseMonkey (lines: string list): Monkey = 
    let parseItems (s: string) = 
        let parts = s.Split(": ")[1]
        parts[1].Split(", ") |> Array.map int

    let parseOperation (s: String): (string -> int) =
        let parts = s.Split("= ")
        let [| x; op; y|] = s.Split(' ')
        (fun (v: string) ->
            let x2 = if x = "old" then (int v) else (int x)
            let y2 = if y = "old" then (int v) else (int y)
            match op with
                | "+" -> x2 + y2
                | "-" -> x2 - y2
                | "*" -> x2 * y2
                | "/" -> x2 / y2
                | _   -> failwith "Unexpected expression %s" s)

    let parseTest (s: string) = 
        let parts = s.Split("by ")
        let denominator = int parts[1]
        (fun (v: int) -> (v % denominator) = 0)

    let parseRecipient (s: string) = 
        let parts = s.Split("monkey ")
        parts[1] |> int

    match lines with
    | l1::l2::l3::l4::l5::l6 -> 



let run (path: string) =
    System.IO.File.ReadLines(path)
    |> 
