module Day4

open System.IO

type Range = {min:int; max:int}

let parseRange (s: string): Range = 
    let parts = s.Split('-')

    {min = (int parts[0]); max= (int parts[1])}


let isWithin (x: int) (r: Range) = 
    x >= r.min && x <= r.max

let parseLine (line: string): Range * Range = 
    let elves = line.Split(',')

    (parseRange elves[0], parseRange elves[1])

let rangeContainsTheOther (pair: Range*Range): int = 
    let isContained (r1: Range) (r2: Range) = 
        (isWithin r1.min r2) && (isWithin r1.max r2)

    let r1 = fst pair
    let r2 = snd pair

    if (isContained r1 r2) || (isContained r2 r1) then 1 else 0

let rangeOverlapsTheOther (pair: Range*Range): int = 
    let overlaps (r1: Range) (r2: Range) = 
        (isWithin r1.min r2) || (isWithin r1.max r2)

    let r1 = fst pair
    let r2 = snd pair

    if (overlaps r1 r2) || (overlaps r2 r1) then 1 else 0
    

let day4 (path: string) = 
    let containedCount = 
        File.ReadLines(path)
        |> Seq.map parseLine
        |> Seq.sumBy rangeContainsTheOther

    printf "Part 1: %d\n" containedCount

    let overlapCount = 
        File.ReadLines(path)
        |> Seq.map parseLine
        |> Seq.sumBy rangeOverlapsTheOther

    printf "Part 2: %d\n" overlapCount



