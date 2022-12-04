module Day3
open System.IO


let splitIntoHalves(s: string): string*string =
    let l = s.Length / 2
    s.Substring(0, l), s.Substring(l)


let findCommonElement (pair: string*string): char =
    let s1 = pair |> fst |> Set
    let s2 = pair |> snd |> Set

    Set.intersect s1 s2 |> Set.toSeq |> Seq.head


let findCommonElement2 (group: string seq): char = 
    group 
        |> Seq.map Set
        |> Seq.reduce Set.intersect
        |> Set.toSeq
        |> Seq.head
    
        

let score (ch: char): int =
    if ch >= 'A' && ch <= 'Z' then ((int ch) - (int 'A') + 27)
    else ((int ch) - (int 'a') + 1)


let day3 (path: string) = 
    let part1 = 
        File.ReadLines(path)
        |> Seq.map splitIntoHalves
        |> Seq.map findCommonElement
        |> Seq.sumBy score

    printf "Part 1: %d\n" part1


    let part2 = 
        File.ReadLines(path)
        |> Seq.chunkBySize 3
        |> Seq.map findCommonElement2
        |> Seq.sumBy score

    printf "Part 2: %d\n" part2
