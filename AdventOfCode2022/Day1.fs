module Day1

open Common

let parseElvesCalories (path: string) = 
    path
    |> readAllLines
    |> chunkAtEmptyLines
    |> Seq.map (List.sumBy int)
    |> Seq.sortDescending

let maxCalories (elves: int seq) = 
    Seq.head elves

let sumOfTop3 (elves: int seq) = 
    elves |> Seq.take 3 |> Seq.sum

let day1 path = 
    let elves = parseElvesCalories path

    let day1a = maxCalories elves   

    let day1b = sumOfTop3 elves

    printf "Day 1a: %d\nDay 1b: %d" day1a day1b
    
