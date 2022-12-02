module Day2

type RockPaperScissors = Rock | Paper | Scissors

let parseTheirs (ch: char): RockPaperScissors = 
    match ch with
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    | _   -> failwithf "Unknown value: %A" ch

let parseMine (ch: char): RockPaperScissors = 
    match ch with
    | 'X' -> Rock
    | 'Y' -> Paper
    | 'Z' -> Scissors
    | _   -> failwithf "Unknown value: %A" ch

let parseLine (line: string) =
    (parseTheirs line[0], parseMine line[2])

let scoreRound (choices: RockPaperScissors * RockPaperScissors) = 
    let (theirs, mine) = choices

    let shapeScore = 
        match mine with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let outcomeScore = 
        match (mine, theirs) with
        | (Rock, Paper) -> 0
        | (Rock, Scissors) -> 6
        | (Paper, Rock) -> 6
        | (Paper, Scissors) -> 0
        | (Scissors, Rock) -> 0
        | (Scissors, Paper) -> 6
        | _ -> 3

    shapeScore + outcomeScore

let scoreContest path = 
    System.IO.File.ReadLines(path)
    |> Seq.map parseLine
    |> Seq.sumBy scoreRound
