open System
open Day2

let path = Environment.GetCommandLineArgs()[1]

let score = scoreContest path

printf "Score: %d\n" score


