open System
open Day2

let path = Environment.GetCommandLineArgs()[1]

let score = scoreContest path

printf "Part 1 Score: %d\n" score

let score2 = scoreContest2 path

printf "Part 2 Score: %d\n" score2


