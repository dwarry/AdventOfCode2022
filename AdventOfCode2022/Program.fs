open System
open Day5

printf "%s\n" (Environment.GetCommandLineArgs()[0])

let path = Environment.GetCommandLineArgs()[1]

day5 path
