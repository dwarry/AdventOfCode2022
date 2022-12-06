module Day6

let day6 (path: string) = 
    let s = System.IO.File.ReadLines(path) |> Seq.head

    let findMarker (l: int) (s: string) = 
        let isMatch (i: int) = 
            let precedingChars = s[(i - l + 1)..i]
            let charSet = Set(precedingChars)
            (Set.count charSet) = l

        let find (i: int) = 
            if isMatch i then Some i else None
    
        seq { l..s.Length} |> Seq.pick find 


    let result1 = findMarker 4 s

    printf "Part 1: %d\n" (result1 + 1)

    let result2 = findMarker 14 s

    printf "Part 2: %d\n" (result2 + 1)

