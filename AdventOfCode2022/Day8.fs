module Day8

let filterAndConvertToSet (items: ((int*int) * bool) seq): Set<int*int> = 
    items
        |> Seq.filter (fun x -> snd x)
        |> Seq.map fst
        |> Set.ofSeq

let isVisibleFromLeft (lines: string array) (row: int) (col: int): bool = 
    let targetHeight = lines[row][col]
    lines[row][..(col - 1)] |> Seq.forall (fun x -> x < targetHeight)

let isTaller (target: char) (rest: char seq) = 
    Seq.forall (fun x -> target > x) rest


let visibleFromLeft (lines: string array) (row: int): Set<int*int> = 
    let line = lines[row]
    
    seq {
        for i in 1..(line.Length - 2) do 
            let target = line[i]
            let rest = line[..(i - 1)]
            let isVisible = isTaller target rest
            yield ((row, i), isVisible)
    }
    |> filterAndConvertToSet

let visibleFromRight (lines: string array) (row: int): Set<int*int> = 
    let line = lines[row]
    seq {
        for i in (line.Length - 2) .. -1 .. 1 do
            let target = line[i]
            let rest = line[(i + 1)..]
            let isVisible = isTaller target rest
            yield ((row, i), isVisible)
    }
    |> filterAndConvertToSet

let visibleFromTop (lines: string array) (col: int): Set<int*int> = 
    seq {
        for i in 1..(lines.Length - 2) do 
            let target = lines[i][col]
            let rest = lines[..(i - 1)] |> Seq.map (fun x -> x[col])
            let isVisible = isTaller target rest
            yield ((i, col), isVisible)
    }
    |> filterAndConvertToSet

let visibleFromBottom (lines: string array) (col: int): Set<int*int> = 
    seq {
        for i in (lines.Length - 2) .. -1 .. 1 do 
            let target = lines[i][col]
            let rest = lines[(i + 1)..] |> Seq.map (fun x -> x[col])
            let isVisible = isTaller target rest
            yield ((i, col), isVisible)
            
    }
    |> filterAndConvertToSet

let scenicScoreLeft (lines: string array) (position: int*int): int =
    let row, col = position
    let line = lines[row]
    let height = line[col]
    let equalHeightPos = line[..(col - 1)] 
                         |> Seq.tryFindIndexBack (fun x -> x >= height) 
    
    match equalHeightPos with
    | None -> col
    | Some x -> col - x 

    


let scenicScoreRight (lines: string array) (position: int*int): int = 
    let row, col = position
    let line = lines[row]
    let height = line[col]
    let equalHeightPos = line[(col + 1)..] |> Seq.tryFindIndex (fun x -> x >= height)

    match equalHeightPos with
    | None -> (line.Length - 1 - col)
    | Some x -> x + 1

let scenicScoreTop (lines: string array) (position: int*int): int = 
    let row, col = position
    
    let height = lines[row][col]

    let equalHeightPos = lines[..(row - 1)]
                         |> Seq.map (fun x -> x[col])
                         |> Seq.tryFindIndexBack (fun x -> x >= height)

    match equalHeightPos with
    | None -> row
    | Some x -> row - x 

let scenicScoreBottom (lines: string array) (position: int*int): int = 
    let row, col = position
    
    let height = lines[row][col]

    let equalHeightPos = lines[(row + 1)..]
                         |> Seq.map (fun x -> x[col])
                         |> Seq.tryFindIndex (fun x -> x >= height)

    match equalHeightPos with
    | None -> (lines.Length - 1 - row)
    | Some x -> x + 1

let scenicScore (lines: string array) (position: int*int): int = 
    let l = scenicScoreLeft lines position
    let r = scenicScoreRight lines position
    let t = scenicScoreTop lines position
    let b = scenicScoreBottom lines position
    l * r * t * b

let interiorVisibleTrees (lines: string array) : Set<int*int> = 
    
    let horizontalVisible = 
        Set.unionMany <| seq { for i in 1..(lines.Length - 2) do 
                                yield Set.union (visibleFromLeft lines i) (visibleFromRight lines i)}
        
    let verticalVisible = 
        Set.unionMany <| seq { for i in 1..(lines[0].Length - 2) do 
                                yield Set.union (visibleFromTop lines i) (visibleFromBottom lines i)}

    Set.union horizontalVisible verticalVisible 


let run (path:string) = 
    let lines = System.IO.File.ReadAllLines(path)

    let perimeter = 2 * ((lines.Length - 1) + (lines[0].Length - 1))

    let interiorTrees = interiorVisibleTrees lines 

    let interiorCount = Set.count interiorTrees

    let totalVisible = perimeter + interiorCount

    printf "Part 1: %d\n" totalVisible

    let highestScenicScore = 
        interiorTrees
        |> Set.fold (fun st item -> let score = scenicScore lines item
                                    if score > (fst st) then (score, item) else st) (0, (0,0))

    printf "Part 2: %d %A\n" (fst highestScenicScore) (snd highestScenicScore)
        

