module Day9

type Direction = Left | Right | Up | Down 

type RopePosition = {head:int*int; tail:int*int}

let parseLine (line: string): Direction*int = 
    match line.Split(' ') with
    | [|"L"; n|] -> Left, (int n)
    | [|"R"; n|] -> Right, (int n)
    | [|"U"; n|] -> Up, (int n)
    | [|"D"; n|] -> Down, (int n)
    | _ -> failwithf "Unexpected input: %s" line

let expandInstruction (instruction: Direction*int): Direction seq = 
    seq {
        let dir, count = instruction
        for i = 1 to count do
            yield dir
    }

let calcNewRopePosition (pos: RopePosition) (dir: Direction): RopePosition = 
    
    let headX,headY = pos.head
    let tailX,tailY = pos.tail

    let newHead = match dir with
                  | Left  -> (headX - 1, headY)
                  | Right -> (headX + 1, headY)
                  | Up    -> (headX,     headY + 1)
                  | Down  -> (headX,     headY - 1)

    let newTailDelta = (fst newHead - tailX, snd newHead - tailY)

    let newTail = match newTailDelta with
                  | (2, dy)  -> (tailX + 1,  tailY + dy)
                  | (-2, dy) -> (tailX - 1,  tailY + dy)
                  | (dx, 2)  -> (tailX + dx, tailY + 1)
                  | (dx, -2) -> (tailX + dx, tailY - 1)
                  | _ -> pos.tail

    {head= newHead; tail = newTail}


let run (path:string) =
    let path = 
        System.IO.File.ReadLines(path)
        |> Seq.map parseLine
        |> Seq.collect expandInstruction
        |> Seq.scan calcNewRopePosition {head=(0,0); tail=(0,0)}

    let tailPositions =
        path
        |> Seq.map (fun x -> x.tail)
        |> Set.ofSeq

    printf "Part 1: %d\n" (Set.count tailPositions)
