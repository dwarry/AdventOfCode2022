module Day9

type Direction = Left | Right | Up | Down 

type RopePosition = {head:int*int; tail:(int*int) array}

let makeRope tailSize = {head = (0,0); tail = Array.create tailSize (0,0)}

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
    
    let newHead = match dir with
                  | Left  -> (headX - 1, headY)
                  | Right -> (headX + 1, headY)
                  | Up    -> (headX,     headY + 1)
                  | Down  -> (headX,     headY - 1)

    let calcNewTailPos (prev: int*int) (current: int*int) = 
        let tailX,tailY = current
        let newTailDelta = (fst prev - tailX, snd prev - tailY)

        let maxDelta x = System.Math.Clamp(x, -1, 1)
        
        match newTailDelta with
            | (2, dy)  -> (tailX + 1,  tailY + (maxDelta dy))
            | (-2, dy) -> (tailX - 1,  tailY + (maxDelta dy))
            | (dx, 2)  -> (tailX + (maxDelta dx), tailY + 1)
            | (dx, -2) -> (tailX + (maxDelta dx), tailY - 1)
            | _ -> current

    let state = (newHead, 0, Array.create (pos.tail.Length) (0,0))

    let folder st x =
        let prev,i,result = st
        let newTail = calcNewTailPos prev x
        Array.set result i newTail

        (newTail, i+1, result)

    let _,_,newTail = Array.fold folder state pos.tail

    {head= newHead; tail = newTail}

let visRope (ropePosition: RopePosition) = 
    let result = [| for i in 1 .. 50 -> (System.Text.StringBuilder(System.String('.',50))) |]


    let setAt x y ch = result[25 - y].Chars(x+25) <- ch

    ropePosition.tail
        |> Array.iteri (fun i x -> setAt (fst x) (snd x) ((i + 1).ToString()[0]))

    setAt (fst ropePosition.head) (snd ropePosition.head) 'H'

    printf "\n"
    Array.iter  (fun x-> printf "%s\n" (x.ToString())) result

    ropePosition

let run (path:string) =

    let rope1 = makeRope 1

    let instructions = 
        System.IO.File.ReadLines(path)
        |> Seq.map parseLine
        |> Seq.collect expandInstruction


    let path = 
        instructions |> Seq.scan calcNewRopePosition rope1

    let tailPositions =
        path
        |> Seq.map (fun x -> x.tail)
        |> Set.ofSeq

    printf "Part 1: %d\n" (Set.count tailPositions)

    let rope2 = makeRope 9

    let path2 = 
        instructions |> Seq.scan calcNewRopePosition rope2

    let tailPositions2 = 
        path2
        //|> Seq.map visRope
        |> Seq.map (fun x -> Array.last x.tail)
        |> Set.ofSeq

    printf "Part2: %d\n" (Set.count tailPositions2)
