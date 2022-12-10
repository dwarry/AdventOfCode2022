module Day9

type Direction = Left | Right | Up | Down 

type Instruction = {dir:Direction; count:int}

type RopePosition = {head:int*int; tail:int*int}

let parseLine (line: string): Instruction = 
    match line.Split(' ') with
    | [|"L"; n|] -> Left (int n)
    | [|"R"; n|] -> Right (int n)
    | [|"U"; n|] -> Up (int n)
    | [|"D"; n|] -> Down (int n)
    | _ -> failwithf "Unexpected input: %s" line

let calcNewRopePosition (pos: RopePosition) (dir: Direction): RopePosition = 
    
    let headX,headY = pos.head
    let tailX,tailY = pos.tail

    let newHead = match dir with
                  | Left  -> (headX - 1, headY)
                  | Right -> (headX + 1, headY)
                  | Up    -> (headX,     headY)
                  | Down  -> (headX,     headY)

    let newTailDelta = (fst newHead - tailX, snd newHead - tailY)

    let newTail = match newTailDelta with
                  | (2, dy)  -> (tailX + 1,  tailY + dy)
                  | (-2, dy) -> (tailX - 1,  tailY + dy)
                  | (dx, 2)  -> (tailX + dx, tailY + 1)
                  | (dx, -2) -> (tailX + dx, tailY - 1)
                  | _ -> pos.tail

    {head= newHead; tail = newTail}

let processInstruction (pos: RopePosition) (instruction: Instruction) = 
    


let run (path:string) = ()
