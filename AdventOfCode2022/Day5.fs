module Day5

open Common

let parseInitialCargoState (lines: string seq): (char list) array = 
    let lineLength = Seq.head lines |> String.length
    
    let numberOfStacks = (lineLength + 1) / 4
    
    let result = Array.create numberOfStacks List.empty<char>

    let cargoLines = lines |> Seq.rev |> Seq.skip 1 

    let processCargoLine (line: string) = 
        let crates = [for i in 0..(numberOfStacks - 1) -> line[(i*4)+1]]
        List.iteri (fun i ch -> if ch <> ' ' then Array.set result i (ch::result[i])) crates

    cargoLines
        |> Seq.iter processCargoLine

    result

type CraneInstruction = {number: int; fromStack: int; toStack: int}

let parseInstructionLine (line: string): CraneInstruction =
    let parts = line.Split(' ')
    { number = int parts[1]; fromStack = (int parts[3]) - 1; toStack = (int parts[5]) - 1}

let applyInstruction (cargoState: char list array) (instruction: CraneInstruction) = 
    for i = 1 to instruction.number do
        let cargoStack = cargoState[instruction.fromStack]
        let cargo = List.head cargoStack
        let remaining = List.tail cargoStack
        Array.set cargoState instruction.fromStack remaining
        
        let toCargoStack = cargoState[instruction.toStack]
        Array.set cargoState instruction.toStack (cargo::toCargoStack)

let applyInstruction2 (cargoState: char list array) (instruction: CraneInstruction) = 
    let cargoStack = cargoState[instruction.fromStack]
    let cargo = List.take instruction.number cargoStack
    let remaining = List.skip instruction.number cargoStack 
    Array.set cargoState instruction.fromStack remaining
        
    let toCargoStack = cargoState[instruction.toStack]
    Array.set cargoState instruction.toStack (List.append cargo toCargoStack)


let headsOfCargoState (cargoState: char list array) = 
    Array.map (fun lst -> if List.isEmpty lst then ' ' else List.head lst) cargoState

let day5 (path: string) = 
    let lines = System.IO.File.ReadLines(path)
    let parts = chunkAtEmptyLines lines
    let initialState = Seq.head parts
    let instructions = Seq.tail parts |> Seq.head |> Seq.map parseInstructionLine

    let cargoState = parseInitialCargoState initialState
    let cargoState2 = Array.copy cargoState

    instructions        
        |> Seq.iter (applyInstruction cargoState)

    printf "Part1: %A\n" (headsOfCargoState cargoState)
    
    instructions 
        |> Seq.iter (applyInstruction2 cargoState2)
    
    printf "Part2: %A\n" (headsOfCargoState cargoState2)
