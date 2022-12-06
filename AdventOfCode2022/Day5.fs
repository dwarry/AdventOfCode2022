module Day5

open Common

/// Set up the initial cargo state based on the first section of the input file. 
let parseInitialCargoState (lines: string seq): (char list) array = 
    // All the lines are of a uniform length, so just take the first. 
    let lineLength = Seq.head lines |> String.length
    
    // work out the number of stacks of crates. 
    let numberOfStacks = (lineLength + 1) / 4
    
    // Urgh - mutable state. Oh well, at least it has O(1) access time for looking up
    // the stacks later on. Initialize it with empty lists. 
    let result = Array.create numberOfStacks List.empty<char>

    // To get the stacks in the right order, process the lines in reverse order, and
    // ignore the one with the stack numbers as it's irrelevant. 
    let cargoLines = lines |> Seq.rev |> Seq.skip 1 

    // Process each line - pluck the crate ids from the expected positions in the line, 
    // and update the array's lists with any non-blank entries 
    let processCargoLine (line: string) = 
        let crates = [for i in 0..(numberOfStacks - 1) -> line[(i*4)+1]]
        List.iteri (fun i ch -> if ch <> ' ' then Array.set result i (ch::result[i])) crates

    cargoLines
        |> Seq.iter processCargoLine

    result

/// Record for an instruction about moving the cargo, which will be taken from the 
/// second section of the input file. 
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
