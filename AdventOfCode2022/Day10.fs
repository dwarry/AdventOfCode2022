module Day10

type CpuState = { xRegister: int }

type Instruction = {cyclesUntilCompletion: int; update: (CpuState -> CpuState)}

let parseLine (line: string): Instruction = 
    let parts = line.Split(' ')

    match parts with
    | [| "noop" |] -> {cyclesUntilCompletion = 1; update = id}
    | [| "addx"; x |] -> {cyclesUntilCompletion = 3; update = fun cpu -> {cpu with xRegister = cpu.xRegister + (int x)}
    | _ -> failwithf "Unexpected line: %s" line

let rec processInstructions (instructions: Instruction list) (cpu: CpuState) = 
    match instructions with
    | [] -> cpu
    | x::xs when x.cyclesUntilCompletion = 0 -> processInstructions xs (x.update cpu) 

let run (path: string) =
    ()
