module Day10

type CpuState = { xRegister: int; tickCount: int; }

type Instruction = (CpuState -> CpuState)

let parseLine (line: string): Instruction = 
    let parts = line.Split(' ')

    match parts with
    | [| "noop" |]    -> fun cpu -> {cpu with tickCount = cpu.tickCount + 1} 
    | [| "addx"; x |] -> fun cpu -> {cpu with xRegister = cpu.xRegister + (int x); tickCount = cpu.tickCount + 2}
    | _ -> failwithf "Unexpected line: %s" line

let rec processInstructions (instructions: Instruction list) (cpu: CpuState) = 
    match instructions with
    | [] -> cpu
    | x::xs  -> processInstructions xs (x cpu) 

let run (path: string) =
    let states = 
        System.IO.File.ReadLines(path)
        |> Seq.map parseLine
        |> Seq.scan (fun cpu instruction -> instruction cpu) {xRegister=1; tickCount=0}
        |> Seq.toArray

    let findXAtStartOfTick tick = 
        let nearest = states |> Array.findIndex (fun x -> x.tickCount >= tick)
        states[nearest - 1].xRegister


    let selectedCycles = 
        Seq.unfold (fun st -> Some (st, st + 40)) 20
        |> Seq.takeWhile (fun x -> x <= 220)                 
    
    let selectedValues =
        selectedCycles
        |> Seq.map (fun x -> (x, findXAtStartOfTick x))


    let part1 = selectedValues |> Seq.sumBy (fun x -> (fst x) * (snd x))

    printf "Part 1: %d\n" part1

    printf "Part 2:\n"

    let result = seq {
        for i = 1 to 240 do
            let j = ((i - 1) % 40)

            let x = findXAtStartOfTick i
            let pmin = x - 1
            let pmax = x + 1
            
            if (j >= pmin) && (j <= pmax) then 
                yield '#' 
            else 
                yield '.'
    } 
                 
    let lines = result |> Seq.chunkBySize 40 |> Seq.map (fun x -> System.String(x))

    Seq.iter (fun line -> printf "%s\n" line) lines
    

     
