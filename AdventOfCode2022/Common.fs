module Common

open System
open System.Collections.Generic
open System.IO

let readAllLines (filePath: string) : seq<string> = 
    seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

let chunkAtEmptyLines (lines: seq<string>): seq<string list> =
    seq {
        let chunk = new List<string>()
        for line in lines do
            if String.IsNullOrWhiteSpace(line) then
                yield List.ofSeq chunk
                chunk.Clear()
            else
                chunk.Add(line)
    
        if chunk.Count > 0 then yield List.ofSeq chunk
    }


