module Day7

open System.Collections.Generic

type Line = 
    | Cd of string
    | Ls
    | Dir of string
    | File of int*string

type DirectoryDetails = {subdirectories: Dictionary<string, DirectoryDetails>; files: Dictionary<string, int>}



let newDir () = {subdirectories = new Dictionary<string, DirectoryDetails>(); files= new Dictionary<string, int>()}





let parseLine (line:string): Line =
    match line.Split(" ") with
    | [| "$"; "cd"; x|] -> Cd x
    | [| "$"; "ls"|]    -> Ls
    | [| "dir"; x|]     -> Dir x
    | [| size; name |]  -> File (int size, name)
    | _ -> failwithf "Unexpected line: '%s'" line


let buildDirectoryTree (path: string) = 
    let root = newDir ()
    let rec processLine (dirs: DirectoryDetails list) (lines: Line list) =
        
        let dir = List.head dirs

        let addSubdir (name: string) = 
            dir.subdirectories.Add(name, newDir ())
            dirs

        let addFile (file: (int*string)) = 
            dir.files.Add(snd file, fst file)
            dirs

        match lines with
        | [] -> [root]
        | (Cd x)::tl when x = "/"  -> processLine [root] tl
        | (Cd x)::tl when x = ".." -> processLine (List.tail dirs) tl
        | (Cd x)::tl               -> processLine (dir.subdirectories[x]::dirs) tl
        | Ls::tl                   -> processLine dirs tl
        | (Dir x)::tl              -> processLine (addSubdir x) tl
        | (File (size,name))::tl   -> processLine (addFile (size,name)) tl
    
    System.IO.File.ReadLines(path)
        |> Seq.map parseLine
        |> Seq.toList
        |> processLine [root]
  

let rec dirSize (dir:DirectoryDetails) = 
    let totalFiles = dir.files.Values |> Seq.sum
    let totalSubDirs = dir.subdirectories.Values |> Seq.sumBy dirSize
    totalFiles + totalSubDirs

let rec walkSubdirectories (dir: DirectoryDetails) = 
    seq { 
        for subdir in dir.subdirectories do 
        yield subdir
        yield! walkSubdirectories subdir.Value
    }

let day7 (path: string) = 
    let tree = buildDirectoryTree path |> List.head

    let totalSize = (dirSize tree)

    printf "Total Size: %d\n" totalSize

    let sizeLimit = 100_000

    let dirSizes = 
        tree
        |> walkSubdirectories
        |> Seq.map (fun kvp -> kvp,(dirSize kvp.Value) )

    let dirsUnderSizeLimit = 
        dirSizes |> Seq.filter (fun x -> (snd x) <= sizeLimit)
        
    let part1 = dirsUnderSizeLimit |> Seq.sumBy (fun x -> snd x) 

    printf "Total size of dirs under 100000: %d\n" part1

    let totalSpace = 70_000_000

    let updateSpace = 30_000_000

    let freeSpace = totalSpace - totalSize

    printf "Free Space: %d\n" freeSpace

    let requiredSpace = updateSpace - freeSpace

    printf "Required Space = %d\n" requiredSpace

    let dirsOverRequiredSpace = dirSizes |> Seq.filter (fun x -> (snd x) > requiredSpace)

    dirsOverRequiredSpace |> Seq.sortBy snd |> Seq.iter (fun x -> printf "%s\t%d\n" ((fst x).Key) (snd x))

    let smallest = dirsOverRequiredSpace |> Seq.sortBy snd |> Seq.head

    printf "\nDelete directory: %s\n" (fst smallest).Key




