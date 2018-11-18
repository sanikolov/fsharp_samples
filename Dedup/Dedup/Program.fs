open System
open System.IO
open System.Security.Cryptography

let NL = Environment.NewLine

let hash_fn file =
    try
        use sha256 = SHA256.Create()
        use str = File.OpenRead(file)
        BitConverter.ToString(sha256.ComputeHash(str)).Replace("-", "").ToLower()
    with _ as e -> sprintf "dang it %s" e.Message

let stringify (mseq: seq<string*string>) =
    let hashMap = // a bag, for each hash, map it to the # of its occurrences in seq
      mseq 
      |> Seq.fold (fun m (_, hash) -> let count = match Map.tryFind hash m with
                                                  | Some value -> value + 1
                                                  | None -> 1
                                      Map.add hash count m) Map.empty
    let m' = Seq.map (fun (file, hash) -> hash, file) mseq
             |> Seq.filter (fun (hash, _) -> hashMap.Item hash > 1)  // unique hashes filtered out
             |> Seq.sortBy (fun (hash, _) -> hash)
    Seq.isEmpty m' |> not, NL + String.Join(NL, m') + NL

// obtain all files rooted at dirs array of directories
let rec get_files dirs = // idea borrowed from a http://www.fssnip.net snippet 
    if Seq.isEmpty dirs then Seq.empty else
    seq { yield! dirs |> Seq.collect Directory.EnumerateFiles
          yield! dirs |> Seq.collect Directory.EnumerateDirectories |> get_files }

// show duplicate files in specified dirs
let show_dups dirs =
    get_files dirs // full file list
    |> Seq.distinctBy Path.GetFullPath // remove duplicate file occurrences if dirs happen to overlap
    |> Seq.sort  // better locality when getting lengths?      
    |> Seq.map (fun file -> FileInfo(file).Length, file)
    |> Seq.fold (fun m (sz, file) -> let fset = match Map.tryFind sz m with
                                                | Some value -> Set.add file value
                                                | None -> Set.add file Set.empty
                                     Map.add sz fset m) Map.empty   
    |> Map.filter (fun _ fset -> Set.count fset > 1)
    |> Map.map (fun sz fset -> fset
                               |> Seq.map (fun file -> async {return file, hash_fn file})
                               |> Async.Parallel
                               |> Async.RunSynchronously)
    |> Map.iter (fun sz fmap -> let prn, str = stringify fmap
                                if prn then printfn "these sets of dups were found for size %d -> %s" sz str)

[<EntryPoint>]
let main argv =
    try
        if Array.length argv > 0 then
            show_dups argv
        0
    with _ as e -> printfn "unexpected exception caught %s, maybe your filename chars are weird?" e.Message
                   1