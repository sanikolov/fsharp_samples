open System
open System.IO
open System.Text
open System.Security.Cryptography

// Slavcho Nikolov, +1-781-530-6394 snikolov@autologictech.com

let a_phrase = @"poultry outwits ants"
let a_wfile = @"wordlist"
let a_md5tgt = ["e4820b45d2277f3844eac66c903e84be"; "23170acc097c24edb98fc5488ab033fe";
                "665e5bcb0c20062fe8abaaf4628bb154"; "4624d200580677270a54ccff86b9610e"] |> Set.ofList

let md5fun (s: string) = 
    use m = MD5.Create()
    m.ComputeHash(Encoding.UTF8.GetBytes(s)) |> Array.fold (fun a b -> a + sprintf "%02x" b) ""

let not_special c = c <> ' ' && c <> '\''

let words_to_bag (w: string) =
   w.ToCharArray()
   |> Array.filter not_special  
   |> Array.fold (fun map key -> let v = match (Map.tryFind key map) with
                                         | Some idx -> idx + 1
                                         | None -> 1
                                 Map.add key v map) Map.empty
let conv_bag bag = bag |> Map.toList |> List.map fst |> Set.ofSeq

// subtract m2 from m1, return None if subtraction not possible
// must pass Map.empty as last param
let rec bag_diff (m1 : Map<'a,int>) (m2 : Map<'a,int>) acc =
    let merge map1 map2 = Map.fold (fun acc k v -> Map.add k v acc) map1 map2
    if Map.isEmpty m2 then Some (merge m1 acc)
    else begin
         let kv = m2 |> Seq.head
         let (k2, v2) = (kv.Key, kv.Value)
         let m2' = Map.remove k2 m2
         match Map.tryFind k2 m1 with
         | Some v1 ->  let m1' = Map.remove k2 m1
                       if v1 < v2 then None
                       else begin
                            let subt = bag_diff m1' m2'
                            let acc' = if v1 = v2 then acc else Map.add k2 (v1 - v2) acc
                            subt acc'
                       end
         | _ -> None            
    end


// pick out only words that do NOT intersect with the anagram
let phrase_filter (wbag: Map<char,int>) (word: string) : bool =
    let wmap = words_to_bag word |> Map.remove '\'' // apostrophe is excepted    
    bag_diff wbag wmap Map.empty <> None

let bag2str (bag: Map<char, int>) =
    let strs = Map.map (fun k v -> sprintf "%c:%d" k v) bag |> Map.toArray |> Array.map snd
    "[" + String.Join(",", strs) + "]"

let set2str (strset : Set<string>) = "{" + String.Join(",", strset) + "}"

let rec split_set s acc1 acc2 =
    if Set.isEmpty s then acc1, acc2
    else begin
        let el = Set.maxElement s
        let s' = Set.remove el s
        let arg1, arg2 = if Set.count acc1 > Set.count acc2 then
                            acc1, (Set.add el acc2)
                         else
                            (Set.add el acc1), acc2            
        split_set s' arg1 arg2     
    end

// inp_words is a seq of strings to try forming anagrams from
// inp_phrase to form anagrams from, tgt_hash is the target hash we're trying to match
let solver inp_words inp_phrase_bag tgt_hash extra_filter =
    let rec hash_matches (p : string list list) acc =
        match p with
        | [] -> (List.isEmpty acc |> not, acc)
        | l :: lx -> let anagram = String.Join(" ", l)
                     let h1 = md5fun anagram                     
                     hash_matches lx (if Set.contains h1 tgt_hash then l :: acc else acc)    
    let rec search (wbag: Map<char,int>) (words : Set<string>) level acc =
        if Seq.isEmpty words then acc
        else begin
            // filtered is the set of all words that can possibly be part of the letters in wbag
            let filtered = words |> Seq.filter (phrase_filter wbag) |> Set.ofSeq
            let filtered' = filtered |> Set.filter (extra_filter level)  // prune some more # of loop iterations
            let rec loop wordset acc' =                
                if Set.isEmpty wordset then acc'
                else begin
                    let word = Set.maxElement wordset         // take some element from the doubly filtered set of words to try
                    let wbag' = words_to_bag word             // construct a bag filter for the next descent
                    let remainder = Set.remove word wordset   // will loop over this ever decreasing set                                         
                    let bag = bag_diff wbag wbag' Map.empty |> Option.get                        
                    //if level = 0 then printfn "%d thr %d: doing word %s, %d remain; %A" System.Threading.Thread.CurrentThread.ManagedThreadId level word (Set.count remainder) acc'
                    let param = if Map.isEmpty bag then
                                   [word] :: acc'
                                else begin                                   
                                   let rest = Set.remove word filtered // recursive descent with this subset
                                   let result = search bag rest (level+1) []
                                   let descent = List.map (fun wlist -> word :: wlist) result
                                   descent @ acc' // join result of this descent with previous results
                                end
                    let matched = if level = 0 then begin
                                      let hmatches, result = hash_matches param []
                                      if hmatches then result else acc'
                                  end
                                  else param
                    loop remainder matched                    
                end
            if level > 0 then               
               loop filtered' [] // don't parallelize subsequent descents
            else begin
               let split s = split_set s Set.empty Set.empty
               let set1, set2 = split filtered'
               let (s1, s2), (s3, s4) = split set1, split set2
               [s1;s2;s3;s4]
               |> Seq.map (fun s -> async { return (loop s []) })
               |> Async.Parallel |> Async.RunSynchronously |> List.concat
            end
        end
    search inp_phrase_bag inp_words 0 []

[<EntryPoint>]
let main argv = 
    let extra_filter1 = function  // prune recursive descents
        | 0 -> (fun (w:string) -> w.Length >= 7)
        | 1 -> (fun w -> w.Length >= 5)
        | 2 -> (fun w -> w.Length >= 4)
        | _ -> (fun _ -> false)  // allow only 3 levels of recursion, 3 words max
    let extra_filter2 = function
        | 0 -> (fun (w:string) -> w.Length >= 2)
        | 1 -> (fun w -> w.Length >= 7)
        | 2 -> (fun w -> w.Length >= 9)
        | _ -> (fun _ -> false)
    let phrase_bag = words_to_bag a_phrase
    let input_seq = File.ReadAllLines(a_wfile) |> Set.ofSeq
    let solution1 = solver input_seq phrase_bag a_md5tgt extra_filter1
    let solution2 = solver input_seq phrase_bag a_md5tgt extra_filter2
    let solutions = [solution1; solution2] |> List.concat
    printfn "%d solutions" (List.length solutions)
    List.iter (fun x -> printfn "%A" x) solutions
    0
