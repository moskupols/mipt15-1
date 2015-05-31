// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let explode (s:string) =
    [for c in s -> c]

let compress1 cs =
    let rec compress1' acc acclast = function
    | h::t when h = fst acclast -> compress1' acc (fst acclast, 1 + (snd acclast)) t
    | h::t -> compress1' (acclast::acc) (h, 1) t
    | []   -> acclast::acc
    in
    match cs with
    | h::t -> compress1' [] (h, 1) t |> List.rev
    | []   -> []

let decompress1 ccs =
    ccs
    |> List.map (fun (ch, count) -> Seq.init count (fun _ -> ch))
    |> List.fold Seq.append Seq.empty
    |> Seq.toList

"aabbba"
|> explode
|> compress1
|> decompress1
|> ignore


type CompressedAtom<'a, 'b> = Single of 'a | Repetition of ('b*int)

let compress2 (charToId: ('a -> 'b)) (cs: list<'a>) =
    let repeatAtom = function
    | Single c -> Repetition (charToId c, 2)
    | Repetition (id, n) -> Repetition (id, (n+1))
    let matchesAtom atom ch =
        match atom with
        | Single c -> c = ch
        | Repetition (id, n) -> id = charToId ch
    let rec compress2' acc acclast = function
    | h::t when matchesAtom acclast h -> compress2' acc (repeatAtom acclast) t
    | h::t -> compress2' (acclast::acc) (Single h) t
    | []   -> acclast::acc
    in
    match cs with
    | h::t -> compress2' [] (Single h) t |> List.rev
    | []   -> []

let decompress2 idToChar ccs =
    ccs
    |> List.map (
        function
        | Single c -> seq{ yield c }
        | Repetition (c, n) -> 
            let ch = idToChar c in
            Seq.init n (fun _ -> ch))
    |> List.fold Seq.append Seq.empty
    |> Seq.toList

"aabbba"
|> explode
|> compress2 (fun (ch:char) -> int(ch))
|> decompress2 (fun (id:int) -> char(id))
|> ignore

"aabbba"
|> explode
|> compress2 (fun x -> x)
|> decompress2 (fun x -> x)
|> ignore

[3;1;4;4;4;4;1;1;5]
|> compress2 (fun x -> x * 10)
|> decompress2 (fun x -> x / 10)
|> ignore


let choose123 n =
    let rec choose123' acc accLen =
        let last = if accLen = 0 then 1 else List.head acc
        seq {
            if accLen = n then
                yield acc
            else
                for i in 1..4-last do
                    yield! choose123' (i::acc) (accLen+1)
        }
    in
    choose123' [] 0
    |> Seq.toList

choose123 0
choose123 1
choose123 2
choose123 3



type version = {major: int; minor: int; release: int; build: int}

let sortVersions = List.sortBy (function
    | {major=a; minor=b; release=c; build=d} -> (a,b,c,d))

let list = [
     {major=2; minor=3; release=12; build=1182}
    ;{major=2; minor=1; release=22; build=1382}
    ;{major=1; minor=7; release=52; build=5182}
    ;{major=2; minor=1; release=21; build=1223}
    ;{major=2; minor=3; release=12; build=1196}]

sortVersions list


open System
open System.IO
open System.Collections

let loadPairDict = // filename =
    File.ReadAllLines(filename)
    |> Seq.take 100
    |> Seq.map (fun (s:string) -> s.Split [| ' '; '\t' |])
    |> Seq.map Array.toSeq
    |> Seq.fold Seq.append Seq.empty
    |> Seq.filter (String.exists (Char.IsWhiteSpace >> (not)))
    |> Seq.toList
    |> (fun ws -> List.zip (List.rev ws |> List.tail |> List.rev) (List.tail ws))
    |> List.sort
    |> List.toSeq
    |> Seq.groupBy fst
    |> Seq.fold (fun acc (a, bs) -> Map.add a (Seq.map snd bs |> Seq.toArray) acc) Map.empty


let randomText (pairDict: Map<string, string array>) (rng: Random) =
    let rec generator start =
        let nextWord w =
            let conts = pairDict.Item start
            conts.[rng.Next(conts.Length)]
        seq {
            yield start
            yield! generator (nextWord start)
        }
    in generator

let pairDict = loadPairDict @"D:\picture.txt"
let printExample randomSeed textStart =
    randomText pairDict (Random randomSeed) textStart
    |> Seq.take 30
    |> Seq.toList
    |> printfn "%A"


printExample 45 "Lord"
printExample 42 "Lord"
printExample 42 "The"

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
