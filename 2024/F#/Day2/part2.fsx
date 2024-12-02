open System
open System.IO

let (|Int|_|) (s: string) =
  match Int32.TryParse(s) with
  | true, i -> Some i
  | _ -> None

let tryGetLevels (line: string) =
  line.Split(' ')
  |> Array.choose (fun item ->
    if String.IsNullOrEmpty item |> not then
      (|Int|_|) item
    else
      None)
  |> Array.toList

let (|IncreasingBy|DecreasingBy|Equal|) (x, y) =
  if x < y then IncreasingBy(y - x)
  elif x > y then DecreasingBy(x - y)
  else Equal

type Safety =
  | SafelyIncreasing
  | SafelyDecreasing
  | Unsafe

let rec combinations k lst =
  match k, lst with
  | 0, _ -> [ [] ]
  | _, [] -> []
  | k, x :: xs ->
    let cTail = combinations (k - 1) xs
    let withX = cTail |> List.map (fun l -> x :: l)
    let withoutX = combinations k xs
    withX @ withoutX

let getLevelVariations dampener levels =
  let rec allVariations dampener levels =
    if dampener = 0 then
      [ levels ]
    else
      let variations = combinations (List.length levels - dampener) levels
      variations @ allVariations (dampener - 1) levels

  allVariations dampener levels

let getSafetyLevels levels =
  List.pairwise levels
  |> List.map (function
    | DecreasingBy 1
    | DecreasingBy 2
    | DecreasingBy 3 -> SafelyDecreasing
    | IncreasingBy 1
    | IncreasingBy 2
    | IncreasingBy 3 -> SafelyIncreasing
    | IncreasingBy _
    | DecreasingBy _
    | Equal -> Unsafe)

let levelsAreSafe (levels: Safety list) =
  let allIncreasing =
    levels
    |> List.forall (function
      | SafelyIncreasing -> true
      | SafelyDecreasing
      | Unsafe -> false)

  let allDecreasing =
    levels
    |> List.forall (function
      | SafelyDecreasing -> true
      | SafelyIncreasing
      | Unsafe -> false)

  allIncreasing || allDecreasing

let reportIsSafe dampener =
  tryGetLevels
  >> getLevelVariations dampener
  >> List.exists (getSafetyLevels >> levelsAreSafe)

File.ReadLines("data.txt")
|> Seq.filter (reportIsSafe 1)
|> Seq.length
|> printfn "Answer: %A"
