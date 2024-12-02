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

let reportIsSafe = tryGetLevels >> getSafetyLevels >> levelsAreSafe

File.ReadLines("data.txt")
|> Seq.filter reportIsSafe
|> Seq.length
|> printfn "Answer: %A"
