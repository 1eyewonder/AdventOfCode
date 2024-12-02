open System
open System.IO

let (|SplitBy|_|) (x: char) (text: string) =
  text.Split(x, StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> Some

let intersect a b =
  Set.intersect (Set.ofList a) (Set.ofList b) |> Set.toList

type Card = {
  Key: string
  WinningNumbers: int list
  YourNumbers: int list
}

let tryParseLine =
  function
  | SplitBy ':' [ key; SplitBy '|' [ SplitBy ' ' winningNumbers; SplitBy ' ' yourNumbers ] ] ->
    {
      Key = key
      WinningNumbers = List.map int winningNumbers
      YourNumbers = List.map int yourNumbers
    }
    |> Some
  | _ -> None

let allCards =
  File.ReadAllText("data.txt").Split("\n")
  |> Array.Parallel.choose (
    tryParseLine
    >> Option.map (fun card -> intersect card.WinningNumbers card.YourNumbers)
    >> Option.map List.length
  )
  |> Array.indexed

(Map.ofList [ for i in 0 .. allCards.Length - 1 -> i, 1 ], allCards)
||> Array.fold (fun map (index, winningCards) ->
  let count = map[index]

  match winningCards with
  | 0 -> map
  | _ ->
    let newCards =
      (map, [ for i in index + 1 .. index + winningCards -> i ])
      ||> List.fold (fun m k ->
        match Map.tryFind k m with
        | Some v -> Map.add k (v + count) m
        | None -> Map.add k count m)

    newCards)
|> Map.values
|> Seq.sum
|> printfn "%A"
