open System
open System.IO

let (|SplitBy|_|) (x: char) (text: string) =
    text.Split(x, StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> Some

let intersect a b =
    Set.intersect (Set.ofList a) (Set.ofList b) |> Set.toList

let getPointsFromNumbers (matchingNumbers: 'a list) =
    Math.Pow(2.0, float (matchingNumbers.Length - 1)) |> int

type Card =
    { Key: string
      WinningNumbers: int list
      YourNumbers: int list }

let tryParseLine =
    function
    | SplitBy ':' [ key; SplitBy '|' [ SplitBy ' ' winningNumbers; SplitBy ' ' yourNumbers ] ] ->
        { Key = key
          WinningNumbers = List.map int winningNumbers
          YourNumbers = List.map int yourNumbers }
        |> Some
    | _ -> None

File.ReadAllText("data.txt").Split("\n")
|> Array.choose (
    tryParseLine
    >> (Option.map (fun o -> intersect o.WinningNumbers o.YourNumbers))
    >> Option.map getPointsFromNumbers
)
|> Array.fold (+) 0
|> printfn "%A"
