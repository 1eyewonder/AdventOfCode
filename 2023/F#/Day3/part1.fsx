open System
open System.IO

type Number = {
  Row: int
  Columns: int list
  Number: uint
}

type Symbol = {
  Row: int
  Column: int
  Symbol: char
}

[<RequireQualifiedAccess>]
type Text =
  | Number of Number
  | Symbol of Symbol

module Text =

  let isSymbol =
    function
    | Text.Symbol _ -> true
    | _ -> false

  let tryGetSymbol =
    function
    | Text.Symbol symbol -> Some symbol
    | _ -> None

  let tryGetNumber =
    function
    | Text.Number number -> Some number
    | _ -> None

let (|IsNumber|IsSymbol|IsDot|) (x: char) =
  match UInt32.TryParse(string x) with
  | true, _ -> IsNumber
  | false, _ ->
    match x with
    | '.' -> IsDot
    | _ -> IsSymbol

let sameRow (symbol: Symbol) (number: Number) = symbol.Row = number.Row

let adjacentRow (symbol: Symbol) (number: Number) =
  symbol.Row = number.Row + 1 || symbol.Row = number.Row - 1

let sameColumn (symbol: Symbol) (number: Number) =
  number.Columns |> List.exists (fun column -> column = symbol.Column)

let adjacentcolumn (symbol: Symbol) (number: Number) =
  number.Columns
  |> List.exists (fun column -> column = symbol.Column + 1 || column = symbol.Column - 1)

let horizontallyAdjacent symbol number =
  sameRow symbol number && adjacentcolumn symbol number

let verticallyAdjacent symbol number =
  sameColumn symbol number && adjacentRow symbol number

let diagonallyAdjacent symbol number =
  adjacentRow symbol number && adjacentcolumn symbol number

let (|PartNumber|NotPartNumber|) (symbol, number) =
  if
    horizontallyAdjacent symbol number
    || verticallyAdjacent symbol number
    || diagonallyAdjacent symbol number
  then
    PartNumber
  else
    NotPartNumber

[<RequireQualifiedAccess>]
module List =

  let inline choosePartition
    ([<InlineIfLambda>] filter: 'a -> bool)
    ([<InlineIfLambda>] chooser1: 'a -> 'b option)
    ([<InlineIfLambda>] chooser2: 'a -> 'c option)
    ls
    =
    ls
    |> List.partition filter
    |> fun (x, y) -> (List.choose chooser1 x, List.choose chooser2 y)

let puzzleInput = File.ReadAllText("data.txt").Split("\n")

let parseText row (line: string) =
  line.Trim()
  |> Array.ofSeq
  |> Array.indexed
  |> Array.Parallel.choose (fun (index, character) ->
    match character with
    | IsNumber ->
      Text.Number {
        Row = row
        Columns = List.singleton index
        Number = UInt32.Parse(string character)
      }
      |> Some
    | IsSymbol ->
      Text.Symbol {
        Row = row
        Column = index
        Symbol = character
      }
      |> Some
    | IsDot -> None)

let rec chunkByPred' acc predicate triggerFunction ls =
  match acc, ls with
  | [], y :: ys -> chunkByPred' (y :: acc) predicate triggerFunction ys
  | x :: _, y :: ys when predicate x y ->
    let z = triggerFunction x y
    chunkByPred' (z :: acc) predicate triggerFunction ys
  | _, y :: ys -> acc :: chunkByPred' [ y ] predicate triggerFunction ys
  | _, [] -> if List.isEmpty acc then [] else [ acc ]

let textIsAdjacent (text1: Text) (text2: Text) =
  match text1, text2 with
  | Text.Number { Columns = indices1 }, Text.Number { Columns = indices2 } ->
    (List.last indices1) + 1 = List.head indices2
  | Text.Symbol { Column = index1 }, Text.Symbol { Column = index2 } -> index1 + 1 = index2
  | _ -> false

let bothTextsAreNumbers (text1: Text) (text2: Text) =
  match text1, text2 with
  | Text.Number _, Text.Number _ -> true
  | _ -> false

let combineNumbers (text1: Text) (text2: Text) =
  match text1, text2 with
  | Text.Number {
                  Row = row
                  Columns = indices1
                  Number = num1
                },
    Text.Number {
                  Columns = indices2
                  Number = num2
                } ->
    let newNumber = string num1 + string num2 |> uint

    Text.Number {
      Row = row
      Columns = indices1 @ indices2
      Number = newNumber
    }
  | _ -> failwith "Cannot combine non-numbers"

let aggregateNumbers ls =
  chunkByPred' [] (fun text1 text2 -> textIsAdjacent text1 text2 && bothTextsAreNumbers text1 text2) combineNumbers ls
  |> List.map List.head

let symbols, numbers =
  puzzleInput
  |> Array.Parallel.mapi (fun index text ->
    parseText index text
    |> Array.toList
    |> aggregateNumbers
    |> List.choosePartition Text.isSymbol Text.tryGetSymbol Text.tryGetNumber)
  |> Array.unzip
  |> fun (x, y) -> (List.concat x, List.concat y)

numbers
|> List.fold
  (fun partNumbers number ->
    let newPartNumbers =
      symbols
      |> List.choose (fun symbol ->
        match symbol, number with
        | PartNumber -> Some number.Number
        | NotPartNumber -> None)

    partNumbers @ newPartNumbers)
  []
|> List.fold (+) 0u
|> printfn "%i"
