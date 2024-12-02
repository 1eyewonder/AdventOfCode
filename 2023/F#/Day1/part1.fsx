open System
open System.IO

let puzzleInput = File.ReadAllText("data.txt")
let getLines (text: string) = text.Split("\n")

let rec getFirstDigitFromLine (text: char list) =
  match text with
  | [] -> None
  | head :: tail ->
    match Int32.TryParse(string head) with
    | true, _ -> Some(string head)
    | _ -> getFirstDigitFromLine tail

getLines puzzleInput
|> Array.choose (fun line ->
  let firstDigit = List.ofSeq line |> getFirstDigitFromLine
  let secondDigit = List.ofSeq line |> List.rev |> getFirstDigitFromLine

  match firstDigit, secondDigit with
  | Some firstDigit, Some secondDigit -> sprintf "%s%s" firstDigit secondDigit |> int |> Some
  | _ -> None)
|> Array.fold (+) 0
|> printfn "%i"
