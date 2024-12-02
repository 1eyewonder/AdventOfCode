open System
open System.IO

let puzzleInput = File.ReadAllText("data.txt")
let getLines (text: string) = text.Split("\n")

let rec getFirstDigitFromLine (text: char list) =
  match text with
  | [] -> None
  | 'o' :: 'n' :: 'e' :: _ -> Some "1"
  | 't' :: 'w' :: 'o' :: _ -> Some "2"
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> Some "3"
  | 'f' :: 'o' :: 'u' :: 'r' :: _ -> Some "4"
  | 'f' :: 'i' :: 'v' :: 'e' :: _ -> Some "5"
  | 's' :: 'i' :: 'x' :: _ -> Some "6"
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> Some "7"
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> Some "8"
  | 'n' :: 'i' :: 'n' :: 'e' :: _ -> Some "9"
  | head :: tail ->
    match Int32.TryParse(string head) with
    | true, _ -> Some(string head)
    | _ -> getFirstDigitFromLine tail

let rec getLastDigitFromLine (text: char list) =
  match text with
  | [] -> None
  | 'e' :: 'n' :: 'o' :: _ -> Some "1"
  | 'o' :: 'w' :: 't' :: _ -> Some "2"
  | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> Some "3"
  | 'r' :: 'u' :: 'o' :: 'f' :: _ -> Some "4"
  | 'e' :: 'v' :: 'i' :: 'f' :: _ -> Some "5"
  | 'x' :: 'i' :: 's' :: _ -> Some "6"
  | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> Some "7"
  | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> Some "8"
  | 'e' :: 'n' :: 'i' :: 'n' :: _ -> Some "9"
  | head :: tail ->
    match Int32.TryParse(string head) with
    | true, _ -> Some(string head)
    | _ -> getLastDigitFromLine tail

getLines puzzleInput
|> Array.choose (fun line ->
  let firstDigit = List.ofSeq line |> getFirstDigitFromLine
  let secondDigit = List.ofSeq line |> List.rev |> getLastDigitFromLine

  match firstDigit, secondDigit with
  | Some firstDigit, Some secondDigit -> sprintf "%s%s" firstDigit secondDigit |> int |> Some
  | _ -> None)
|> Array.fold (+) 0
|> printfn "%i"
