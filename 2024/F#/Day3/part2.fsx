open System
open System.IO
open System.Text.RegularExpressions

let regex1 = @"mul\((\d+),(\d+)\)"
let regex2 = @"don't\(\).*?do\(\)|$"

let text = File.ReadAllText("data.txt")
let newText = Regex.Replace(text, regex2, String.Empty, RegexOptions.Singleline)

Regex.Matches(newText, regex1)
|> Seq.map (fun m -> int m.Groups.[1].Value * int m.Groups.[2].Value)
|> Seq.sum
|> printfn "Answer: %A"
