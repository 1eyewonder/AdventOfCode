open System.IO
open System.Text.RegularExpressions

let regex = @"mul\((\d+),(\d+)\)"
let text = File.ReadAllText("data.txt")

Regex.Matches(text, regex)
|> Seq.map (fun m -> int m.Groups.[1].Value * int m.Groups.[2].Value)
|> Seq.sum
|> printfn "Answer: %A"