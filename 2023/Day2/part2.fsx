open System
open System.IO
open System.Text.RegularExpressions

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let (|SplitBy|_|) x (text: string) = text.Split(x) |> Array.toList |> Some

let (|IsGreen|_|) (text: string) =
    match text.ToLower() with
    | ParseRegex @"\s*([\d]*)\s*green" [ number ] -> Some(int number)
    | _ -> None

let (|IsBlue|_|) (text: string) =
    match text.ToLower() with
    | ParseRegex @"\s*([\d]*)\s*blue" [ number ] -> Some(int number)
    | _ -> None

let (|IsRed|_|) (text: string) =
    match text.ToLower() with
    | ParseRegex @"\s*([\d]*)\s*red" [ number ] -> Some(int number)
    | _ -> None

type ColoredCube =
    | Red of int
    | Blue of int
    | Green of int

module ColoredCube =

    let tryGetFromText =
        function
        | IsGreen num -> Green num |> Some
        | IsBlue num -> Blue num |> Some
        | IsRed num -> Red num |> Some
        | _ -> None

type GameResult = { Red: int; Blue: int; Green: int }

module GameResult =

    let empty = { Red = 0; Blue = 0; Green = 0 }

    let getCubePower gameResult =
        gameResult.Red * gameResult.Blue * gameResult.Green

let getMaxCubeQty (cubes: ColoredCube[]) =
    cubes
    |> Array.fold
        (fun results cube ->
            match cube with
            | Red num ->
                match results.Red < num with
                | true -> { results with Red = num }
                | false -> results
            | Blue num ->
                match results.Blue < num with
                | true -> { results with Blue = num }
                | false -> results
            | Green num ->
                match results.Green < num with
                | true -> { results with Green = num }
                | false -> results)
        GameResult.empty

// 1 red, 1 green, 1 blue
let getCubesFromHandfullText (cubeDelimiter: char) (text: string) =
    text.Split(cubeDelimiter) |> Array.choose ColoredCube.tryGetFromText

let getHandfullsFromText (handDelimiter: char) (cubeDelimiter: char) (text: string) =
    text.Split(handDelimiter)
    |> Array.collect (fun line -> getCubesFromHandfullText cubeDelimiter line)

// Game 1: 1 red, 1 green, 1 blue; 2 red, 1 green, 1 blue; 1 red, 2 green, 1 blue; 1 red, 1 green, 2 blue
let tryGetCubePowerFromText (gameDelimiter: char) (handDelimiter: char) (cubeDelimiter: char) =
    function
    | SplitBy [| gameDelimiter |] (gameKey :: gameHandfulls) when gameHandfulls.Length = 1 ->
        let maxGameResult =
            getHandfullsFromText handDelimiter cubeDelimiter gameHandfulls.Head
            |> getMaxCubeQty

        Some(gameKey, maxGameResult |> GameResult.getCubePower)
    | _ -> None

// Game 1: 1 red, 1 green, 1 blue; 2 red, 1 green, 1 blue; 1 red, 2 green, 1 blue; 1 red, 1 green, 2 blue
// Game 2: 1 red, 1 green, 1 blue; 2 red, 1 green, 1 blue; 1 red, 2 green, 1 blue; 1 red, 1 green, 2 blue
let getCubePowerForGames (games: string[]) =
    Array.choose (tryGetCubePowerFromText ':' ';' ',') games

File.ReadAllText("data.txt").Split("\n")
|> getCubePowerForGames
|> Array.fold (fun acc (_game, power) -> power + acc) 0
|> printfn "%i"
