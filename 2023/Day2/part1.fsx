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

let getGameResults (cubes: ColoredCube[]) =
    cubes
    |> Array.fold
        (fun results cube ->
            match cube with
            | Red num -> { results with Red = results.Red + num }
            | Blue num ->
                { results with
                    Blue = results.Blue + num }
            | Green num ->
                { results with
                    Green = results.Green + num })
        GameResult.empty

// 1 red, 1 green, 1 blue
let getCubesFromHandfullText (cubeDelimiter: char) (text: string) =
    text.Split(cubeDelimiter) |> Array.choose ColoredCube.tryGetFromText

let getHandfullsFromText (handDelimiter: char) (cubeDelimiter: char) (text: string) =
    text.Split(handDelimiter)
    |> Array.map (fun line -> getCubesFromHandfullText cubeDelimiter line |> getGameResults)

// Game 1: 1 red, 1 green, 1 blue; 2 red, 1 green, 1 blue; 1 red, 2 green, 1 blue; 1 red, 1 green, 2 blue
let tryGetGameResultsFromText
    (gameDelimiter: char)
    (handDelimiter: char)
    (cubeDelimiter: char)
    (gameResultFilter: GameResult)
    =
    function
    | SplitBy [| gameDelimiter |] (gameKey :: gameHandfulls) when gameHandfulls.Length = 1 ->
        let filter gameResult = 
            gameResult.Red <= gameResultFilter.Red
            && gameResult.Green <= gameResultFilter.Green
            && gameResult.Blue <= gameResultFilter.Blue

        let cubes =
            getHandfullsFromText handDelimiter cubeDelimiter gameHandfulls.Head

        match cubes |> Array.forall filter with
        | true -> Some (gameKey, cubes)
        | false -> None
    | _ -> None

// Game 1: 1 red, 1 green, 1 blue; 2 red, 1 green, 1 blue; 1 red, 2 green, 1 blue; 1 red, 1 green, 2 blue
// Game 2: 1 red, 1 green, 1 blue; 2 red, 1 green, 1 blue; 1 red, 2 green, 1 blue; 1 red, 1 green, 2 blue
let getValidGames (gameResultFilter: GameResult) (games: string[]) =
    Array.choose (tryGetGameResultsFromText ':' ';' ',' gameResultFilter) games

File.ReadAllText("data.txt").Split("\n")
|> getValidGames { Red = 12; Green = 13; Blue = 14 }
|> Array.choose (fun (game, _) ->
    match game with
    | ParseRegex @"Game\s*([\d]*)" [ number ] -> 
        printfn "Game: %s" number
        Some(int number)
    | _ -> None)
|> Array.fold (+) 0
|> printfn "%i"
