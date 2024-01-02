#r "nuget: Fs.Units"

open Fs.Units
open System
open System.IO

let (|SplitBy|_|) (x: string) (text: string) =
    text.Split(x, StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> Some

let determineWinningMethods availableTime requiredDistance =
    let startingSpeed = 0L<mm / ms>
    let requiredDistance = LanguagePrimitives.Int64WithMeasure<mm> requiredDistance

    let calculateBoatSpeed (rampUpTime: int64<ms>) = rampUpTime / 1L<ms> * 1L<mm / ms>
    let getTravelTime raceTime rampUpTime = raceTime - rampUpTime

    [| 0L .. availableTime |]
    |> Array.Parallel.choose (fun rampUpTime ->
        let rampUpTime = LanguagePrimitives.Int64WithMeasure<ms> rampUpTime
        let availableTime = LanguagePrimitives.Int64WithMeasure<ms> availableTime
        let boatSpeed = (calculateBoatSpeed rampUpTime) + startingSpeed
        let travelTime = getTravelTime availableTime rampUpTime
        let distance = boatSpeed * travelTime

        match distance >= requiredDistance with
        | true -> Some(rampUpTime, distance)
        | _ -> None)

let data = File.ReadAllLines("data.txt")

let time =
    match data[0] with
    | SplitBy ":" [ _; SplitBy " " times ] -> times |> String.concat String.Empty
    | _ -> failwith "Invalid input"

let distance =
    match data[1] with
    | SplitBy ":" [ _; SplitBy " " distances ] -> distances |> String.concat String.Empty
    | _ -> failwith "Invalid input"


(int64 time, int64 distance)
||> determineWinningMethods
|> Array.length
|> printfn "%A"
