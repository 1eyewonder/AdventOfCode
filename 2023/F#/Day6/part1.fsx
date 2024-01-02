#r "nuget: Fs.Units"

open Fs.Units
open System
open System.IO

let (|SplitBy|_|) (x: string) (text: string) =
    text.Split(x, StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> Some

let determineWinningMethods (availableTime, requiredDistance) =
    let startingSpeed = 0<mm / ms>
    let requiredDistance = LanguagePrimitives.Int32WithMeasure<mm> requiredDistance

    let calculateBoatSpeed (rampUpTime: int<ms>) = rampUpTime / 1<ms> * 1<mm / ms>
    let getTravelTime raceTime rampUpTime = raceTime - rampUpTime

    [ 0..availableTime ]
    |> List.choose (fun rampUpTime ->
        let rampUpTime = LanguagePrimitives.Int32WithMeasure<ms> rampUpTime
        let availableTime = LanguagePrimitives.Int32WithMeasure<ms> availableTime
        let boatSpeed = (calculateBoatSpeed rampUpTime) + startingSpeed
        let travelTime = getTravelTime availableTime rampUpTime
        let distance = boatSpeed * travelTime

        match distance >= requiredDistance with
        | true -> Some(rampUpTime, distance)
        | _ -> None)

let data = File.ReadAllLines("data.txt")

let times =
    match data[0] with
    | SplitBy ":" [ _; SplitBy " " times ] -> times
    | _ -> failwith "Invalid input"

let distances =
    match data[1] with
    | SplitBy ":" [ _; SplitBy " " distances ] -> distances
    | _ -> failwith "Invalid input"

let raceStats =
    match times.Length, distances.Length with
    | x, y when x = y ->
        List.zip times distances
        |> List.map (fun (time, distance) -> int time, int distance)
    | _ -> failwith "Invalid input"

raceStats
|> List.map determineWinningMethods
|> List.map List.length
|> List.fold (*) 1
|> printfn "%A"
