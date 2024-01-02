open System
open System.IO

let (|SplitBy|_|) (x: string) (text: string) =
    text.Split(x, StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> Some

type Card =
    | Ace = 1
    | King = 2
    | Queen = 3
    | Jack = 4
    | Ten = 5
    | Nine = 6
    | Eight = 7
    | Seven = 8
    | Six = 9
    | Five = 10 
    | Four = 11
    | Three = 12
    | Two = 13

module Card =

    let tryGetFromString (cardText: char) =
        match cardText with
        | 'A' -> Some Card.Ace
        | 'K' -> Some Card.King
        | 'Q' -> Some Card.Queen
        | 'J' -> Some Card.Jack
        | 'T' -> Some Card.Ten
        | '9' -> Some Card.Nine
        | '8' -> Some Card.Eight
        | '7' -> Some Card.Seven
        | '6' -> Some Card.Six
        | '5' -> Some Card.Five
        | '4' -> Some Card.Four
        | '3' -> Some Card.Three
        | '2' -> Some Card.Two
        | _ -> None

type Hand = private Hand of Card list

module Hand =

    let tryCreate (cards: Card list) =
        match cards |> List.length with
        | 5 -> Some(Hand cards)
        | _ -> None

    let value (Hand cards) = cards

[<RequireQualifiedAccess>]
type HandType =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

let (|FiveOfAKind|FourOfAKind|FullHouse|ThreeOfAKind|TwoPair|OnePair|HighCard|) (cards: Card list) =
    match cards |> List.groupBy id |> List.sortByDescending (snd >> List.length) with
    | cardGroups when cardGroups.Length = 1 -> FiveOfAKind
    | cardGroups when cardGroups.Length = 2 && snd cardGroups.Head |> List.length = 4 -> FourOfAKind
    | group1 :: group2 :: _ as cardGroups when
        cardGroups.Length = 2
        && snd group1 |> List.length = 3
        && snd group2 |> List.length = 2
        ->
        FullHouse
    | cardGroups when cardGroups.Length = 3 && snd cardGroups.Head |> List.length = 3 -> ThreeOfAKind
    | group1 :: group2 :: _ as cardGroups when
        cardGroups.Length = 3
        && snd group1 |> List.length = 2
        && snd group2 |> List.length = 2
        ->
        TwoPair
    | cardGroups when cardGroups.Length = 4 && cardGroups.Head |> snd |> List.length = 2 -> OnePair
    | _ -> HighCard

module HandType =

    let getFromHand (hand: Hand) =
        match Hand.value hand with
        | FiveOfAKind -> HandType.FiveOfAKind
        | FourOfAKind -> HandType.FourOfAKind
        | FullHouse -> HandType.FullHouse
        | ThreeOfAKind -> HandType.ThreeOfAKind
        | TwoPair -> HandType.TwoPair
        | OnePair -> HandType.OnePair
        | HighCard -> HandType.HighCard
    let sortValue = function
        | HandType.FiveOfAKind -> 1
        | HandType.FourOfAKind -> 2
        | HandType.FullHouse -> 3
        | HandType.ThreeOfAKind -> 4
        | HandType.TwoPair -> 5
        | HandType.OnePair -> 6
        | HandType.HighCard -> 7

let tryParseLine = function 
    | SplitBy " " [ cards; bid ] -> 
        let cards = cards |> Seq.choose Card.tryGetFromString |> Seq.toList
        let bid = bid |> int
        match Hand.tryCreate cards with
        | Some hand -> Some (hand, bid)
        | _ -> None
    | _ -> None

File.ReadAllLines("data.txt")
|> Array.take 10
|> Array.Parallel.choose tryParseLine
|> Array.groupBy (fst >> HandType.getFromHand)
|> Array.sortByDescending (fst >> HandType.sortValue)
|> Array.sortBy (fun x -> x)
|> printfn "%A"
