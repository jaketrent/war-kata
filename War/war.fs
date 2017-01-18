module war

type Suit =
    | Spade
    | Club
    | Diamond
    | Heart

type Rank =
    | Value of int
    | Jack
    | Queen
    | King
    | Ace

type Card = Suit * Rank

let rankValue card =
    match snd card with
    | Ace -> 14
    | King -> 13
    | Queen -> 12
    | Jack -> 11
    | Value v -> v
    
let suitValue card =
    match fst card with
    | Heart -> 4
    | Diamond -> 3
    | Club -> 2
    | Spade -> 1
    | _ -> 0
    
let value card =
    (suitValue card) * 4 + (rankValue card)

let playRound (card1 : Card, card2 : Card) =
    if value card1 > value card2 then
      card1
    else
      card2
    
    
    //failwith "not implemented: winning card"

let playGame (hand1 : Card list, hand2 : Card list) =
    failwith "not implemented: game winner"
