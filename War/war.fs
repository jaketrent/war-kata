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
    
let value card =
    (suitValue card) * 4 + (rankValue card)

let playRound (card1 : Card, card2 : Card) =
    if value card1 > value card2 then
      card1
    else
      card2
    
let rec playGame (hand1 : Card list, hand2 : Card list) =
    match (hand1, hand2) with
    | [], _ -> "player 2 wins"
    | _, [] -> "player 1 wins"
    | (head1 :: tail1), (head2 :: tail2) -> 
        match playRound(head1, head2) with
        | win when win = head1 -> playGame(tail1 @ [head1; head2], tail2)
        | _ -> playGame(tail1, tail2 @ [head1; head2])
