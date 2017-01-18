module war_tests

open NUnit.Framework
open Swensen.Unquote.Assertions
open war

[<Test>]
let ``the highest rank wins the cards in the round`` () = 
    let card1 = (Spade, Value 2)
    let card2 = (Spade, Value 3)
    let winner = playRound(card1, card2)
    test <@ winner = card2 @>
 
[<Test>]
let ``queens are higher rank than jack`` () = 
    let card1 = (Diamond, Queen)
    let card2 = (Diamond, Jack)
    let winner = playRound(card1, card2)
    test <@ winner = card1 @>

[<Test>]
let ``kings are higher rank than queens`` () = 
    let card1 = (Diamond, Queen)
    let card2 = (Diamond, King)
    let winner = playRound(card1, card2)
    test <@ winner = card2 @>

[<Test>]
let ``aces are higher rank than kings`` () = 
    let card1 = (Diamond, Ace)
    let card2 = (Diamond, King)
    let winner = playRound(card1, card2)
    test <@ winner = card1 @>


[<Test>]
let ``if the ranks are equal, clubs beat spades`` () = 
    let card1 = (Club, Value 2)
    let card2 = (Spade, Value 2)
    let winner = playRound(card1, card2)
    test <@ winner = card1 @>

[<Test>]
let ``if the ranks are equal, diamonds beat clubs`` () = 
    let card1 = (Club, Value 2)
    let card2 = (Diamond, Value 2)
    let winner = playRound(card1, card2)
    test <@ winner = card2 @>

[<Test>]
let ``if the ranks are equal, hearts beat diamonds`` () = 
    let card1 = (Heart, Value 2)
    let card2 = (Diamond, Value 2)
    let winner = playRound(card1, card2)
    test <@ winner = card1 @>

[<Test>]
let ``the player loses when they run out of cards`` () = 
    Assert.Fail "TODO"
