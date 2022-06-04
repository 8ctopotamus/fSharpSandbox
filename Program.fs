module RPS =
  let choices = ["r"; "p"; "s"]
  let mutable roundNum = 1
  let mutable keepPlaying = true
  let mutable wins = 0
  let mutable losses = 0
  let mutable ties = 0

  let getComputerChoice () : string =
    let rand = new System.Random()
    choices.[rand.Next(choices.Length)]

  let isValidChoice (str : string) : bool =
    match str with
    | "r" | "p" | "s" -> true
    | _ -> false

  let getUserChoice () : string =
    printf("Choose r, p, s (q to quit): ")
    System.Console.ReadLine()
    
  let compareChoices (userChoice: string, computerChoice: string) : string =
    if (System.String.Compare(userChoice, "r") = 0 && System.String.Compare(computerChoice, "s") = 0) || (System.String.Compare(userChoice, "p") = 0 && System.String.Compare(computerChoice, "r") = 0) || (System.String.Compare(userChoice, "s") = 0 && System.String.Compare(computerChoice, "p") = 0) then
      "win"
    elif System.String.Compare(userChoice, computerChoice) = 0 then 
      "tie"
    else
      "lose"

  let updateScore (result: string) =
    if System.String.Compare(result, "win") > -1 then
      wins <- wins + 1
    elif System.String.Compare(result, "tie") > -1 then
      ties <- ties + 1
    else
      losses <- losses + 1

  let playAgainPrompt () : bool =
    printf("Want to play again? [y/n]\n")
    let answer = System.Console.ReadLine()
    match answer with
    | "n" -> false
    | _ -> true

  let checkGameOver () =
    let mutable msg = ""
    if wins > 2 then
      msg <- "win"
    elif ties > 2 then 
      msg <- "tie"
    elif losses > 2 then
      msg <- "lose"
    let gameOver = 
      match msg with
        | "" -> false
        | _ -> true
    if gameOver then
      printf "========================\n"
      printf "GAME OVER. You %s!\n" msg
      printf "========================\n"
      keepPlaying <- playAgainPrompt()

  let rpsRound () =
    printf "-----------------------------------\n"
    printf "ROUND %i\n" roundNum 
    let userChoice = getUserChoice()
    let computerChoice = getComputerChoice()
    if isValidChoice(userChoice) then 
      let result = compareChoices(userChoice, computerChoice)
      printf "You: %s | Computer: %s\n" userChoice computerChoice
      updateScore(result)
      printf "Score: Wins: %i | Tie: %i | Losses: %i\n" wins ties losses
      checkGameOver()
      roundNum <- roundNum + 1
    else
      printf "Bad input, try again.\n"

  // game loop
  while keepPlaying do
    rpsRound()

[<EntryPoint>]
let main args = 
  while RPS.keepPlaying do
    RPS.rpsRound()
  0