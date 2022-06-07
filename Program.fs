module RPS =
  type Choice =
  | Rock
  | Paper
  | Scissors

  type RoundResult =
  | UserWins
  | UserLoses
  | Tie

  module Choice =
    open System.Linq
    let parse (input: string) : Result<Choice,string> =
      match input.ToUpper().First() with
      | 'R' -> Ok Rock
      | 'P' -> Ok Paper
      | 'S' -> Ok Scissors
      | _ -> Error input

    let compare (user: Choice) (computer: Choice) : RoundResult =
      match (user, computer) with
      | (Rock,Scissors)
      | (Paper,Rock)
      | (Scissors,Paper) -> UserWins
      | (u,c) when u = c -> Tie
      | _ -> UserLoses


  // let choices = ["r"; "p"; "s"]
  let mutable roundNum = 1
  let mutable keepPlaying = true
  // let mutable wins = 0
  // let mutable losses = 0
  // let mutable ties = 0

  [<AutoOpen>]
  module RPSRand =
    let private rand = new System.Random()
    let getComputerChoice () : Choice =
      match rand.Next 2 with
      | 0 -> Rock
      | 1 -> Paper
      | 2 -> Scissors
      | x -> failwithf "Critical Error: Random should only generate values from 0 to 2, we received %i" x

  // let isValidChoice (str : string) : bool =
  //   match str with
  //   | "r" | "p" | "s" -> true
  //   | _ -> false

  let getUserChoice () : string =
    printf("Choose r, p, s: ")
    System.Console.ReadLine()

  type Player = Player of lifeCount:int

  type Score = {
    User: Player
    Computer: Player }

  module Score =
    let init () : Score = {
      User = Player 3
      Computer = Player 3 }
    let update (score: Score) (result: RoundResult) : Score = 
      score

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
    let userChoice = () |> getUserChoice |> Choice.parse
    let computerChoice = getComputerChoice()
    match userChoice with
    | Ok userChoice' ->
      let result = Choice.compare userChoice' computerChoice
      printf "You: %A | Computer: %A\n" userChoice' computerChoice
      updateScore(result)
      printf "Score: Wins: %i | Tie: %i | Losses: %i\n" wins ties losses
      checkGameOver()
      roundNum <- roundNum + 1
    | Error badInput ->
      printfn "Bad input '%s', try again." badInput

[<EntryPoint>]
let main args = 
  while RPS.keepPlaying do
    RPS.rpsRound()
  0