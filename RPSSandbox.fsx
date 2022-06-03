// let choices: (string * string * string)[] = [|"r", "p", "s"|]
let choices = ["r"; "p"; "s"]
let mutable keepPlaying = true
let mutable wins = 0
let mutable losses = 0
let mutable ties = 0

let getRandomChoice () : string =
  let rand = new System.Random()
  choices.[rand.Next(choices.Length)]

let validateChoice (str : string) : bool =
  match str with
  | "r" | "p" | "s" -> true
  | _ -> false

let compareChoices (userChoice: string, computerChoice: string) : string =
  if System.String.Compare(userChoice, "r") > -1 && System.String.Compare(computerChoice, "s") > -1 || System.String.Compare(userChoice, "p") > -1 && System.String.Compare(computerChoice, "r") > -1 || System.String.Compare(userChoice, "s") > -1 && System.String.Compare(computerChoice, "p") > -1 then
    "win"
  elif System.String.Compare(userChoice, computerChoice) > -1 then 
    "tie"
  else
    "lose"

// game loop
while keepPlaying do
  printf("Choose 'r', 'p', or 's']: ")
  let userChoice = System.Console.ReadLine()
  let computerChoice = getRandomChoice()
  if validateChoice(userChoice) then 
    let result = compareChoices(userChoice, computerChoice)
    if System.String.Compare(result, "win") > -1 then
      wins <- wins + 1
    elif System.String.Compare(result, "tie") > -1 then
      ties <- ties + 1
    else
      losses <- losses + 1
    printfn "Score:\nWins: %i\nTie: %i\nLosses: %i" wins ties losses
    // keepPlaying <- true
  else
    keepPlaying <- false