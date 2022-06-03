module PigLatin =
  let private isVowel c =
    match c with 
    | 'a' | 'e' | 'i' | 'o' | 'u'
    | 'A' | 'E' | 'I' | 'O' | 'U' -> true
    |_ -> false

  let toPigLatin (word: string) =
    if isVowel word[0] then
      word + "yay"
    else
      word[1..] + string(word[0]) + "ay"

[<EntryPoint>]
let main args = 
  for arg in args do
    let newArg = PigLatin.toPigLatin arg
    printfn "%s in Pig latin is %s" arg newArg
  0