// Learn more about F# at http://fsharp.org

open System
open Microsoft.VisualBasic.CompilerServices
open Microsoft.VisualBasic.CompilerServices

//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode s =
  Seq.toList s
 
//
// implode
//
// The opposite of explode ---given a list of characters, returns
// the list as a string.  Example: implode ['t';'h';'e'] => "the"
//
let implode L =
  let sb = System.Text.StringBuilder()
  L |> List.iter (fun c -> ignore (sb.Append (c:char)))
  sb.ToString()
 
// YOUR FUNCTIONS HERE.

//getting the length
let rec length L =
  match L with
  | [ ] -> 0
  | head::tail -> 1 + length tail

//getting the number of spaces in input
let rec space L =
  match L with
  | [ ] -> 0
  | head::tail when head = ' ' ->  1 + space tail
  | head::tail -> 0 + space tail

//getting the number of commas in input
let rec commas L =
  match L with
  | [ ] -> 0
  | head::tail when head = ',' ->  1 + commas tail
  | head::tail -> 0 + commas tail

//getting the number of periods in input
let rec periods L =
  match L with
  | [ ] -> 0
  | head::tail when head = '.' ->  1 + periods tail
  | head::tail -> 0 + periods tail

//helper function for acronym
let rec helper L =
  match L with
  | [ ] -> [ ]
  | head::temp::tail when head = ' ' -> temp::helper tail
  | _::tail -> helper tail

//getting the first character of the word
let rec acronym M =
  match M with
  | [ ] -> [ ]
  | _ -> List.head M::helper M

[<EntryPoint>]
let main argv =
    printf "input> "
    let input = System.Console.ReadLine()

    let strExp = explode input

    printfn "Length of total string = %A " (length strExp)
    printfn "Spaces = %A " (space strExp)
    printfn "Commas = %A " (commas strExp)
    printfn "Periods = %A " (periods strExp)
    let strImp = acronym strExp
    let acro = implode strImp
    printfn "Acronym = %A " (acro)

    0
