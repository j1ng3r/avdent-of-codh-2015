open System

let lines = IO.File.ReadAllLines "input.txt"

type Op = TurnOn | TurnOff | Toggle
type Coord = (int * int)
module Coord =
   let fromString (s: string): Coord =
      let nums = s.Split ","
      (int nums.[0], int nums.[1])

type Instruction = {
   Op   : Op
   Start: (int * int)
   End  : (int * int)
}

module Instruction =
   // turn on 489,959 through 759,964
   let fromString (s: string): Instruction =
      let words = s.Split " "
      match words with
      | [|"turn"; "on"; Start; "through"; End|] -> {
         Op = TurnOn
         Start = Coord.fromString Start
         End = Coord.fromString End
      }
      | _ -> 

lines |> Array.map Instruction.fromString
