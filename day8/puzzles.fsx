module Puzzle1
open System

type Mode = Count | Backslash | UnicodeSearch

module Char =
   let IsHex (c: char): bool = "0123456789abcdef" |> Seq.contains c

let countRemovedChars (line: string): int =
   let n = line |> String.length
   let mutable count = 0
   let mutable i = 1
   while i < n - 1 do
      count <- count + 1
      i <- i +
         match line.[i] with
         | '\\' ->
            match line.[i+1] with
            | '\\' | '"' -> 2
            | 'x' -> 4
            | _   -> 0 // This causes an infinite loop. Don't tell anyone. :shh:
         | _ -> 1
   n - count

let countAddedChars (line: string): int =
   let n = line |> String.length
   let mutable count = 2
   for i = 0 to n - 1 do
      count <- count +
         match line.[i] with
         | '\\' | '"' -> 2
         | _ -> 1
   count - n

let lines = IO.File.ReadAllLines "input.txt"
let printCount fn = printf "%d%c" (lines |> Array.sumBy fn) '\n'

printCount countRemovedChars
printCount countAddedChars
