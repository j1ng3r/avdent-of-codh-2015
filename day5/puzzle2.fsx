open System;

let inline (|>>) (fn1, fn2) func arg = func (fn1 arg) (fn2 arg)
let tupply fn (arg1, arg2) = (fn arg1, fn arg2)
let strs2chrss = tupply Array.ofSeq >> (<||) Array.zip


let nilString = string (char 1)
let hasSandwichLetters (line:string) = (nilString + nilString + line, line + nilString + nilString) |> strs2chrss |> Array.exists ((<||) (=))
let getChoppedTuple (line:string) = (line.[..(String.length line - 2)], line.[1..])
let getLetterPairs (line:string) =
   (line.[..(String.length line - 2)], line.[1..])
   |> strs2chrss
   |> Array.map (fun (ch1:char, ch2:char) -> (string ch1) + (string ch2))
let hasDuplicate (line:string) (search:string) = (line.IndexOf search) + 1 < line.LastIndexOf search
let hasDoubleLetters (line:string) = line |> getLetterPairs |> Array.exists (hasDuplicate line)

let containsThreeVowels = String.filter (fun chr -> "aeiou" |> String.exists ((=) chr)) >> String.length >> (<=) 3

let isNice (line:string) :bool = (hasDoubleLetters line) && (hasSandwichLetters line)
let countNiceLines = Array.filter isNice >> Array.length

let lines = IO.File.ReadAllLines "data.txt"
printfn "%d" (countNiceLines lines)
// printfn "%b" (hasDoubleLetters "zspdwdqcrmtmdtsp")
// printfn "%b" (hasSandwichLetters "zspdwdqcrmtmdtsp")