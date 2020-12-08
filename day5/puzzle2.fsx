open System;

let inline (|>>) (fn1, fn2) func arg = func (fn1 arg) (fn2 arg)
let tupply fn (arg1, arg2) = (fn arg1, fn arg2)
let swapTA2AT (arr1:'a[], arr2:'b[]) = arr1 |> Array.indexed |> Array.map (fun (i, v) -> (v, arr2.[i]))
let strs2chrss = tupply Array.ofSeq >> swapTA2AT

let nilString = string (char 1)
let hasSandwichLetters (line:string) = (nilString + nilString + line, line + nilString + nilString) |> strs2chrss |> Array.exists ((<||) (=))
let getLetterPairs (line:string) = (line.[..(String.length line - 2)], line.[1..]) |> strs2chrss |> Array.map ((string, string) |>> (+))
let hasDuplicate (line:string) = ((line.IndexOf, line.LastIndexOf) |>> (<>))

let containsThreeVowels = String.filter (fun chr -> "aeiou" |> String.exists ((=) chr)) >> String.length >> (<=) 3

let isNice (line:string) :bool = (hasDoubleLetters line) && (containsThreeVowels line) && not (containsBadSeqs line)
let countNiceLines = Array.filter isNice >> Array.length

let lines = IO.File.ReadAllLines "data.txt"
printfn "%d" (countNiceLines lines)