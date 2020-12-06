open System;

let inline (|>>) (fn1, fn2) func arg = func (fn1 arg) (fn2 arg)
let tupply fn (arg1, arg2) = (fn arg1, fn arg2)
let swapTA2AT (arr1:'a[], arr2:'b[]) = arr1 |> Array.indexed |> Array.map (fun (i, v) -> (v, arr2.[i]))

let nilString = string (char 1)
let hasDoubleLetters (line:string) = (nilString + line, line + nilString) |> tupply Array.ofSeq |> swapTA2AT |> Array.exists ((<||) (=))
let containsThreeVowels = String.filter (fun chr -> "aeiou" |> String.exists ((=) chr)) >> String.length >> (<=) 3
let containsBadSeqs (line:string) = [|"ab"; "cd"; "pq"; "xy"|] |> Array.exists line.Contains

let isNice (line:string) :bool = (hasDoubleLetters line) && (containsThreeVowels line) && not (containsBadSeqs line)
let countNiceLines = Array.filter isNice >> Array.length

let lines = IO.File.ReadAllLines "data.txt"
printfn "%d" (countNiceLines lines)