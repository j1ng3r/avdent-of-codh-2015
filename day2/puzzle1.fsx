open System;

let inline (|>>) (fn1, fn2) func arg = func (fn1 arg) (fn2 arg)
let areasFromLengths = (Array.reduce ( * ) >> (/), id) |>> Array.map
let findSqFt = ((areasFromLengths >> Array.sum >> ( * ) 2), areasFromLengths >> Array.min) |>> (+)
let parseLineIntoLengths (str:string) = (str.Split "x") |> Array.map int
let getTotalSqFt = Array.map (parseLineIntoLengths >> findSqFt) >> Array.sum

let bytes = IO.File.ReadAllLines "data.txt"
printfn "%d" (getTotalSqFt bytes)