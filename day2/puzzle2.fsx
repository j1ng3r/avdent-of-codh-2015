open System;

let inline (|>>) (fn1, fn2) func arg = func (fn1 arg) (fn2 arg)

let getPerimeterRibbonFromLengths = (Array.sum, Array.max) |>> (-) >> ( * ) 2
let findRibbonFromLengths = (getPerimeterRibbonFromLengths, Array.reduce ( * )) |>> (+)

let parseLineIntoLengths (str:string) = (str.Split "x") |> Array.map int
let getTotalRibbon = Array.map (parseLineIntoLengths >> findRibbonFromLengths) >> Array.sum

let bytes = IO.File.ReadAllLines "data.txt"
printfn "%d" (getTotalRibbon bytes)