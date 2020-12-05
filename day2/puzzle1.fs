open System;

let bytes = IO.File.ReadAllLines "data.txt"

let inline flip f a b = f b a

let fork fns arg = fns |> ((arg |> (|>)) |> Array.map)
let forkInv arg = Array.map ((|>) arg)


let prod = Array.reduce ( * )
let prod2 = prod >> ( * ) 2
let areasFromLengths (args:int[]) :int[] = args |> Array.map ((/) (prod args))
let minAreaFromLengths = areasFromLengths >> Array.min
let findSqFt lengths =
   2 * Array.sum (areasFromLengths lengths) + minAreaFromLengths lengths