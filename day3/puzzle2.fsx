open System;

let inline (|>>) (fn1, fn2) func arg = func (fn1 arg) (fn2 arg)

type Location = int * int
type SantaSet = Set<Location>

let b2i bool = if bool then 1 else 0

let dx (chr:char) = b2i (chr = '>') - b2i (chr = '<')
let dy (chr:char) = b2i (chr = '^') - b2i (chr = 'v')
let updateLoc ((x,y):Location) (chr:char) = (x + dx chr, y + dy chr)
let updateSet (loc:Location) (set:SantaSet) = set.Add loc
let folder origSet = Array.fold (fun (loc, set) chr -> (updateLoc loc chr, updateSet loc set)) ((0, 0), origSet)
let getUniqueHouseSet (barr:byte[]) (origSet:SantaSet) = barr |> Array.map char |> folder origSet ||> updateSet
let filterByParity parity (barr:byte[]) = barr |> Array.indexed |> Array.filter (fun (i, v) -> i % 2 = parity) |> Array.map snd
let getRoboUniqueHouses (barr:byte[]) = Set.empty |> getUniqueHouseSet (filterByParity 0 barr) |> getUniqueHouseSet (filterByParity 1 barr) |> Set.count

let input = IO.File.ReadAllBytes "data.txt"
printfn "%d" (getRoboUniqueHouses input)