module Puzzles
open System

let always v _ = v
let indexInto (list) (ptr: int) = list.[ptr]

let numStations = 8
let ptrlen = numStations - 1
let best = 871
let lines = IO.File.ReadAllLines "input.txt"

type Link = {
   Nodes : string * string
   Weight : int
}

module Link =
   let ofLine (line: string): Link =
      match line.Split " " with
      | [|n1; "to"; n2; "="; w|] -> {Nodes = (n1, n2); Weight = int w}
      | _                        -> {Nodes = ("", ""); Weight = 0}

let linksAreConsistent (links: Link List) =
   let mutable counts: Map<string, int> = Map []
   for link in links do
      let (n1, n2) = link.Nodes
      for n in [|n1; n2|] do
         if not (counts.ContainsKey n) then
            counts <- counts.Add (n, 1)
         else
            counts <- counts.Add (n, counts.[n] + 1)
   let values = counts |> Map.toArray |> Array.map snd
   let noTriples = values |> Array.forall ((>=) 2)
   let hasEndpoint = values |> Array.any ((=) 1)
   noTriples && hasEndpoint
let ptrsAreConsistent (links: Link List) (ptrs: int List): bool =
   ptrs |> List.map (fun ptr -> links.[ptr]) |> linksAreConsistent

let maximize (ptrs: int List): int =
   let mutable copiedPtrs = ptrs
   let mutable value = copiedPtrs.[-1]
   while Seq.length copiedPtrs < ptrlen do
      value <- value + 1
      copiedPtrs <- List.append copiedPtrs value
   copiedPtrs

let minimize ptrs: int =
   let mutable copiedPtrs = List.copy ptrs
   let mutable i = ptrlen - ptrs.Length
   while i > 0 do
      copiedPtrs.Add slinks.Length - i
      i <- i - 1
   copiedPtrs

let filterDiscrepancy links ptrs: int =
   let discrepancy = getMaxDiscrepancy links ptrs
   links |> Array.filter (fun link -> link.Weight > discrepancy)

let links = lines |> Array.map Link.ofLine
let mutable slinks = links |> Array.filter (fun link -> link.Weight > 65) |> Array.sortByDescending (fun a -> a.Weight) |> List.ofArray

let areValid (ptrs: seq<int>): bool = 
      (ptrs[-1] < slinks.Length + ptrs.Length - ptrlen)
   && (ptrs |> minimize |> Array.sumBy (fun ptr -> slinks.[ptr].Weight) > best)

let mutable ptrs = List<int>(0)

let printlist (ptrs: seq<int>) =
   for ptr in ptrs do
      printf "%d" ptr


while true do
   while ptrs |> ptrsAreConsistent do
      if ptrs.Length < ptrlen then
         ptrs.Add (ptrs[-1] + 1)
      else
         printlist ptrs
   ptrs[-1] <- ptrs[-1] + 1
   while not (areValid ptrs) do
      ptrs <- ptrs.[..(-2)]
      ptrs[-1] <- ptrs.[-1] + 1
