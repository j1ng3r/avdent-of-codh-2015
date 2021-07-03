module Puzzles
open System
open System.Collections

let always v _ = v

let numStations = 8
let ptrlen = numStations - 1
let best = 871
let station = "Arbre"
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

let linksAreConsistent (links: Link seq) =
   let connections = new Generic.Dictionary<string, string Generic.List>()
   for link in links do
      let (n1, n2) = link.Nodes
      connections.[n1].Add n2
      connections.[n2].Add n1
   let stations = connections.[station]
   stations.ToArray() |> Array.forall (fun start ->
      let mutable s = start
      let mutable a = connections.[s].ToArray()
      let mutable res = true
      while res && a.Length > 1 do
         a <- connections.[s].ToArray()
         s <- a |> Array.find (fun s2 -> (start <> s2))
         if a.Length > 2 || s = start then
            res <- false
      res
   )
let ptrsAreConsistent (links: Link List) (ptrs: int seq): bool =
   ptrs |> Seq.map (fun ptr -> links.[ptr]) |> linksAreConsistent

let maximize (ptrs: int Generic.List) =
   let mutable value = ptrs.[ptrs.Count - 1]
   let copiedPtrs = new Generic.List<int>()
   copiedPtrs.AddRange ptrs
   while Seq.length copiedPtrs < ptrlen do
      value <- value + 1
      copiedPtrs.Add value
   copiedPtrs

let links = lines |> Array.map Link.ofLine
let mutable slinks = links |> Array.filter (fun link -> link.Weight > 65) |> Array.sortByDescending (fun a -> a.Weight) |> List.ofArray
let mutable ptrs = Generic.List<int>()
ptrs.Add(0)

let minimize (ptrs: int Generic.List) =
   let mutable i = ptrlen - ptrs.Count
   let copiedPtrs = new Generic.List<int>()
   copiedPtrs.AddRange ptrs
   while i > 0 do
      copiedPtrs.Add (slinks.Length - i)
      i <- i - 1
   copiedPtrs

let areValid (ptrs: int Generic.List): bool = 
      (ptrs.[ptrs.Count - 1] < slinks.Length + ptrs.Count - ptrlen)
   && ((minimize ptrs).ToArray() |> Array.sumBy (fun ptr -> slinks.[ptr].Weight) > best)

let printlist (ptrs: int Generic.List) =
   for ptr in ptrs do
      printf "%d\n" ptr
   printf "%c" '\n'


while true do
   while ptrs |> ptrsAreConsistent slinks do
      if ptrs.Count < ptrlen then
         ptrs.Add (ptrs.[ptrs.Count - 1] + 1)
      else
         printlist ptrs
         failwith "Done"
   if not (areValid ptrs) then
      ptrs.RemoveAt (ptrs.Count - 2)
   ptrs.[ptrs.Count - 1] <- ptrs.[ptrs.Count - 1] + 1
