module Puzzles
open System
open System.Collections

let ptrs = Generic.List<int>(0)
ptrs.Add 2
ptrs.Add 5
ptrs.Add 6

printf "%d" ptrs.[ptrs.Count - 1]
