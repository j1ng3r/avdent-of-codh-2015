module Puzzle1
open System
open System.Collections.Generic

let always v _ = v
let (|>>) fn1 fn2 v =
   let (a, b) = fn1 v
   fn2 a b

module Map =
   let getFirst map = map |> Map.pick (fun k v -> Some(k, v))

type Name = String
type Value = Option<int>

module Value =
   let get v =
      match v with
      | Some(w) -> w
      | None -> -1

type Data<'a> =
   | One of 'a
   | Two of ('a * 'a)

module Name =
   let isValue = Seq.forall Char.IsDigit
   let toReq (s: Name): Name array =
      if isValue s then
         Array.empty
      else
         [| s |]

module Data =
   let map (fn: 'a -> 'b) (data: Data<'a>): Data<'b> =
      match data with
      | One v -> One (fn v)
      | Two (u, v) -> Two (fn u, fn v)
   let toOne (data: Data<'a>) =
      match data with
      | One v -> v
      | Two (u, v) -> u
   let toTwo (data: Data<'a>) =
      match data with
      | One v -> (v, v)
      | Two (u, v) -> (u, v)
   

type Ops =
   | ID
   | NOT
   | AND
   | OR
   | LSHIFT
   | RSHIFT

module Ops =
   let resolve (op: Ops): Data<int> -> int =
      match op with
      | ID     -> Data.toOne
      | NOT    -> Data.toOne  >> (~~~)
      | AND    -> Data.toTwo |>> (&&&)
      | OR     -> Data.toTwo |>> (|||)
      | LSHIFT -> Data.toTwo |>> (<<<)
      | RSHIFT -> Data.toTwo |>> (>>>)

type Wire = {
   Op  : Ops
   mutable Data: Data<Name>
   mutable Req : Name array
}

module Wire =
   let make name data op = 
      let req =
         match data with
         | One v -> Name.toReq v
         | Two (u, v) -> Array.append (Name.toReq u) (Name.toReq v)
      (name, {Data = data; Op = op; Req = req})
   let ofLine (str: string): (Name * Wire) =
      match str.Split " " with
      | [|i; "->"; o|] -> make o (One i) ID
      | [|"NOT"; i; "->"; o|] -> make o (One i) NOT
      | [|f; opstr; s; "->"; o|] ->
         let op =
            match opstr with
            | "AND"    -> AND
            | "OR"     -> OR
            | "LSHIFT" -> LSHIFT
            | "RSHIFT" -> RSHIFT
            | _ -> failwith ""
         make o (Two (f, s)) op
      | _ -> failwith ""
   let resolve (wire: Wire): int = wire.Data |> Data.map int |> Ops.resolve wire.Op

type BiWire = {
   Wire: Wire
   Feeds : Name List
   mutable Value : Value
}

module BiWire =
   let ofWire (wire: Wire): BiWire = { Wire = wire; Feeds = List<Name>(); Value = None}
   let ofWires (wires: Map<Name, Wire>): Map<Name, BiWire> =
      let mutable biwires = wires |> Map.map (always ofWire)
      for kvp in biwires do
         let name = kvp.Key
         let biwire = kvp.Value
         for req in biwire.Wire.Req do
            biwires.[req].Feeds.Add(name)
      biwires
   let canResolve (biwire: BiWire) = biwire.Wire.Req |> Array.isEmpty

let reqsSatisfied (biwires: Map<Name, BiWire>) req = req |> Array.forall (fun name -> Option.isSome biwires.[name].Value)

let readBiWires filename =
   let lines = IO.File.ReadAllLines filename
   let wires = lines |> Array.map Wire.ofLine |> Map.ofArray
   wires |> BiWire.ofWires

let resolveBiWires biwires =
   let mutable biwireQ = biwires |> Map.filter (always BiWire.canResolve)

   while not (Map.isEmpty biwireQ) do
      let (name, biwire) = biwireQ |> Map.getFirst
      let value = Wire.resolve biwire.Wire
      biwire.Value <- Some(value)
      let strValue = string value

      for feedName in biwire.Feeds do
         let feed = biwires.[feedName]
         feed.Wire.Data <- feed.Wire.Data |> Data.map (fun feedreq -> if feedreq = name then strValue else feedreq)
         feed.Wire.Req <- feed.Wire.Req |> Array.except [|name|]
         if BiWire.canResolve feed then
            biwireQ <- biwireQ.Add (feedName, feed)
      biwireQ <- biwireQ.Remove name
   biwires

let getA (biwires: Map<Name, BiWire>) = Value.get biwires.["a"].Value

let avalue = "input.txt" |> readBiWires |> resolveBiWires |> getA
printfn "%d" avalue

let nextBiWires = "input.txt" |> readBiWires
nextBiWires.["b"].Wire.Data <- One (string avalue)
let nextAvalue = nextBiWires |> resolveBiWires |> getA
printfn "%d" nextAvalue
