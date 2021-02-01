let indexed2 = Array.map Array.indexed >> Array.indexed
let mapdex2 fn arr =
   arr |> indexed2 |> Array.map (fun (i, arr1) -> arr1 |> Array.map (fun (j, b) -> fn (i,j) b))
let parseString (str:string) :(bool -> bool) = 

let updateMap (map:bool[][]) (str:string) = map |> mapdex2 (fun (i, j, b) -> )

//Suffering
