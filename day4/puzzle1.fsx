open System.Security.Cryptography
open System.Text

let md5 = MD5.Create()
let hash (data:string) :string =
   (StringBuilder(), md5.ComputeHash (Encoding.ASCII.GetBytes data))
   ||> Array.fold (fun sb b -> sb.Append (b.ToString "x2"))
   |> string

let input = "ckczppom"
let hashedHas5 (hshd:string) = hshd.[0..4] = "00000"

let mutable num = 0
while (input + string num) |> hash |> hashedHas5 |> not do
   num <- num + 1
printfn "%d" num