// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open GenCodeLib.Say

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    hello message
    0 // return an integer exit code