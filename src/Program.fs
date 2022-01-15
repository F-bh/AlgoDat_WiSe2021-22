open System
open Sorting
open ListTools

[<EntryPoint>]
let main argv =
    let list = [6;1;2;5;3;2;5;6;8;213;7;3;242;6;87;12;56467;23]
    let bSorted = bubbleSort list
    let iSorted = insertionSort list
    printfn "bubblesort:\n %A\n" bSorted
    printfn "insertionsort:\n %A\n" iSorted

    0 