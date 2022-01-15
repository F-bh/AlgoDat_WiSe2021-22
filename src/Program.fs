open Sorting
open Toolbox.Debug

[<EntryPoint>]
let main argv =
    let rand = System.Random()
    let list = List.init 2000 (fun _ -> rand.Next 2000)
    //let list = [6;1;2;5;3;2;5;6;8;213;7;3;242;6;87;12;56467;23]
    
    let res = time (bubbleSort, list, "insertionsort")
    printfn "%A\n" res

    let res = time (insertionSort, list, "insertionsort")
    printfn "%A\n" res

    let res = time (selectionSort, list, "selectionsort")
    printfn "%A\n" res

    0 