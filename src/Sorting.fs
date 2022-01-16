module Sorting
  let bubbleSort values =
    let rec bubble values =
      match values with
      | [] -> values
      | _::[] -> values
      | x::y::xs ->
        if x > y then
          y::bubble (x::xs)
        else
          x::bubble (y::xs)
      
    List.fold (fun values _ -> bubble values) values values

  let insertionSort values =
    let rec insertOrdered ordered value =
      match ordered with
      | [] -> [value]
      | x::[] -> if x < value then x::[value] else value::[x]
      | x::xs ->
        if x < value then
          x::insertOrdered xs value
        else
          value::x::xs

    match values with
      | [] -> values
      | _::[] -> values
      | values -> List.fold insertOrdered [] values

  let selectionSort values =
    let rec helper ordered  values =
      match values with
        | [] -> ordered
        | values ->
          let minimum = List.min values
          let withoutMin = Toolbox.ListTools.removeFirst minimum values
          helper (ordered@[minimum]) withoutMin
    
    helper [] values

  let rec quickSort values =
    match values with
      | [] -> []
      | _::[] -> values
      | x::xs -> 
        let (left, right) = List.fold (fun (left, right) value -> if value <= x then (left@[value], right) else (left, right@[value])) ([], []) xs
        (quickSort left)@x::(quickSort right)

  let mergeSort values =
    let rec merge (left, right) =
      match (left, right) with
      | ([], right) -> right
      | (left, []) -> left
      | (x::xs , y::ys) ->
        if x <= y then
          x::(merge(xs, y::ys))
        else
          y::(merge(x::xs, ys))
    
    match values with
    | [] -> []
    | _::[] -> values
    | x::xs ->
      let right = (fun sublist -> merge (([List.head sublist]), (List.tail sublist)))
      List.fold (fun acc sublist -> merge (acc, right sublist)) [] (List.chunkBySize 2 values)