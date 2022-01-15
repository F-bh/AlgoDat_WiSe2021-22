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