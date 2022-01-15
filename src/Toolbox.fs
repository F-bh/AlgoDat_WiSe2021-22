module ListTools

  let removeFirst item list = 
    let rec helper toFind retValue found list = 
      match list with
      | [] -> if found then retValue else list
      | x::[] -> retValue
      | x::y::[] -> if x = toFind then [y] elif y = toFind then [x] else list
      | x::y::xs -> 
        if x = toFind then
          helper toFind (retValue@y::xs) true []
        elif y = toFind then
          helper toFind (retValue@x::xs) true []
        else
          helper toFind (retValue@[x;y]) false xs
        
    helper item [] false list
    
    //List.fold (fun  (retVal, found) value -> if value == item && not found then    ) ([], false) list 