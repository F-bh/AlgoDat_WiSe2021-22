namespace Toolbox
  module Debug =
    open System.Diagnostics
    //time a function that does something and print it's name
    let time (work: ('a -> 'b), parames: 'c, name: string ): 'd =
      let timer = new Stopwatch()
      timer.Start()
      let res = work parames
      timer.Stop()
      printfn "%s took: %ims and resulted in:\n" name timer.ElapsedMilliseconds
      res

  module ListTools =
    let removeFirst item list = 
      let rec helper toFind retValue found list = 
        match list with
        | [] -> if found then retValue else list
        | x::[] -> retValue
        | x::y::[] ->
          if x = toFind then
            if retValue.IsEmpty then
              [y]
            else
              retValue@[y]
          elif y = toFind then
            if retValue.IsEmpty then
              [x]
            else
              retValue@[x]
          else
            list
        | x::y::xs -> 
          if x = toFind then
            helper toFind (retValue@y::xs) true []
          elif y = toFind then
            helper toFind (retValue@x::xs) true []
          else
            helper toFind (retValue@[x;y]) false xs
          
      helper item [] false list