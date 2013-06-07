module GestIT.Features

open System
open GestIT.ObserverFeature
open GestIT.History
open GestIT.HistoryEngine


type Direction = 
    |   Right = 0
    |   Left = 1
    |   Up = 2
    |   Down = 3

type DirectionFeature<'U >(dir:Direction, dist:int, ms:float) = 
    inherit ObservableFeature<'U>()
    
    let mutable direction = dir
    let mutable distance = dist
    let mutable millisec = ms

    override x.runCheck hh =
            let recentevent = hh.Eventlist.FindAll(new System.Predicate<_> (fun x -> x.getTimestamp() > DateTime.Now.AddMilliseconds(-1.0*millisec)))
  //          let firstelement = recentevent.FindIndex(0) 
  //          let lastelement = recentevent.FindIndex(recentevent.Count)
  //          ignore
          //  if (recentevent.TrueForAll( new System.Predicate<_> (fun x. ->
            ()

