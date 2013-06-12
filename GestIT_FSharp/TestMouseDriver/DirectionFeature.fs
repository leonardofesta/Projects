module GestIT.Features

open System
open GestIT.ObserverFeature
open GestIT.History
open GestIT.HistoryEngine
open MouseDriver
open System.Collections.Generic

type Direction = 
    |   Right = 0
    |   Left = 1
    |   Up = 2
    |   Down = 3

type DirectionFeature<'U > when 'U :> MouseDriver.MouseEventArgsGestIT (dir:Direction, dist:int, ms:float) = 
    inherit ObservableFeature<'U>(new Event<_>())

    let mutable direction = dir
    let mutable distance = dist
    let mutable millisec = ms

    override this.runCheck hh =
            let recentevent = hh.Eventlist.FindAll(new System.Predicate<_> (fun v -> v.getTimestamp() > DateTime.Now.AddMilliseconds(-1.0*millisec)))
            let recentevent = Seq.cast<MouseDriver.MouseEventArgsGestIT> recentevent
            let recentevent = Seq.toList recentevent
            let firstelement = recentevent.Item(0)
            let lastelement = Seq.last recentevent

            //qualche roba con i generics boh... da capire
            if (List.forall<MouseEventArgsGestIT>( fun x -> x.X >= firstelement.X) recentevent) && ((firstelement.X - lastelement.X ) >0) 
                       then
                            this.EventTrigger  
            ()

