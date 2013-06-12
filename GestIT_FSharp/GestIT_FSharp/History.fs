module GestIT.History
open GestIT
open System.Collections.Generic


type HEvent = 
    abstract member getTimestamp : unit -> System.DateTime

    
    

type HistoryContainer<'U> when 'U :> HEvent ( historysize:float )  = 
    
    let mutable timelength = historysize // in milliseconds
    let mutable recording = false
    let mutable eventlist = new List<'U>() 

    let stamparoba() =
        System.Diagnostics.Debug.WriteLine(" Primo --> %s " + eventlist.Item(0).getTimestamp().ToString())
        System.Diagnostics.Debug.WriteLine(" Ultimo --> %s " + eventlist.Item(eventlist.Count-1).getTimestamp().ToString())
        System.Diagnostics.Debug.WriteLine(" Quantità --> %s " + eventlist.Count.ToString())
 
    member this.Timelength   
        with get() = timelength
        and set v = timelength <- v 
 
    member this.Recording
        with get() = recording
        and set v = recording <- v
    
    member this.Eventlist
        with get() = eventlist

    member this.clearcache() =
        this.Eventlist.RemoveAll( new System.Predicate<_>( (fun x -> x.getTimestamp() <= System.DateTime.Now.AddMilliseconds(-1.0*timelength)))) |>ignore

    member this.addevt(event:'U):unit =
        if (recording)
            then this.Eventlist.Add(event)
                 this.clearcache()
//               stamparoba()


