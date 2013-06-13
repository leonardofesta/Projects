module GestIT.History
open GestIT
open System.Collections.Generic
open System

type HEvent = 
    abstract member getTimestamp : unit -> System.DateTime

type HID = int

type FilteredHistory<'U> when 'U :> EventArgs and 'U :> HEvent (myid:HID, hs:float, pf: ('U->bool))   =
    let mutable id = myid
    let mutable recording = false
    let historysize = hs
    let predicatefilter =  pf
    let eventlist = new List<'U>() 
 
    member x.ID
        with get() = id
        and set y = id <- y
    
    member x.Recording 
        with get() = recording
        and set y = recording <- y
 
    member x.AddItem (arg:'U) =
        eventlist.Add(arg)

    member x.Filter (arg:'U) =
        if (predicatefilter(arg)) then
                x.AddItem(arg)

    member x.CleanCache() =
        eventlist.RemoveAll( new System.Predicate<_>( (fun y -> y.getTimestamp() <= System.DateTime.Now.AddMilliseconds(-1.0*historysize)))) |>ignore

    // ho pensato meglio ritornare la lista su cui fare le query che mettere dei metodi per fare le query dirette sulla lista
    member x.GetItems() =
        eventlist


type HistoryContainer<'U> when 'U :> HEvent and 'U :> EventArgs ()  = 
    
    let mutable lastsessionid = 0
    let mutable defaultsize = 10000.0 // in milliseconds
    let filters = new Dictionary<HID,FilteredHistory<'U>>()

    member private this.NextSessionID : int =
        lastsessionid <- ( lastsessionid + 1 )
        lastsessionid   

    member this.AddFilter(filter:'U->bool, ?hsize:float):HID =
        
        let hs =
            match hsize with
                | None -> defaultsize
                | Some h -> h
             
        let newid = this.NextSessionID:HID

        let newfilter = new FilteredHistory<'U>(newid,hs,filter)

        filters.Add(newid,newfilter)

        newid

    member this.RemoveFilter(id:HID) = 
        filters.Remove(id)

    member this.AddEvt(event:'U):unit =
          filters.Values |> Seq.iter ( fun x -> 
                                                if (x.Recording) then x.Filter(event)
                                                x.CleanCache()
                                     )
          this.stamparoba()    

    member this.GetItems(id:HID) = 
        filters.Item(id).GetItems()


    member this.SetRecording(id:HID,b:bool) =
        filters.Item(id).Recording <- b

    member private x.stamparoba() =
  
        let printitem(item:List<'U>) =
            if(item.Count>0)
                then
                System.Diagnostics.Debug.WriteLine(" Primo --> %s " + item.Item(0).getTimestamp().ToString())
                System.Diagnostics.Debug.WriteLine(" Ultimo --> %s " + item.Item(item.Count-1).getTimestamp().ToString())
                System.Diagnostics.Debug.WriteLine(" Quantità --> %s " + item.Count.ToString())

        System.Diagnostics.Debug.WriteLine("inizio stampa")
        filters.Values |> Seq.iter (fun y -> printitem(y.GetItems()))

    //TODO : metodo per fare le prove, da togliere successivamente
    member this.RecordAll(b:bool) = 
            filters.Keys |> Seq.iter (fun x -> this.SetRecording(x,b))