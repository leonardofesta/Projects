module GestIT.History
open GestIT
open System.Collections.Generic
open System

type HEvent = 
    abstract member getTimestamp : unit -> System.DateTime

type HID = int

type FilteredHistoryEx<'U> = HID * bool ref * float * (DateTime*'U->bool) * List<DateTime*'U>
type FilteredHistory<'U> (historysize:float, predicatefilter: (DateTime*'U->bool))   =

    let mutable recording = false
    let eventlist = new List< DateTime*'U >()
 
    member x.Recording 
        with get() = recording
        and set y = recording <- y
 
    member x.AddItem (time:DateTime,arg:'U) =
        eventlist.Add(time,arg)

    member x.Filter
        with get() = predicatefilter

    member x.CleanCache() =
        eventlist.RemoveAll( new System.Predicate<_>( (fun (t,_) -> t <= System.DateTime.Now.AddMilliseconds(-1.0*historysize)))) |>ignore

    // ho pensato meglio ritornare la lista su cui fare le query che mettere dei metodi per fare le query dirette sulla lista
    member x.GetItems() =
        eventlist


type HistoryContainer<'U> when 'U :> HEvent ()  =
    
    let lastsessionid = ref (LanguagePrimitives.GenericZero<HID>)
    let mutable defaultsize = 10000.0 // in milliseconds
    let filters = new Dictionary<HID,FilteredHistory<'U>>()

    member private this.NextSessionID : int =
            //lastsessionid := ( !lastsessionid + 1 ); ! lastsessionid   
            let newId = System.Threading.Interlocked.Increment(lastsessionid)
            if newId = 0 then raise (new System.Exception("Nooo! hai finito gli ID!")) else newId

    member this.AddFilter(filter:DateTime*'U->bool, ?hsize:float):HID =
        
        let hs =
            match hsize with
                | None -> defaultsize
                | Some h -> h
             

        let newfilter = new FilteredHistory<_>(hs,filter)

        let newid = this.NextSessionID:HID

        filters.Add(newid,newfilter)

        newid

    member this.RemoveFilter(id:HID) = 
        filters.Remove(id)

    member this.AddEvt(event:'U):unit =
        let e = event.getTimestamp(),event
        for x in filters.Values do
            if (x.Recording && x.Filter(e)) then
                x.AddItem(event.getTimestamp(),event)
            x.CleanCache()
        this.stamparoba()

    member this.GetItems(id:HID) = 
        filters.Item(id).GetItems() :> seq<_>


    member this.SetRecording(id:HID,b:bool) =
        filters.Item(id).Recording <- b

    member private x.stamparoba() =
  
        let printitem(item:List<DateTime*'U>) =
            if(item.Count>0)
                then
                System.Console.WriteLine(" Primo --> " + let t,_ = item.Item(0) in t.ToString())
                System.Console.WriteLine(" Ultimo --> " + let t,_ = item.Item(item.Count-1) in t.ToString())
                System.Console.WriteLine(" Quantità --> " + item.Count.ToString())

        System.Diagnostics.Debug.WriteLine("inizio stampa")
        filters.Values |> Seq.iter (fun y -> printitem(y.GetItems()))

    //TODO : metodo per fare le prove, da togliere successivamente
    member this.RecordAll(b:bool) = 
            filters.Keys |> Seq.iter (fun x -> this.SetRecording(x,b))