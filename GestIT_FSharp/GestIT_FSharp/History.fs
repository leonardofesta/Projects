module GestIT.History
open GestIT
open System.Collections.Generic



type EventGestIT = 
    abstract member getTimestamp : unit -> System.DateTime


type HistoryContainer<'U> when 'U :> EventGestIT ()  = 
    
    let mutable framedist = 100.0 // millisec float
    let mutable TIMELENGTH = 10000.0 // in milliseconds
    let mutable recording = false
    let mutable eventlist = new List<'U>() 

    // fare un costruttore sensato magari che piazza framedist e timelength

    member this.Framedist 
        with get() = framedist
        and set v = framedist <- v
 
    member this.Timelength   
        with get() = TIMELENGTH
        and set v = TIMELENGTH <- v 
 
    member this.Recording
        with get() = recording
        and set v = recording <- v
    
    member this.Eventlist
        with get() = eventlist


    // cancellare le robe troppo vecchie da mettere magari di routine ogni tot nella registrazione..
    member this.clearcache<'U> () =
        this.Eventlist.RemoveAll( new System.Predicate<'U> (fun i -> i.getTimestamp() < System.DateTime.Now.AddMilliseconds(-1.0* this.Framedist)))
        |> ignore


    //volendo includere tipi di eventi stile il click di un mouse sarebbe il caso di splittare il riconoscimento in 2,
    //una roba riconosciuta a prescinedere... un'altra serie di eventi che invece tipo posizionali vanno registrati ogni x per non smattare
    //magari fare 2 sottotipi a partire da EventArgsGestIT che splittano la roba a livello di inserimento boh
    member this.addevt(event:'U):unit =
        this.Eventlist.Add(event)
        this.Eventlist.RemoveAll( new System.Predicate<_>( (fun x -> x.getTimestamp() <= event.getTimestamp().AddMilliseconds(-1.0*TIMELENGTH)))) |>ignore
(*
        System.Diagnostics.Debug.WriteLine(" Primo --> %s " + this.Eventlist.Item(0).getTimestamp().ToString())
        System.Diagnostics.Debug.WriteLine(" Ultimo --> %s " + this.Eventlist.Item(this.Eventlist.Count-1).getTimestamp().ToString())
        System.Diagnostics.Debug.WriteLine(" Quantità --> %s " + this.Eventlist.Count.ToString())
*)

//        if (this.Eventlist.Count = 0) 
//            then 
//                this.Eventlist.Add(event)
            //aggiungi l'evento se dista almeno framedist dall'ultimo ricevuto
//            elif (this.Eventlist.FindLast(new System.Predicate<_>( fun i -> true)).getTimestamp().AddMilliseconds(this.Framedist) > event.getTimestamp())    
//                then this.Eventlist.Add(event)
     


     //creare dei listener a cui aggiungersi ... magari generics che elaborano in qualche modo la robaccia sopra
    