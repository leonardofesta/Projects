module GestIT.HistoryEngine
open GestIT.History
open GestIT.ObserverFeature
open System.Collections.Generic

(*
type HistoryID = int

type HFilter = int

type RecInterface<'U> = 
    abstract member Record : bool -> unit
    abstract member AddFilter : HFilter*HistoryID -> bool 
*)

type HistoryEngine<'Y,'X,'U> when 'U :> System.EventArgs and 'U :> HEvent and 'X :> System.Enum and 'Y :> GestureExpr<'X,'U> (expression:'Y, sens:ISensor<'X,_>, ?hc:HistoryContainer<_>) =

    let mutable gestITExpr = expression
    let mutable sensor = sens   
    let historycontainer =
        match hc with
        | None -> new HistoryContainer<'U>()
        | Some t -> t

    let mutable features = new List<ObservableFeature<_>>()

    member x.Run() = 
        sensor.SensorEvents.Add(fun d -> 
                                        historycontainer.AddEvt(d.Event:'U) 
                                        x.CheckFeatures()
                                        )
        historycontainer.RecordAll(true)
        gestITExpr.ToGestureNet(sensor) |> ignore

    member x.AddFilter<'U> (predicate: System.DateTime*'U -> bool,?hsize:float ) : HID = 
        match hsize with 
            | None -> historycontainer.AddFilter(predicate)
            | Some s -> historycontainer.AddFilter(predicate,s)

    member x.RemoveFilter(id:HID) = 
        historycontainer.RemoveFilter(id)

    member x.Record(id:HID, b:bool) =
        historycontainer.SetRecording(id,b)


//fuffa per ora
 
    member x.AddFeature( feat) =
        features.Add(feat)

    member x.ClearFeatures() =
        features.Clear()

    member x.CheckFeatures() = 
        features.ForEach(fun t -> t.runCheck(historycontainer) )

    member x.RemoveFeature(feat:ObservableFeature<_>) = 
        features.Remove(feat)

    member x.Subscribe(feat:ObservableFeature<_>, observer:System.IObserver<_>) =
            feat.Event.Add(fun observer -> ())


    member x.Unsubscribe(feat:ObservableFeature<_>, observer:System.IObserver<_>) =
            //feat.Subscribe(fun observer -> ()) 
            // TODO Capire come fare
            ignore

// fine fuffa
