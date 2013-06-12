module GestIT.HistoryEngine
open GestIT.History
open GestIT.ObserverFeature
open System.Collections.Generic


type HistoryID = int

type HFilter = int

type RecInterface<'U> = 
    abstract member Record : bool -> unit
    abstract member AddFilter : HFilter*HistoryID -> bool 
    
    // abstract member StartSession :  -> int




type HistoryEngine<'Y,'X,'U> when 'U :> System.EventArgs and 'U :> HEvent and 'X :> System.Enum and 'Y :> GestureExpr<'X,'U> (expression:'Y, sens:ISensor<'X,_>, ?hc:HistoryContainer<_>) =

    let mutable gestITExpr = expression
    let mutable sensor = sens   
    let hh = match hc with
                      | None -> new HistoryContainer<'V>(5000.0)
                      | Some t -> t

    let mutable features = new List<ObservableFeature<_>>()
 
    member x.Record(value:bool) =
        hh.Recording <- value

    member x.AddFeature( feat) =
        features.Add(feat)


    member x.ClearFeatures() =
        features.Clear()

    member x.CheckFeatures() = 
        features.ForEach(fun t -> t.runCheck(hh) )

    member x.RemoveFeature(feat:ObservableFeature<_>) = 
        features.Remove(feat)

    member x.run() = 
        sensor.SensorEvents.Add(fun d -> 
                                        hh.addevt(d.Event:'U) 
                                        x.CheckFeatures()
                                        )

        gestITExpr.ToGestureNet(sensor) |> ignore

    member x.Subscribe(feat:ObservableFeature<_>, observer:System.IObserver<_>) =
            feat.Event.Add(fun observer -> ())


    member x.Unsubscribe(feat:ObservableFeature<_>, observer:System.IObserver<_>) =
            //feat.Subscribe(fun observer -> ()) 
            // TODO Capire come fare
            ignore

