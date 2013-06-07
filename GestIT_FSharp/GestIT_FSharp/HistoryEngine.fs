module GestIT.HistoryEngine
open GestIT.History
open GestIT.ObserverFeature
open System.Collections.Generic


type HistoryEngine<'U> when 'U :> System.EventArgs and 'U :> EventGestIT (expression:GestureExpr<_,_>, sens:ISensor<_,_>, ?hc:HistoryContainer<_>) =

    let mutable gestITExpr = expression
    let mutable sensor = sens   
    let hh = match hc with
                      | None -> new HistoryContainer<'V>()
                      | Some t -> t

    let mutable features = new List<ObservableFeature<_>>()
 

    member x.AddFeature( feat) =
        features.Add(feat)

    member x.ClearFeatures() =
        features.Clear()

    member x.CheckFeatures() = 
        features.ForEach(fun t -> t.runCheck(hh) )

    member x.RemoveFeature(feat:ObservableFeature<_>) = 
        features.Remove(feat)

    member x.sendEvt(t:'V) = 
        hh.addevt(t) |> ignore

    member x.run() = 
        sensor.SensorEvents.Add(fun d -> 
                                        hh.addevt(d.Event:'U) 
                                        x.CheckFeatures()
                                        )

        gestITExpr.ToGestureNet(sensor) |> ignore

    member x.Subscribe(feat:ObservableFeature<_>, observer:System.IObserver<_>) =
            feat.Subscribe(fun observer -> ())


    member x.Unsubscribe(feat:ObservableFeature<_>, observer:System.IObserver<_>) =
            //feat.Subscribe(fun observer -> ()) 
            // TODO Capire come fare
            ignore

