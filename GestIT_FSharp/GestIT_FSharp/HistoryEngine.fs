module GestIT.HistoryEngine
open GestIT.History
open GestIT.ObserverFeature
open System.Collections.Generic


type HistoryEngine<'U> when 'U :> EventGestIT() =

    let hh = new HistoryContainer<'U>()
    let mutable features = new List<ObservableFeature<_>>()


    member x.AddFeature( feat) =
        features.Add(feat)

    member x.ClearFeatures() =
        features.Clear()

    member x.CheckFeatures() = 
        features.ForEach(fun t -> t.runCheck(hh) )

    member x.RemoveFeature(feat:ObservableFeature<_>) = 
        features.Remove(feat)

    member x.sendEvt(t:'U) = 
        hh.addevt(t)

//  member 
    
    member x.Subscribe(feat:ObservableFeature<_>, observer:System.IObserver<_>) =
            feat.Subscribe(fun observer -> ())


    member x.Unsubscribe(feat:ObservableFeature<_>, observer:System.IObserver<_>) =
            //feat.Subscribe(fun observer -> ()) 
            // TODO Capire come fare
            ignore

