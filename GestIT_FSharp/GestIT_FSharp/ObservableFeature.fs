module GestIT.ObserverFeature

open System 
open GestIT.History

/// A Observable base class which notifies
/// all observers in parallel 

[<AbstractClass>]
type ObservableFeature<'U>() =
  let mutable observers = []
 
  /// Notifies all observers in parallel about the new value
  let notifyObservers f =
    observers
      |> Seq.map (fun (observer:IObserver<'U>) ->
                         async { return f observer})
      |> Async.Parallel
      |> Async.RunSynchronously
      |> ignore
 
  interface IObservable<'U> with
    member observable.Subscribe(observer)  =
      // subscribe observer
      observers <- observer :: observers
      // create Disposable to unsubscribe observer later
      {new IDisposable with
         member this.Dispose() =
            observers <-
               observers |> List.filter ((<>) observer)}
 
  /// Notifies all observers in parallel about the new value
  member observable.OnNext value =
   notifyObservers (fun observer -> observer.OnNext value)
 
  /// Notifies all observers in parallel about the error
  /// and finishes all observations
  member observable.OnError error =
    notifyObservers (fun observer -> observer.OnError error)
    observers <- []
 
  /// Notifies all observers in parallel about the completion
  /// and finishes all observations
  member observable.Completed =
    notifyObservers (fun observer -> observer.OnCompleted())
    observers <- [] 

  abstract member runCheck : HistoryContainer<_> -> unit

//abstract member triggerEvt : () -> ()
//vedere come e dove fare partire i trigger degli eventi