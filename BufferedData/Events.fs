module GestIT.Events

open System.Collections.Generic
open GestIT.IData
open GestIT.Data
open GestIT.TData

// The f function will raise the T event if needed
type TEvent<'X,'V> (triggerfun : 'V -> bool, ?active: bool, ?name:string)=
    inherit Event<'X>()

    // da vedere se mettere a false
    let mutable activity = match active with 
                                    | None -> true
                                    | Some  h -> h

    let nome = match name with 
                        | None -> ""
                        | Some x -> x
    
    //let mutable counter = 0
    // restituisce la funzione che attiva il trigger 
    member this.CheckFun(value:'V):bool=   //( l:List<'T> when 'T :> BufferedData) =
    (*
        System.Diagnostics.Debug.WriteLine(nome + counter.ToString())
        if (triggerfun value) then 
            counter <- counter + 1
            System.Diagnostics.Debug.WriteLine("buffer ->" )
            System.Diagnostics.Debug.WriteLine ( (triggerfun value).ToString())
     *)  
        triggerfun value
    
    member this.IsActive():bool = 
        activity

    member this.SetActive(v:bool) = 
        activity <- v


///<summary>  // da vedere come inserire i commenti in modo giusto
///Classe contenente il buffer e a cui passare gli eventi su cui effettuare un controllo<br/>
///I tipi: <'T,'W><br/>
///'T uno dei sottotipi di BufferedData<br/>
///'W il tipo di Tdata che contiene il dato da controllare con i predicati 
///</summary>
///<param name="data">l'oggetto buffer su cui controllare gli eventi</param>
type EventBuffer<'T,'W> when 'T :> BufferedData<'W> and 'W :> TData (data:'T)  = //   when 'Y:>BufferedData) =
 
//:Buffered1D) = // when 'T:>BufferedData = ///(data:'T) = //   when 'Y:>BufferedData) =
    
    let eventlist = new List<TEvent<_,_>>()
    
    member this.addEvent(t:TEvent<_,_>) = eventlist.Add(t)

    member this.AddItem(d:'W) =
        data.AddItem(d)  
    
        eventlist
            |>Seq.filter( fun x -> (x.IsActive() && x.CheckFun(data)))
            |>Seq.iter(fun x ->x.Trigger(data))
           