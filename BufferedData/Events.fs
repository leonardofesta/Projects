module BufferData.Events

open System.Collections.Generic
open BufferData.IData
open BufferData.Data
open BufferData.TData

exception DataBufferIDExists of string
exception DataBufferNotFound of string

/// <summary>
/// Tipo rappresentante l'evento, 
/// 'V rappresenta il buffer da controllare
/// </summary>
/// <param name="triggerfun">Funzione che verrà controllata nel triggering</param>
/// <param name="active"> Booleano che rappresenta se l'evento è attivo oppure no </param>
type TEvent<'X,'V> (triggerfun : 'V -> bool, ?active: bool, ?name:string)=
    inherit Event<'X>()

    let mutable activity = match active with 
                                    | None -> true
                                    | Some  h -> h

    let nome = match name with 
                        | None -> ""
                        | Some x -> x
    

    member this.CheckFun(value:'V):bool =
        triggerfun value
    
    member this.IsActive():bool = 
        activity

    member this.SetActive(v:bool) = 
        activity <- v


///<summary> 
///Classe contenente il buffer e a cui passare gli eventi su cui effettuare un controllo<br/>
///I tipi: <'T,'W><br/>
///'T uno dei sottotipi di BufferedData<br/>
///'W il tipo di Tdata che contiene il dato da controllare con i predicati 
///</summary>
///<param name="data">l'oggetto buffer su cui controllare gli eventi</param>
type EventBuffer<'T,'W,'U> when 'T :> BufferedData<'W> and 'W :> Data<'U> (data:'T)  =

    let eventlist = new List<TEvent<_,_>>()
    
    member this.addEvent(t:TEvent<_,_>) = eventlist.Add(t)

    member this.AddItem(d:'W,filter:'W -> bool) = 
        data.AddItem(d,filter)
        if filter d then this.checkevents()

    member this.AddItem(d:'W) =
        data.AddItem(d)  
        this.checkevents()

    member private this.checkevents() = 
        eventlist
            |>Seq.filter(fun x-> (x.IsActive() && x.CheckFun(data)))
            |>Seq.iter(fun x-> x.Trigger(data))
  

///<summary>
///Classe contenente più di un buffer e a cui passare gli eventi su cui effettuare un controllo<br/>
///I tipi: <'T,'W><br/>
///'T uno dei sottotipi di BufferedData<br/>
///'W il tipo di Tdata che contiene il dato da controllare con i predicati 
///</summary>
///<param name="resultargFun">Funzione che servirà a costruire il returntype, costruendolo dal dictionary dei buffers</param>
type EventsBuffer<'T,'W,'U> when 'T :> BufferedData<'W> and 'W :> Data<'U> (resultargFun:Dictionary<int,'T> -> 'Z when 'Z:>System.EventArgs)  =

    let eventlist = new List<TEvent<_,_>>()
    let datalist = new Dictionary<int,'T>()
    
    member this.AddDataBuffer(id:int, buff:'T) = 
                    if (datalist.ContainsKey(id)) then 
                                                       raise (DataBufferIDExists("ID Già esistente")) 
                                                  else 
                                                       datalist.Add(id,buff)
 
    member this.AddEvent(t:TEvent<_,_>) = eventlist.Add(t)

    member this.AddItem(id:int,d:'W,filter:'W -> bool) = 
        
        if not(datalist.ContainsKey(id)) then raise (DataBufferNotFound("il buffer con quell'id non esiste"))
       
        let databuffer = datalist.Item(id)
        databuffer.AddItem(d,filter)
        this.checkevents()

    member this.AddItem(id,int,d:'W) =
        if not(datalist.ContainsKey(id)) then raise (DataBufferNotFound("il buffer con quell'id non esiste"))
        
        let databuffer = datalist.Item(id)
        databuffer.AddItem(d)  
    
        this.checkevents()

    member private this.checkevents() = 
        eventlist
            |>Seq.filter(fun x-> (x.IsActive() && x.CheckFun(datalist)))
            |>Seq.iter(fun x-> x.Trigger(resultargFun datalist))
        