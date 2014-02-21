module BufferData.TData

open System
open BufferData.IData
open BufferData.Utils
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Distributions
open MathNet.Numerics.IntegralTransforms
open MathNet.Numerics.IntegralTransforms.Algorithms


/// <summary>
/// Tipo di supporto per calcolare sui dati, serve a ottenere una rappresentazione temporale in float per i confronti
/// </summary>
type TimespanData(timespan:float,x:float,?y:float,?z:float) = 

           member this.D1:float = x

           member this.D2:float = match y with
                                        | Some t->t
                                        | None -> -1.0

           member this.D3:float = match z with
                                        | Some t->t
                                        | None -> -1.0

           member this.Time:float = timespan        



type Buffered1D<'T> (?item:List<TData1D<'T>>, ?soglia:float) =
    inherit BufferedData<TData1D<'T>>()

    let threshold = match soglia with
                            | None -> 10000.0
                            | Some h -> h
    
    let  itemlist = match item with
                            | None -> new List<TData1D<'T>>()
                            | Some h -> h
  
    member this.Count () = itemlist.Count    

    member this.Clear () = itemlist.Clear()

    member this.GetArrayBuffer() = Seq.toArray(itemlist)

    member this.GetListBuffer() = Seq.toList(itemlist)

    override this.AddItem(d:TData1D<'T>, filter:TData1D<'T>->bool) = 
        if (filter d) then itemlist.Add(d)
    
    override this.AddItem(d:TData1D<'T>) = 
        itemlist.Add (d)
#if Reading
        //Evitare di fare il remove all inserendo eventi con orari arbitrari sarebbero cancellati
#else
        itemlist.RemoveAll(fun x -> (x.Time < System.DateTime.Now.AddMilliseconds(-1.0*threshold)))
        |>ignore
#endif

    ///<summary>
    ///Restituisce un nuovo oggetto con il buffer tagliato a tot millisecondi
    ///</summary>
    ///<param name="millisec"> la porzione di tempo da tenere a partire dall'istante attuale, in millisecondi </param>
    ///<returns>un oggetto Buffereed1D</returns>    
    member this.cutBuffer(millisec:float):Buffered1D<'T> = 
            let newlist = listcut(itemlist,millisec)
            new Buffered1D<'T>(new List<TData1D<'T>> (newlist))

    ///<summary>
    ///Calcola la lunghezza del periodo reale di tempo campionato dal buffer 
    ///dal primo evento disponibile all'ultimo rispetto il tempo attuale
    ///</summary>
    ///<returns>un oggetto float rappresentante i millisecondi </returns>    
    member this.PeriodLength():float =
            if (itemlist.Count < 1) 
                then 
                    0.0
                else  
                    let primo = itemlist.[0]
                    (System.DateTime.Now.Subtract (primo.Time)).TotalMilliseconds

    ///<summary>
    ///Calcola se la cardinalità dei dati nel buffer è maggiore/uguale di una certa soglia
    ///</summary>
    ///<param name="n">intero che rappresenta la soglia della cardinalità da controlalre</param>
    ///<return>True se il numero di item nel buffer è almeno n, False altrimenti</return>   
    member this.Cardinality(n:int):Boolean = 
             n>=itemlist.Count
    


    ///<summary>
    ///Controlla che il buffer sta ricevendo dati per un periodo "timespan" con una certa continuità. 
    ///utile per evitare falsi possitivi negli altri predicati
    ///</summary>
    ///<param name="timespan">float che rappresenta la soglia della cardinalità da controlalre</param>
    ///<param name="interval">float rappresentante la distanza massima in millisecondi tra 2 eventi continui</param>
    ///<return>True se il numero di item nel buffer è almeno n, False altrimenti</return>   
    member this.IsContinuous(timespan:float, interval:float):Boolean =
            let mylist = listcut( itemlist,timespan)
            continuity(mylist,interval)
            
                    
    ///<summary>
    ///Calcola la distanza totale percorsa percorsa dato un intervallo di tempo, in millisecondi
    ///</summary>
    ///<param name="timespan">Float che rappresenta il periodo in cui calcolare la distanza percorsa</param>
    ///<returns>la distanza totale percorsa</returns>
    member this.TotalDistance(timespan:float) = 
            
            let mutable datacp = listcut(itemlist, timespan)
               
            if (Seq.length datacp >2)
                then
                    let mutable current = datacp.Head
                    let mutable distance = 0.0
                    datacp <- datacp.Tail
                    while(not(datacp.IsEmpty))
                        do
                        distance <- Math.Abs(current.D1 - datacp.Head.D1) + distance
                        current <- datacp.Head
                        datacp <- datacp.Tail

                    distance
                else
                    0.0
    
    ///<summary>
    ///Calcola la distanza totale percorsa percorsa dato un intervallo di tempo, in millisecondi
    ///</summary>
    ///<param name="timespan">Float che rappresenta il periodo in cui calcolare la distanza percorsa</param>
    ///<returns>la distanza totale percorsa per ogni componente</returns>
    member this.ComponentDistance(timespan:float):float = 
            
            let mutable datacp = listcut(itemlist, timespan)
                                                             
            if (Seq.length datacp >2)
                then
                    let mutable current = datacp.Head
                    let mutable distance = 0.0
                    datacp <- datacp.Tail
                    while(not(datacp.IsEmpty))
                        do
                        distance <- Math.Abs(current.D1 - datacp.Head.D1) + distance
                        current <- datacp.Head
                        datacp <- datacp.Tail

                    distance
                else
                    0.0
    


    ///<summary>
    ///Calcola la velocità istantanea degli ultimi eventi negli ultimi 100ms
    ///</summary>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.InstantVelocity() = 
           this.AverageVelocity(100.0)
    
    ///<summary>
    ///Calcola la velocità media percorsa dato un intervallo di tempo, in millisecondi
    ///</summary>
    ///<param name="timespan">Float che rappresenta il periodo in cui calcolare la velocità media</param>
    ///<returns>il valore di velocità media, oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.AverageVelocity(timespan:float) = 
            
            let mutable datacp = listcut(itemlist, timespan)
 
            if (Seq.length datacp >2)
                then
                    let startingtime = datacp.Head.Time
                    let lasttime = (datacp.Item(datacp.Length - 1)).Time
                    let distance = this.TotalDistance(timespan)

                    let velocity = (distance / (lasttime - startingtime).TotalMilliseconds)*1000.0
                    velocity   
                else
                    0.0



    
    ///<summary>
    ///Calcola la accelerazione ottenuta comparando la velocità ottenuta nella parte tra start e mid con quella da mid al momento attuale
    ///</summary>
    ///<param name="start">rappresenta il punto di inizio della area presa in considerazione come momento precedente, da considerare fino al mid </param>
    ///<param name="mid"> rappresenta il punto da cui in poi verrà preso in considerazione come momento attuale fino al momento attuale </param>
    ///<returns>float rappresentante la posizione media</returns>    
    member this.Acceleration(start:float, mid:float):float = 
             let mutable datafirst = listcut(itemlist,start)
             let dataend = listcut(itemlist,mid)
             let seconddistance = this.TotalDistance(mid)
             let firstdistance = this.TotalDistance(start) - seconddistance
             
             datafirst  <- List.filter(fun t -> ispresent(t,dataend)) datafirst
             if (dataend.Length < 1 ) 
                then  0.0
                else
                    let startingtime = dataend.Head.Time
                    let lasttime = (dataend.Item(dataend.Length - 1)).Time
                    let secondtime = ((lasttime - startingtime).TotalMilliseconds)*1000.0
                    let secondvelocity = seconddistance / secondtime

                    if (datafirst.Length <1) 
                        then
                            secondvelocity/ secondtime
                        else
                            let firststartingtime =  datafirst.Head.Time
                            let firstlasttime = (datafirst.Item(datafirst.Length - 1 )).Time
                            let firsttime = ((firstlasttime - firststartingtime).TotalMilliseconds)*1000.0 
                            let firstvelocity = firstdistance / firsttime

                            (secondvelocity - firstvelocity) / (secondtime)

    ///<summary>
    ///Calcola la velocità istantanea degli ultimi eventi negli ultimi 100ms
    ///</summary>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.InstantAcceleration():float=
            this.Acceleration(200.0,100.0)
           

    ///<summary>
    ///Calcola la posizione media del punto dato un timespan e ritorna il valore
    ///</summary>
    ///<param name="timespan">rappresenta la dimensione di tempo per cui controllare (millisecondi) </param>
    ///<returns>float rappresentante la posizione media</returns>    
    member this.AveragePosition(timespan : float):float = 
            
            let lista = listcut(itemlist,timespan)
            if (lista.Length = 0) 
                then 
                    Double.NaN
                else 
                    lista
                    |> List.map(fun x -> (x:TData1D<'T>).D1)
                    |> List.average 


    ///<summary>
    ///Controlla se il punto è stazionario rispetto ad una certa differenza
    ///</summary>
    ///<param name="timespan">rappresenta la dimensione di tempo per cui controllare (millisecondi) </param>
    ///<param name="tolleranza">range di quanto può variare il valore dal punto ultimo</param>
    ///<returns>vero o falso</returns>    
    member this.StationaryPosition(timespan : float, tolleranza:float) = 
            let newlist = listcut(itemlist,timespan)
            let center = this.AveragePosition(timespan)
            let result = List.forall(fun x -> StaticPoint((x:TData1D<'T>).D1,center,tolleranza  )) newlist

            result




    
    ///<summary>
    /// fa il fitting alla retta, con la regressione lineare usando il metodo QR e restituisce 2 float
    /// L'equazione è Y = r1*X  + r0
    ///</summary>
    ///<return>float x float</returns>
    member this.FittingToLine():float*float =
        let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData1D<'T>).D1) itemlist)
        let firsttime = itemlist.Item(0).Time
        let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanmilliseconds((x:TData1D<'T>).Time , firsttime)) itemlist)

        linearRegression(ArrayX,arrayTime)
(*
        //si potrebbe fare per uniformità con array di 1 così tutti i metodi tornano indietro l'array   
        let dim1x,dim2x  = linearRegression(ArrayX,arrayTime)    //  Y = dim2x * X + dim1x
        ([|dim1x|],[| dim2x|])
*)

    ///<summary>
    /// fa il fitting alla retta a partire da un certo timespan, con la regressione lineare usando il metodo QR e restituisce 2 float
    /// L'equazione è Y = r1*X  + r0
    ///</summary>
    ///<param name="timespan">rappresenta la finestra di tempo per cui controllare dall'ultimo evento (in  millisecondi) </param>
    ///<return>float x float</returns>
    member this.FittingToLine(timespan : float):float*float = 

        let listacorta = listcut (itemlist, timespan)
        let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData1D<'T>).D1) listacorta)
        let firsttime = listacorta.Item(0).Time
        let arrayTime = Seq.toArray ( Seq.map( fun x -> timespanseconds((x:TData1D<'T>).Time , firsttime)) listacorta)

        linearRegression(ArrayX,arrayTime)
(*
        si potrebbe fare per uniformità con array di 1 così tutti i metodi tornano indietro l'array   
        let dim1x,dim2x  = linearRegression(ArrayX,arrayTime)    //  Y = dim2x * X + dim1x
        ([|dim1x|],[| dim2x|])
*)  

    ///<summary>
    /// fa il fitting alla retta e poi controlla la distanza euclidea di tutti i punti a partire dal valore ottenuto in un certo timespan
    ///</summary>
    ///<param name="timespan">rappresenta la finestra di tempo per cui controllare dall'ultimo evento (in  millisecondi) </param>
    ///<param name="tolleranza">soglia che non può essere superata dalla distanza media di tutti i valori dalla rretta</param>
    ///<return>Booleano che indica se la retta segue una linea orizzontale</returns>
    member this.IsStraightDirection(timespan:float,tolleranza:float):bool = 
        
        // y = b*x + a
        let vnoto,coeff = this.FittingToLine(timespan)
        let listacorta = listcut (itemlist,timespan)
        let firsttime = listacorta.Head.Time
        let arrayTimed = List.map (fun x -> new TimespanData(timespanseconds((x:TData1D<'T>).Time , firsttime),(x:TData1D<'T>).D1)) listacorta
 
        let result = 
            arrayTimed
            |> Seq.map(fun f -> disteuclidea(coeff,vnoto,f.D1,f.Time))
            |> Seq.sum 
        if (result / float listacorta.Length) < tolleranza 
                                                        then 
                                                             true
                                                        else 
                                                            false 

    ///<summary>
    ///Fa sampling sull'input restituendo un nuovo oggetto BufferedData 
    ///</summary>
    member this.Sample(funzione:(TData1D<'T> -> bool)) = 
        let data  = new List<TData1D<'T>> ( Seq.filter(funzione) itemlist )
        new Buffered1D<'T>(data ,threshold)

    ///<summary>
    ///<param name=funzione> funzione per calcolare i valori da seguire (rispetto al tempo, float tempo --> timespan data)
    ///<param name=funzione> funzione che verifica la proprietà richiesta, restituendo un bool
    ///<return> Ritorna un float con una percentuale (0.qualcosa) che rappresenta quante iterazioni sono ok</return>
    ///</summary>
    member this.FollowingFunction(theoricfun:(float -> TimespanData), checkingfun:((TimespanData*TimespanData) -> Boolean) , timespan:float):float =    ///TODO : dare titolo
 
        let listatagliata = listcut (itemlist,timespan)
        let primo = listatagliata.Head.Time
        let listatimeshift = List.map (fun x -> let bb = x:TData1D<'T>
                                                new TimespanData(timespanmilliseconds(x.Time,primo),x.D1)) listatagliata
 
        let listafunzione  = List.map ( fun x -> theoricfun (x:TimespanData).Time) listatimeshift
        let totale = List.length listafunzione
        let mutable totvere = 0
        for i in 0 .. totale-1 do
               if checkingfun( listatimeshift.[i],listafunzione.[i]) then totvere <- totvere+1

        (float totvere)/ (float totale)


    ///<summary>
    ///Fa la trasformata di fourier, usa come coefficiente radice n e per l'inversa è 1/(radice n )  
    ///</summary>
    member this.FFT() = 
        let d1buff = Array.map(fun x -> ( new Numerics.Complex((x:TData1D<'T>).D1,0.0 ))) (this.GetArrayBuffer())
        Transform.FourierForward(d1buff)
        d1buff 
        |> Array.map(fun x -> x.Real)

    ///<summary>
    ///Fa la trasformata di fourier, applica un filtro passato, e l'inversa, restituendo un nuovo buffer con i valori T1Data  
    ///</summary>
    ///<param name="filter">il filtro, una funzione da complessi a unit </param>
    ///<return>nuovo Buffered1D con i valori filtrati</returns>
    member this.FFFilter(filter:( Numerics.Complex -> unit) ):Buffered1D<'T> = 
        let d1buff = Array.map (fun x -> ( new Numerics.Complex((x:TData1D<'T>).D1,0.0 ))) (this.GetArrayBuffer())
        Transform.FourierForward(d1buff)
        Array.iter(filter) d1buff
        Transform.FourierInverse(d1buff)
        let valori = Array.map2(fun x y ->  { new TData1D<'T> with 
                                                    member this.D1   = (x:Numerics.Complex).Real
                                                    member this.Time = (y:TData1D<'T>).Time
                                                    member this.Info = (y:TData1D<'T>).Info
                                            })  d1buff  (this.GetArrayBuffer()) 
        Buffered1D(new List<TData1D<'T>>(valori))
     
    ///<summary>
    ///Fa un buffer contenente le differenze tra il dato acquisito rispetto al dato precedente preso in considerazione. 
    ///Il primo dato viene confrontato con se stesso e sarà sempre 0
    ///<param name=timespan> periodo di tempo da considerare
    ///<return> Ritorna un buffer contenente una lista di dati raprresentanti le differenze tra il dato attuale e il dato precedente</return>
    ///</summary>
    member this.DifferenceVector(timespan:float):Buffered1D<'T> =
        let listacorta = listcut(itemlist,timespan)
        if (listacorta.Length>1) then
            let a = List.rev ((List.rev listacorta).Tail)
            let secondlist = List.append ([(listacorta.Head)]) a

            let finallist = List.map2(fun x y -> {new TData1D<'T> with
                                                        member this.D1 = ((x:TData1D<'T>).D1-(y:TData1D<'T>).D1)
                                                        member this.Time = (x:TData1D<'T>).Time
                                                        member this.Info = (x:TData1D<'T>).Info   
                                                 }) listacorta secondlist
        
            let x = new Buffered1D<'T>(new List<TData1D<'T>>(finallist))
            x
        else
            this 

type Buffered2D<'T> (?item:List<TData2D<'T>>, ?soglia:float) =
    inherit BufferedData<TData2D<'T>>()
    
    let itemlist = match item with
                            | None -> new List<TData2D<'T>>()
                            | Some h -> h

    let threshold = match soglia with
                            | None -> 10000.0
                            | Some h -> h


    member this.Count () = itemlist.Count    

    member this.Clear () = itemlist.Clear()

    member this.GetArrayBuffer() = Seq.toArray(itemlist)

    member this.GetListBuffer() = Seq.toList(itemlist)

    override this.AddItem(d:TData2D<'T>, filter:TData2D<'T>->bool) = 
        if (filter d) then itemlist.Add(d)


    override this.AddItem(d:TData2D<'T>) = 
        itemlist.Add (d)
#if Reading
        //Evitare di fare il remove all inserendo eventi con orari arbitrari sarebbero cancellati
#else
        itemlist.RemoveAll(fun x -> (x.Time < System.DateTime.Now.AddMilliseconds(-1.0*threshold)))
        |>ignore

#endif

    ///<summary>
    ///Calcola la lunghezza del periodo reale di tempo campionato dal buffer 
    ///dal primo evento disponibile all'ultimo rispetto il tempo attuale
    ///</summary>
    ///<returns>un oggetto float rappresentante i millisecondi </returns>    
    member this.PeriodLength():float =
            if (itemlist.Count < 1) 
                then 
                    0.0
                else  
                    let primo = itemlist.[0]
                    (System.DateTime.Now.Subtract (primo.Time)).TotalMilliseconds

    ///<summary>
    ///Controlla che il buffer sta ricevendo dati per un periodo "timespan" con una certa continuità. 
    ///utile per evitare falsi possitivi negli altri predicati
    ///</summary>
    ///<param name="timespan">float che rappresenta la soglia della cardinalità da controlalre</param>
    ///<param name="interval">float rappresentante la distanza massima in millisecondi tra 2 eventi continui</param>
    ///<return>True se il numero di item nel buffer è almeno n, False altrimenti</return>   
    member this.IsContinuous(timespan:float, interval:float):Boolean =
            let mylist = listcut( itemlist,timespan)
            continuity(mylist,interval)
            



    ///<summary>
    ///Calcola se la cardinalità dei dati nel buffer è maggiore/uguale di una certa soglia
    ///</summary>
    ///<param name="n">intero che rappresenta la soglia della cardinalità da controlalre</param>
    ///<returns>True se il numero di item nel buffer è almeno n, False altrimenti</returns>   
    member this.Cardinality(n:int):Boolean = 
             n>=itemlist.Count
    



    ///<summary>
    ///Restituisce un nuovo oggetto con il buffer tagliato a tot millisecondi
    ///</summary>
    ///<param name="millisec"> la porzione di tempo da tenere a partire dall'istante attuale, in millisecondi </param>
    ///<returns>un nuovo oggetto Buffereed2D</returns>    
    member this.cutBuffer(millisec:float):Buffered2D<'T> = 
            let newlist = listcut(itemlist,millisec)
            new Buffered2D<'T>(new List<TData2D<'T>> ( newlist))

    ///<summary>
    ///Calcola la velocità istantanea degli ultimi eventi negli ultimi 100ms
    ///</summary>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.InstantVelocity() = 
           this.AverageVelocity(100.0)

    ///<summary>
    ///Calcola la distanza totale percorsa percorsa dato un intervallo di tempo, in millisecondi
    ///</summary>
    ///<param name="timespan">Float che rappresenta il periodo in cui calcolare la distanza percorsa</param>
    ///<returns>la distanza totale percorsa</returns>
    member this.TotalDistance(timespan:float) = 
            
            let mutable datacp = listcut(itemlist, timespan)
             
            if (Seq.length datacp >2)
                then
                    let mutable current = datacp.Head
                    let mutable distance = 0.0
                    datacp <- datacp.Tail
                    while(not(datacp.IsEmpty))
                        do
                        distance <- Math.Sqrt(sqr(current.D1 - datacp.Head.D1)+sqr(current.D2 - datacp.Head.D2)) + distance
                        current <- datacp.Head
                        datacp <- datacp.Tail

                    distance
                else
                    0.0 


    ///<summary>
    ///Calcola la distanza totale percorsa percorsa dato un intervallo di tempo, in millisecondi
    ///</summary>
    ///<param name="timespan">Float che rappresenta il periodo in cui calcolare la distanza percorsa</param>
    ///<returns>la distanza totale percorsa</returns>
    member this.ComponentDistance(timespan:float):float*float = 
            
            let mutable datacp = listcut(itemlist, timespan)
            
            if (Seq.length datacp >2)
                then
                    let mutable current = datacp.Head
                    let mutable distanceX = 0.0
                    let mutable distanceY = 0.0
                    datacp <- datacp.Tail
                    while(not(datacp.IsEmpty))
                        do
                        distanceX <- Math.Abs(current.D1 - datacp.Head.D1) + distanceX
                        distanceY <- Math.Abs(current.D2 - datacp.Head.D2) + distanceY
                        current <- datacp.Head
                        datacp <- datacp.Tail

                    distanceX,distanceY
                else
                    0.0,0.0 


    ///<summary>
    ///Calcola la velocità media percorsa dato un intervallo di tempo, in millisecondi
    ///</summary>
    ///<param name="timespan">Float che rappresenta il periodo in cui calcolare la velocità media</param>
    ///<returns>il valore di velocità media, oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.AverageVelocity(timespan:float) = 
            
            let datacp = listcut(itemlist, timespan)
                   
            if (Seq.length datacp >2)
                then
                    let startingtime = datacp.Head.Time
                    let lasttime = (datacp.Item(datacp.Length - 1)).Time
                    let distance = this.TotalDistance(timespan)
                  
                    let velocity = (distance / (lasttime - startingtime).TotalMilliseconds)*1000.0
                    velocity   
                else
                    0.0 
 
    ///<summary>
    ///Calcola la accelerazione ottenuta comparando la velocità ottenuta nella parte tra start e mid con quella da mid al momento attuale
    ///</summary>
    ///<param name="start">rappresenta il punto di inizio della area presa in considerazione come momento precedente, da considerare fino al mid </param>
    ///<param name="mid"> rappresenta il punto da cui in poi verrà preso in considerazione come momento attuale fino al momento attuale </param>
    ///<returns>float rappresentante la posizione media</returns>    
    member this.Acceleration(start:float, mid:float):float = 
             let mutable datafirst = listcut(itemlist,start)
             let dataend = listcut(itemlist,mid)
             let seconddistance = this.TotalDistance(mid)
             let firstdistance = this.TotalDistance(start) - seconddistance
             
             datafirst  <- List.filter(fun t -> ispresent(t,dataend)) datafirst
             if (dataend.Length < 1 ) 
                then  0.0
                else
                    let startingtime = dataend.Head.Time
                    let lasttime = (dataend.Item(dataend.Length - 1)).Time
                    let secondtime = ((lasttime - startingtime).TotalMilliseconds)*1000.0
                    let secondvelocity = seconddistance / secondtime

                    if (datafirst.Length <1) 
                        then
                            secondvelocity/ secondtime
                        else
                            let firststartingtime =  datafirst.Head.Time
                            let firstlasttime = (datafirst.Item(datafirst.Length - 1 )).Time
                            let firsttime = ((firstlasttime - firststartingtime).TotalMilliseconds)*1000.0 
                            let firstvelocity = firstdistance / firsttime

                            (secondvelocity - firstvelocity) / (secondtime)

    ///<summary>
    ///Calcola la velocità istantanea degli ultimi eventi negli ultimi 100ms
    ///</summary>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.InstantAcceleration():float=
            this.Acceleration(200.0,100.0)

    ///<summary>
    ///Calcola la posizione media del punto dato un timespan e ritorna il valore
    ///</summary>
    ///<param name="timespan">rappresenta la dimensione di tempo per cui controllare (millisecondi) </param>
    ///<returns>float rappresentante la posizione media</returns>    
    member this.AveragePosition(timespan : float):float*float = 
            
            let lista = listcut(itemlist,timespan)
            if (lista.Length = 0) 
                then 
                    Double.NaN,Double.NaN
                else 
                    let D1 = (
                            lista
                            |> List.map(fun x -> (x:TData2D<'T>).D1)
                            |> List.average
                            )
                    let D2 = (
                            lista
                            |> List.map(fun x -> (x:TData2D<'T>).D2)
                            |> List.average
                            )
                    D1,D2

    ///<summary>
    ///Controlla se il punto è stazionario rispetto ad una certa differenza
    ///</summary>
    ///<param name="timespan">rappresenta la dimensione di tempo per cui controllare (millisecondi) </param>
    ///<param name="tolleranza">range di quanto può variare il valore dal punto centrale (in quantità assoluta)</param>
    ///<returns>vero o falso</returns>    
    member this.StationaryPosition(timespan : float, tolleranza:float) = 
            if itemlist.Count <2 
                then 
                    false
                else
                    let center = itemlist.Item(itemlist.Count-1)
                    let newlist = listcut(itemlist,timespan)

                    let result = Seq.forall(fun x -> (StaticPoint((x:TData2D<'T>).D1,center.D1,tolleranza)  &&
                                                      StaticPoint((x:TData2D<'T>).D2,center.D2,tolleranza))
                                                      ) newlist
                    result
                
    ///<summary>
    /// fa il fitting alla retta, con la regressione lineare usando il metodo QR e restituisce 2 float
    /// L'equazione è Y = r1*X  + r0
    ///</summary>
    ///<return>float[] della costante * float[] per il coefficiente della X</returns>
    member this.FittingToLine():float[]*float[] =

            let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData2D<'T>).D1) itemlist)
            let ArrayY = Seq.toArray ( Seq.map(fun x -> (x:TData2D<'T>).D2) itemlist)
            let firsttime = itemlist.Item(0).Time
            let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData2D<'T>).Time , firsttime)) itemlist)

            let dim1x,dim2x  = linearRegression(ArrayX,arrayTime)    //  Y = dim2x * X + dim1x
            let dim1y,dim2y  = linearRegression(ArrayY,arrayTime)    //  Y = dim2x * X + dim1x

            ([|dim1x ; dim1y|],[| dim2x; dim2y|])

    ///<summary>
    /// fa il fitting alla retta, con la regressione lineare usando il metodo QR e restituisce 2 float
    /// L'equazione è Y = r1*X  + r0
    ///</summary>
    ///<return>float[] della costante * float[] per il coefficiente della X</returns>
    member this.FittingToLine(timespan:float):float[]*float[] =

            let listacorta = listcut (itemlist, timespan)
            let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData2D<'T>).D1) listacorta)
            let ArrayY = Seq.toArray ( Seq.map(fun x -> (x:TData2D<'T>).D2) listacorta)
            let firsttime = listacorta.Item(0).Time
            let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData2D<'T>).Time , firsttime)) listacorta)

            let dim1x,dim2x  = linearRegression(ArrayX,arrayTime)    //  Y = dim2x * X + dim1x
            let dim1y,dim2y  = linearRegression(ArrayY,arrayTime)    //  Y = dim2x * X + dim1x

            ([|dim1x ; dim1y|],[| dim2x; dim2y|])

    ///<summary>
    /// fa il fitting alla retta e poi controlla la distanza euclidea di tutti i punti a partire dal valore ottenuto in un certo timespan
    ///</summary>
    ///<param name="timespan">rappresenta la finestra di tempo per cui controllare dall'ultimo evento (in  millisecondi) </param>
    ///<param name="tolleranza">soglia che non può essere superata dalla distanza media di tutti i valori dalla rretta</param>
    ///<return>Booleano che indica se la retta segue una linea orizzontale</returns>
    member this.IsStraightDirection(timespan:float,tolleranza:float):bool = 
        
        // y = b*x + a
        let vnoto,coeff = this.FittingToLine(timespan)
        let listacorta = listcut (itemlist,timespan)
        let firsttime = listacorta.Head.Time
        let arrayTimed = List.map (fun x -> new TimespanData(timespanseconds((x:TData2D<'T>).Time , firsttime),(x:TData2D<'T>).D1,(x:TData2D<'T>).D2)) listacorta
 
        let result = 
            arrayTimed
            |> Seq.map (fun f -> Math.Sqrt( sqr(disteuclidea(coeff.[0],vnoto.[0],f.D1,f.Time))+sqr(disteuclidea(coeff.[1],vnoto.[1],f.D2,f.Time))))  // radice quadrata del quadrato delle distanze x dimensione
            |> Seq.sum 
        if (result / float listacorta.Length) < tolleranza 
                                                        then true
                                                        else false 

    member this.LinearMovement(timespan:float,tolleranza:float):bool = 
        

        let listacorta = listcut (itemlist, timespan)
        let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData2D<'T>).D1) listacorta)
        let ArrayY = Seq.toArray ( Seq.map(fun x -> (x:TData2D<'T>).D2) listacorta)
        let firsttime = listacorta.Item(0).Time
        let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData2D<'T>).Time , firsttime)) listacorta)

        let vnoto,coeff  = linearRegression(ArrayY,ArrayX)    //  Y = dim2x * X + dim1x

        let firsttime = listacorta.Head.Time
        let Listavalori = List.map (fun x -> new TimespanData(0.0,(x:TData2D<'T>).D1,(x:TData2D<'T>).D2)) listacorta
 
        let result = 
            Listavalori
            |> Seq.map (fun f -> disteuclidea(coeff,vnoto,f.D2,f.D1))  // radice quadrata del quadrato delle distanze x dimensione

        Seq.forall(fun x -> ( x < tolleranza) )  result 

    member this.Sample(funzione:(TData2D<'T> -> bool)) = 
        let data  = new List<TData2D<'T>> ( Seq.filter(funzione) itemlist )
        new Buffered2D<'T>(data ,threshold)

    
    ///<summary>
    ///<param name=theoricfun> funzione per calcolare i valori da seguire (rispetto al tempo, float tempo --> timespan data)
    ///<param name=checkingfun> funzione che verifica la proprietà richiesta, restituendo un bool
    ///<return> Ritorna un float con una percentuale (0.qualcosa) che rappresenta quante iterazioni sono ok</return>
    ///</summary>
    member this.FollowingFunction(theoricfun:(float -> TimespanData), checkingfun:((TimespanData*TimespanData) -> Boolean) , timespan:float) =    ///TODO : dare titolo
 
        let listatagliata = listcut (itemlist,timespan)
        let primo = listatagliata.Head.Time
        let listatimeshift = List.map (fun x -> let bb = x:TData2D<'T>
                                                new TimespanData(timespanmilliseconds(x.Time,primo),x.D1,x.D2)) listatagliata
 
        let listafunzione  = List.map ( fun x -> theoricfun (x:TimespanData).Time) listatimeshift
        let totale = List.length listafunzione
        let mutable totvere = 0
        for i in 0 .. totale-1 do
               if checkingfun( listatimeshift.[i],listafunzione.[i]) then totvere <- totvere+1

        (float totvere)/ (float totale)

    ///<summary>
    ///Fa un buffer contenente le differenze tra il dato acquisito rispetto al dato precedente preso in considerazione. 
    ///Il primo dato viene confrontato con se stesso e sarà sempre 0
    ///<param name=timespan> periodo di tempo da considerare
    ///<return> Ritorna un buffer contenente una lista di dati raprresentanti le differenze tra il dato attuale e il dato precedente</return>
    ///</summary>
    member this.DifferenceVector(timespan:float):Buffered2D<'T> =
        let listacorta = listcut(itemlist,timespan)
        if (listacorta.Length>1) then
            let a = List.rev ((List.rev listacorta).Tail)
            let secondlist = List.append ([(listacorta.Head)]) a

            let finallist = List.map2(fun x y -> {new TData2D<'T> with
                                                        member this.D1 = ((x:TData2D<'T>).D1-(y:TData2D<'T>).D1)
                                                        member this.D2 = ((x:TData2D<'T>).D2-(y:TData2D<'T>).D2)
                                                        member this.Time = (x:TData2D<'T>).Time
                                                        member this.Info = (x:TData2D<'T>).Info   
                                                 }) listacorta secondlist
        
            let x = new Buffered2D<'T>(new List<TData2D<'T>>(finallist))
            x
        else
            this 


type Buffered3D<'T> (?item:List<TData3D<'T>>, ?soglia:float) =
    inherit BufferedData<TData3D<'T>>()
    

    let threshold = match soglia with
                            | None -> 10000.0
                            | Some h -> h

    let itemlist = match item with
                            | None -> new List<TData3D<'T>>()
                            | Some h -> h

    member this.Count () = itemlist.Count    

    member this.Clear () = itemlist.Clear()

    member this.GetArrayBuffer() = Seq.toArray(itemlist)

    member this.GetListBuffer() = Seq.toList(itemlist)

    override this.AddItem(d:TData3D<'T>, filter:TData3D<'T>->bool) = 
        if (filter d) then itemlist.Add(d)

    override this.AddItem(d:TData3D<'T>) =
            itemlist.Add  (d)
 #if Reading
            //Evitare di fare il remove all inserendo eventi con orari arbitrari sarebbero cancellati
#else
            itemlist.RemoveAll(fun x -> (x.Time < System.DateTime.Now.AddMilliseconds(-1.0*threshold)))
            |>ignore
#endif



    ///<summary>
    ///Calcola la lunghezza del periodo reale di tempo campionato dal buffer 
    ///dal primo evento disponibile all'ultimo rispetto il tempo attuale
    ///</summary>
    ///<returns>un oggetto float rappresentante i millisecondi </returns>    
    member this.PeriodLength():float =
            if (itemlist.Count < 1) 
                then 
                    0.0
                else  
                    let primo = itemlist.[0]
                    (System.DateTime.Now.Subtract (primo.Time)).TotalMilliseconds

    ///<summary>
    ///Controlla che il buffer sta ricevendo dati per un periodo "timespan" con una certa continuità. 
    ///utile per evitare falsi possitivi negli altri predicati
    ///</summary>
    ///<param name="timespan">float che rappresenta la soglia della cardinalità da controlalre</param>
    ///<param name="interval">float rappresentante la distanza massima in millisecondi tra 2 eventi continui</param>
    ///<return>True se il numero di item nel buffer è almeno n, False altrimenti</return>   
    member this.IsContinuous(timespan:float, interval:float):Boolean =
            let mylist = listcut( itemlist,timespan)
            continuity(mylist,interval)
            


    ///<summary>
    ///Calcola se la cardinalità dei dati nel buffer è maggiore/uguale di una certa soglia
    ///</summary>
    ///<param name="n">intero che rappresenta la soglia della cardinalità da controlalre</param>
    ///<returns>True se il numero di item nel buffer è almeno n, False altrimenti</returns>   
    member this.Cardinality(n:int):Boolean = 
             n>=itemlist.Count
    

    ///<summary>
    ///Restituisce un nuovo oggetto con il buffer tagliato a tot millisecondi
    ///</summary>
    ///<param name="millisec"> la porzione di tempo da tenere a partire dall'istante attuale, in millisecondi </param>
    ///<returns>un nuovo oggetto Buffereed2D</returns>    
    member this.cutBuffer(millisec:float):Buffered3D<'T> = 
            let newlist = listcut(itemlist,millisec)
            new Buffered3D<'T>(new List<TData3D<_>> ( newlist))


    ///<summary>
    ///Calcola la distanza totale percorsa percorsa dato un intervallo di tempo, in millisecondi
    ///</summary>
    ///<param name="timespan">Float che rappresenta il periodo in cui calcolare la distanza percorsa</param>
    ///<returns>la distanza totale percorsa</returns>
    member this.TotalDistance(timespan:float) = 
            
            let mutable datacp = listcut(itemlist, timespan)
             
            if (Seq.length datacp >2)
                then
                    let mutable current = datacp.Head
                    let mutable distance = 0.0
                    datacp <- datacp.Tail
                    while(not(datacp.IsEmpty))
                        do
                        distance <- Math.Sqrt(sqr(current.D1 - datacp.Head.D1)+sqr(current.D2 - datacp.Head.D2) + sqr(current.D3 - datacp.Head.D3)) + distance
                        current <- datacp.Head
                        datacp <- datacp.Tail

                    distance
                else
                    0.0 


    ///<summary>
    ///Calcola la distanza totale percorsa percorsa dato un intervallo di tempo, in millisecondi
    ///</summary>
    ///<param name="timespan">Float che rappresenta il periodo in cui calcolare la distanza percorsa</param>
    ///<returns>la distanza totale percorsa</returns>
    member this.ComponentDistance(timespan:float):float*float*float = 
            
            let mutable datacp = listcut(itemlist, timespan)
            
            if (Seq.length datacp >2)
                then
                    let mutable current = datacp.Head
                    let mutable distanceX = 0.0
                    let mutable distanceY = 0.0
                    let mutable distanceZ = 0.0
                    datacp <- datacp.Tail
                    while(not(datacp.IsEmpty))
                        do
                        distanceX <- Math.Abs(current.D1 - datacp.Head.D1) + distanceX
                        distanceY <- Math.Abs(current.D2 - datacp.Head.D2) + distanceY
                        distanceZ <- Math.Abs(current.D3 - datacp.Head.D3) + distanceZ

                        current <- datacp.Head
                        datacp <- datacp.Tail

                    distanceX,distanceY,distanceZ
                else
                    0.0,0.0,0.0 



    ///<summary>
    ///Calcola la velocità istantanea degli ultimi eventi negli ultimi 100ms
    ///</summary>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.InstantVelocity() = 
           this.AverageVelocity(100.0)




    ///<summary>
    ///Calcola la velocità media percorsa dato un intervallo di tempo, in millisecondi
    ///</summary>
    ///<param name="timespan">Float che rappresenta il periodo in cui calcolare la velocità media</param>
    ///<returns>il valore di velocità media, oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.AverageVelocity(timespan:float) = 
            
            let mutable datacp = listcut(itemlist, timespan)
                
            if (Seq.length datacp >2)
                then
                    let startingtime = datacp.Head.Time
                    let lasttime = (datacp.Item(datacp.Length - 1)).Time
                    let distance = this.TotalDistance(timespan)
                  
                    let velocity = (distance / (lasttime - startingtime).TotalMilliseconds)*1000.0
                    velocity   
                else
                    0.0 

    ///<summary>
    ///Calcola la accelerazione ottenuta comparando la velocità ottenuta nella parte tra start e mid con quella da mid al momento attuale
    ///</summary>
    ///<param name="start">rappresenta il punto di inizio della area presa in considerazione come momento precedente, da considerare fino al mid </param>
    ///<param name="mid"> rappresenta il punto da cui in poi verrà preso in considerazione come momento attuale fino al momento attuale </param>
    ///<returns>float rappresentante la posizione media</returns>    
    member this.Acceleration(start:float, mid:float):float = 
             let mutable datafirst = listcut(itemlist,start)
             let dataend = listcut(itemlist,mid)
             let seconddistance = this.TotalDistance(mid)
             let firstdistance = this.TotalDistance(start) - seconddistance
             
             datafirst  <- List.filter(fun t -> ispresent(t,dataend)) datafirst
             if (dataend.Length < 1 ) 
                then  0.0
                else
                    let startingtime = dataend.Head.Time
                    let lasttime = (dataend.Item(dataend.Length - 1)).Time
                    let secondtime = ((lasttime - startingtime).TotalMilliseconds)*1000.0
                    let secondvelocity = seconddistance / secondtime

                    if (datafirst.Length <1) 
                        then
                            secondvelocity/ secondtime
                        else
                            let firststartingtime =  datafirst.Head.Time
                            let firstlasttime = (datafirst.Item(datafirst.Length - 1 )).Time
                            let firsttime = ((firstlasttime - firststartingtime).TotalMilliseconds)*1000.0 
                            let firstvelocity = firstdistance / firsttime

                            (secondvelocity - firstvelocity) / (secondtime)

   ///<summary>
    ///Calcola la velocità istantanea degli ultimi eventi negli ultimi 100ms
    ///</summary>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.InstantAcceleration():float=
            this.Acceleration(200.0,100.0)


    ///<summary>
    ///Calcola la posizione media del punto dato un timespan e ritorna il valore
    ///</summary>
    ///<param name="timespan">rappresenta la dimensione di tempo per cui controllare (millisecondi) </param>
    ///<returns>float rappresentante la posizione media</returns>    
    member this.AveragePosition(timespan : float):float*float*float = 
            
            let lista = listcut(itemlist,timespan)
            if (lista.Length = 0) 
                then 
                    Double.NaN,Double.NaN,Double.NaN
                else 
                    let D1 = (
                            lista
                            |> List.map(fun x -> (x:TData3D<'T>).D1)
                            |> List.average
                            )
                    let D2 = (
                            lista
                            |> List.map(fun x -> (x:TData3D<'T>).D2)
                            |> List.average
                            )
                    let D3 = (
                            lista
                            |> List.map(fun x -> (x:TData3D<'T>).D3)
                            |> List.average
                            )

                    D1,D2,D3



    ///<summary>
    ///Controlla se il punto è stazionario rispetto ad una certa differenza
    ///</summary>
    ///<param name="timespan">rappresenta la dimensione di tempo per cui controllare (millisecondi) </param>
    ///<param name="tolleranza">range di quanto può variare il valore dal punto centrale</param>
    ///<returns>vero o falso</returns>       
    member this.StationaryPosition(timespan : float, tolleranza:float) = 
            if itemlist.Count <2
              then 
                false
              else
                let center = itemlist.Item(itemlist.Count-1)
                let newlist = listcut(itemlist,timespan)

                let centeredlist = List.forall(fun x -> (StaticPoint((x:TData3D<'T>).D1,center.D1,tolleranza)  &&
                                                         StaticPoint((x:TData3D<'T>).D2,center.D2,tolleranza)  &&
                                                         StaticPoint((x:TData3D<'T>).D3,center.D3,tolleranza) 
                                                         )) newlist

                centeredlist

    ///<summary>
    /// fa il fitting alla retta, con la regressione lineare usando il metodo QR e restituisce 2 float
    /// L'equazione è Y = r1*X  + r0
    ///</summary>
    ///<return>float[] della costante * float[] per il coefficiente della X</returns>
    member this.FittingToLine():float[]*float[] =

            let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData3D<'T>).D1) itemlist)
            let ArrayY = Seq.toArray ( Seq.map(fun x -> (x:TData3D<'T>).D2) itemlist)
            let ArrayZ = Seq.toArray ( Seq.map(fun x -> (x:TData3D<'T>).D3) itemlist)

            let firsttime = itemlist.Item(0).Time
            let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData3D<'T>).Time , firsttime)) itemlist)

            let dim1x,dim2x  = linearRegression(ArrayX,arrayTime)    //  Y = dim2x * X + dim1x
            let dim1y,dim2y  = linearRegression(ArrayY,arrayTime)    //  Y = dim2x * X + dim1x
            let dim1z,dim2z  = linearRegression(ArrayZ,arrayTime)    //  Y = dim2x * X + dim1x

            ([|dim1x ; dim1y ; dim1z|],[| dim2x ; dim2y ; dim2z |])


    ///<summary>
    /// fa il fitting alla retta, con la regressione lineare usando il metodo QR e restituisce 2 float
    /// L'equazione è Y = r1*X  + r0
    ///</summary>
    ///<return>float[] della costante * float[] per il coefficiente della X</returns>
    member this.FittingToLine(timespan:float):float[]*float[] =

            let listacorta = listcut (itemlist, timespan)
            let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData3D<'T>).D1) listacorta)
            let ArrayY = Seq.toArray ( Seq.map(fun x -> (x:TData3D<'T>).D2) listacorta)
            let ArrayZ = Seq.toArray ( Seq.map(fun x -> (x:TData3D<'T>).D3) listacorta)

            let firsttime = listacorta.Item(0).Time
            let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData3D<'T>).Time , firsttime)) listacorta)

            let dim1x,dim2x  = linearRegression(ArrayX,arrayTime)    //  Y = dim2x * X + dim1x
            let dim1y,dim2y  = linearRegression(ArrayY,arrayTime)    //  Y = dim2x * X + dim1x
            let dim1z,dim2z  = linearRegression(ArrayZ,arrayTime)    //  Y = dim2x * X + dim1x

            ([|dim1x ; dim1y ; dim1z|],[| dim2x ; dim2y ; dim2z |])


    ///<summary>
    /// fa il fitting alla retta e poi controlla la distanza euclidea di tutti i punti a partire dal valore ottenuto in un certo timespan
    ///</summary>
    ///<param name="timespan">rappresenta la finestra di tempo per cui controllare dall'ultimo evento (in  millisecondi) </param>
    ///<param name="tolleranza">soglia che non può essere superata dalla distanza media di tutti i valori dalla rretta</param>
    ///<return>Booleano che indica se la retta segue una linea orizzontale</returns>
    member this.IsStraightDirection(timespan:float,tolleranza:float):bool = 
        
        // y = b*x + a
        let vnoto,coeff = this.FittingToLine(timespan)
        let listacorta = listcut (itemlist,timespan)
        let firsttime = listacorta.Head.Time
        let arrayTimed = List.map (fun x -> new TimespanData(timespanseconds((x:TData3D<'T>).Time , firsttime),(x:TData3D<'T>).D1,(x:TData3D<'T>).D2,(x:TData3D<'T>).D3)) listacorta
 
        let result = 
            arrayTimed
            |> Seq.map (fun f -> Math.Sqrt( sqr(disteuclidea(coeff.[0],vnoto.[0],f.D1,f.Time))+sqr(disteuclidea(coeff.[1],vnoto.[1],f.D2,f.Time))+sqr(disteuclidea(coeff.[2],vnoto.[2],f.D3,f.Time))))  // radice quadrata del quadrato delle distanze x dimensione
            |> Seq.sum 
 
        if (result / float listacorta.Length) < tolleranza 
                                                        then true
                                                        else false 

    member this.Sample(funzione:(TData3D<'T> -> bool)) = 
        let data  = new List<TData3D<'T>> ( Seq.filter(funzione) itemlist )
        new Buffered3D<'T>(data ,threshold)



    ///<summary>
    ///<param name=theoricfun> funzione per calcolare i valori da seguire (rispetto al tempo, float tempo --> timespan data)
    ///<param name=checkingfun> funzione che verifica la proprietà richiesta, restituendo un bool
    ///<return> Ritorna un float con una percentuale (0.qualcosa) che rappresenta quante iterazioni sono ok</return>
    ///</summary>
    member this.FollowingFunction(theoricfun:(float -> TimespanData), checkingfun:((TimespanData*TimespanData) -> Boolean) , timespan:float) =    ///TODO : dare titolo
 
        let listatagliata = listcut (itemlist,timespan)
        let primo = listatagliata.Head.Time
        let listatimeshift = List.map (fun x -> let bb = x:TData3D<'T>
                                                new TimespanData(timespanmilliseconds(x.Time,primo),x.D1,x.D2,x.D3)) listatagliata
 
        let listafunzione  = List.map ( fun x -> theoricfun (x:TimespanData).Time) listatimeshift
        let totale = List.length listafunzione
        let mutable totvere = 0
        for i in 0 .. totale-1 do
               if checkingfun( listatimeshift.[i],listafunzione.[i]) then totvere <- totvere+1

        (float totvere)/ (float totale)


    ///<summary>
    ///Fa un buffer contenente le differenze tra il dato acquisito rispetto al dato precedente preso in considerazione. 
    ///Il primo dato viene confrontato con se stesso e sarà sempre 0
    ///<param name=timespan> periodo di tempo da considerare
    ///<return> Ritorna un buffer contenente una lista di dati raprresentanti le differenze tra il dato attuale e il dato precedente</return>
    ///</summary>
    member this.DifferenceVector(timespan:float):Buffered3D<'T> =
        let listacorta = listcut(itemlist,timespan)
        if (listacorta.Length>1) then
            let a = List.rev ((List.rev listacorta).Tail)
            let secondlist = List.append ([(listacorta.Head)]) a

            let finallist = List.map2(fun x y -> {new TData3D<'T> with
                                                        member this.D1 = ((x:TData3D<'T>).D1-(y:TData3D<'T>).D1)
                                                        member this.D2 = ((x:TData3D<'T>).D2-(y:TData3D<'T>).D2)
                                                        member this.D3 = ((x:TData3D<'T>).D3-(y:TData3D<'T>).D3)
                                                        member this.Time = (x:TData3D<'T>).Time
                                                        member this.Info = (x:TData3D<'T>).Info   
                                                 }) listacorta secondlist
        
            let x = new Buffered3D<'T>(new List<TData3D<'T>>(finallist))
            x
        else
            this