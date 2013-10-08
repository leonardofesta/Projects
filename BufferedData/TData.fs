module GestIT.TData

open System
open GestIT.IData
open GestIT.Utils
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Distributions
open MathNet.Numerics.IntegralTransforms
open MathNet.Numerics.IntegralTransforms.Algorithms
                                    
type Direction1D =
        |  Positive = 1
        |  Negative = -1
        |  Casual = 0

type Direction2D = 
        |   Casual = 0
        |   Top = 1
        |   TopRight = 2 
        |   Right = 3
        |   BottomRight  = 4 
        |   Bottom = 5
        |   BottomLeft = 6
        |   Left = 7
        |   TopLeft = 8

type Direzione(timespan:float,x:float,?y:float,?z:float) = 
           member this.D1:float = x
           member this.D2:float = match y with
                                        | Some t->t
                                        | None -> -1.0
           member this.D3:float = match z with
                                        | Some t->t
                                        | None -> -1.0
           member this.Time:float = timespan


type Buffered1D (?item:List<TData1D>, ?soglia:float) =
    inherit BufferedData<TData1D>()

    let threshold = match soglia with
                            | None -> 10000.0
                            | Some h -> h
    
    let  itemlist = match item with
                            | None -> new List<TData1D>()
                            | Some h -> h
  
    member this.Count () = itemlist.Count    

    member this.Clear () = itemlist.Clear()

    member this.GetArrayBuffer() = Seq.toArray(itemlist)

    member this.GetListBuffer() = Seq.toList(itemlist)

    
    override this.AddItem(d:TData1D) = 
        itemlist.Add (d)
        System.Console.WriteLine("Elemento aggiunto " + d.D1.ToString() + "  con tempo  " + d.Time.ToString())
  //      itemlist.RemoveAll(fun x -> (x.Time < System.DateTime.Now.AddMilliseconds(-1.0*threshold)))
        |>ignore

    ///<summary>
    ///Restituisce un nuovo oggetto con il buffer tagliato a tot millisecondi
    ///</summary>
    ///<param name="millisec"> la porzione di tempo da tenere a partire dall'istante attuale, in millisecondi </param>
    ///<returns>un oggetto Buffereed1D</returns>    
    member this.cutBuffer(millisec:float):Buffered1D = 
            let newlist = listcut(itemlist,millisec)
            new Buffered1D(new List<TData1D> ( newlist))

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
            
    //        Console.WriteLine("lunghezza itemlist -> " + (Seq.length itemlist).ToString())
    //        Console.WriteLine("lunghezza datacp -> " + (Seq.length datacp).ToString())
            
            if (Seq.length datacp >2)
                then
                    let mutable current = datacp.Head.D1
                    let startingtime = datacp.Head.Time
                    let lasttime = (datacp.Item(datacp.Length - 1)).Time
                    let mutable distance = 0.0
                    datacp <- datacp.Tail
                    while(not(datacp.IsEmpty))
                        do
                        distance <- Math.Abs(current - datacp.Head.D1) + distance
                        current <- datacp.Head.D1
                        datacp <- datacp.Tail

                    let velocity = (distance / (lasttime - startingtime).TotalMilliseconds)*1000.0
                    System.Console.WriteLine (velocity.ToString())
                    velocity   
                else
                    0.0 // TODO : Decidere cosa fare x quando non ho dettagli



    // FORSE INUTILE
    ///<summary>
    ///Da la direzione positiva o negativa se la lista di eventi a partire da un determinato timespan è consistente verso una direzione, 
    ///altrimenti restituisce direzione casuale
    ///</summary>
    ///<param name="timespan">rappresenta la dimensione di tempo per cui controllare (millisecondi) </param>
    ///<param name="tolleranza">tolleranza nelle possibilità di insuccesso, float 0<x<1 in caso di assenza non si prevede tolleranza</param>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.Direction ( timespan : float, ?tolleranza:float) = 
            let last = itemlist.Item(itemlist.Count - 1)
            let newlist = listcut (itemlist,timespan)

            let positivelist = List.filter(fun x -> (x:TData1D).D1<= last.D1 ) newlist
            let negativelist = List.filter(fun x -> (x:TData1D).D1>= last.D1 ) newlist
            let mutable toll = 1.0

            match tolleranza with 
                | None -> toll<- 1.0
                | Some s -> toll <- s

            if ( float( positivelist.Length ) > (toll * (float (newlist.Length))))
                then Direction1D.Positive
                elif ( float (negativelist.Length) > (toll* float( newlist.Length)))
                then Direction1D.Negative
                else Direction1D.Casual


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
                    |> List.map(fun x -> (x:TData1D).D1)
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
            let result = List.forall(fun x -> StaticPoint((x:TData1D).D1,center,tolleranza  )) newlist

            result




    
    ///<summary>
    /// fa il fitting alla retta, con la regressione lineare usando il metodo QR e restituisce 2 float
    /// L'equazione è Y = r1*X  + r0
    ///</summary>
    ///<return>float x float</returns>
    member this.FittingToLine():float*float =
        let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData1D).D1) itemlist)
        let firsttime = itemlist.Item(0).Time
        let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanmilliseconds((x:TData1D).Time , firsttime)) itemlist)

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
        let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData1D).D1) listacorta)
        let firsttime = listacorta.Item(0).Time
        let arrayTime = Seq.toArray ( Seq.map( fun x -> timespanseconds((x:TData1D).Time , firsttime)) listacorta)

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
        let arrayTimed = List.map (fun x -> new Direzione(timespanseconds((x:TData1D).Time , firsttime),(x:TData1D).D1)) listacorta
 
        let result = 
            arrayTimed
            |> Seq.map(fun f -> distanzaeuclidea(coeff,vnoto,f.D1,f.Time))
            |> Seq.sum 
        System.Console.WriteLine("Distanza eucidea totale = " + result.ToString() + " diviso le unità " + ( result/  float arrayTimed.Length ).ToString())
        if (result / float listacorta.Length) < tolleranza 
                                                        then System.Console.WriteLine("canae")
                                                             true
                                                        else false 


    ///<summary>
    ///Fa la trasformata di fourier, usa come coefficiente radice n e per l'inversa è 1/(radice n )  
    ///</summary>
    member this.FFT() = 
        let d1buff = Array.map(fun x -> ( new Numerics.Complex((x:TData1D).D1,0.0 ))) (this.GetArrayBuffer())
        Transform.FourierForward(d1buff)
        d1buff 
        |> Array.map(fun x -> x.Real)

    ///<summary>
    ///Fa la trasformata di fourier, applica un filtro passato, e l'inversa, restituendo un nuovo buffer con i valori T1Data  
    ///</summary>
    ///<param name="filter">il filtro, una funzione da complessi a unit </param>
    ///<return>nuovo Buffered1D con i valori filtrati</returns>
    member this.FFFilter(filter:( Numerics.Complex -> unit) ):Buffered1D = 
        let d1buff = Array.map (fun x -> ( new Numerics.Complex((x:TData1D).D1,0.0 ))) (this.GetArrayBuffer())
        Transform.FourierForward(d1buff)
        Array.iter(filter) d1buff
        Transform.FourierInverse(d1buff)
        let valori = Array.map2(fun x y ->  { new TData1D with 
                                                    member this.D1 = (x:Numerics.Complex).Real
                                                    member this.Time = (y:TData1D).Time
                                        })  d1buff  (this.GetArrayBuffer()) 
        Buffered1D(new List<TData1D>(valori))
  

type Buffered2D (?item:List<TData2D>, ?soglia:float) =
    inherit BufferedData<TData2D>()
    
    let itemlist = match item with
                            | None -> new List<TData2D>()
                            | Some h -> h

    let threshold = match soglia with
                            | None -> 10000.0
                            | Some h -> h


    member this.Count () = itemlist.Count
  
    member this.Clear () = 
            itemlist.Clear()

    override this.AddItem(d:TData2D) = 
        itemlist.Add (d)
   //     itemlist.RemoveAll(fun x -> (x.Time < System.DateTime.Now.AddMilliseconds(-1.0*threshold)))
   // TODO: Tolto per usare gli eventi farocchi e non stare troppo a badare al timestamp
        |>ignore

    ///<summary>
    ///Restituisce un nuovo oggetto con il buffer tagliato a tot millisecondi
    ///</summary>
    ///<param name="millisec"> la porzione di tempo da tenere a partire dall'istante attuale, in millisecondi </param>
    ///<returns>un nuovo oggetto Buffereed2D</returns>    
    member this.cutBuffer(millisec:float):Buffered2D = 
            let newlist = listcut(itemlist,millisec)
            new Buffered2D(new List<TData2D> ( newlist))

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
            
    //        Console.WriteLine("lunghezza itemlist -> " + (Seq.length itemlist).ToString())
    //        Console.WriteLine("lunghezza datacp -> " + (Seq.length datacp).ToString())    
              
            if (Seq.length datacp >2)
                then
                    let mutable current = datacp.Head
                    let startingtime = current.Time
                    let lasttime = (datacp.Item(datacp.Length - 1)).Time
                    let mutable distance = 0.0
                    datacp <- datacp.Tail
                    while(not(datacp.IsEmpty))
                        do
                        distance <- Math.Sqrt(sqr(current.D1 - datacp.Head.D1)+sqr(current.D2 - datacp.Head.D2)) + distance
                        current <- datacp.Head
                        datacp <- datacp.Tail

                    let velocity = (distance / (lasttime - startingtime).TotalMilliseconds)*1000.0
                    velocity   
                else
                    0.0 // TODO : Decidere cosa fare x quando non ho dettagli


    // Sa di funzione inutile... un pò na cacata
    ///<summary>
    ///Da la direzione positiva o negativa se la lista di eventi a partire da un determinato timespan è consistente verso una direzione, 
    ///altrimenti restituisce direzione casuale
    ///</summary>
    ///<param name="timespan">rappresenta la dimensione di tempo per cui controllare (millisecondi) </param>
    ///<param name="tolleranza">tolleranza nelle possibilità di insuccesso, in caso di assenza non si prevede tolleranza</param>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.Direction ( timespan : float, ?tolleranza:float) = 
            let last       = itemlist.Item(itemlist.Count - 1)
            let newlist    = listcut (itemlist,timespan)
            
            let rightlist  = List.filter(fun x -> (x:TData2D).D1< last.D1 ) newlist
            let leftlist   = List.filter(fun x -> (x:TData2D).D1> last.D1 ) newlist
            let toplist    = List.filter(fun x -> (x:TData2D).D2< last.D2 ) newlist
            let bottomlist = List.filter(fun x -> (x:TData2D).D2> last.D2 ) newlist
     
            let mutable toll = 1.0
    
            match tolleranza with 
                | None -> toll<- 1.0
                | Some s -> toll <- s

            let minlength = toll * float newlist.Length 

            if ( (float rightlist.Length) > minlength) then
                
                    if ((float toplist.Length) > minlength) 
                        then Direction2D.TopRight
                    elif ( (float bottomlist.Length) > minlength) 
                        then Direction2D.BottomRight
                    else 
                        Direction2D.Right
            elif ( (float leftlist.Length) > minlength) then
                        
                        if ( (float toplist.Length) > minlength) 
                            then Direction2D.TopLeft
                        elif (( float bottomlist.Length) > minlength) 
                            then Direction2D.BottomLeft
                        else Direction2D.Left
            
            elif ( (float toplist.Length) > minlength) then Direction2D.Top
            elif ( (float bottomlist.Length) > minlength) then Direction2D.Bottom
            else Direction2D.Casual            


            
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
                            |> List.map(fun x -> (x:TData2D).D1)
                            |> List.average
                            )
                    let D2 = (
                            lista
                            |> List.map(fun x -> (x:TData2D).D2)
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
            let center = itemlist.Item(itemlist.Count-1)
            let newlist = listcut(itemlist,timespan)

            let result = Seq.forall(fun x -> (StaticPoint((x:TData2D).D1,center.D1,tolleranza)  &&
                                              StaticPoint((x:TData2D).D2,center.D2,tolleranza) 
                                                    )) newlist
            result

    ///<summary>
    /// fa il fitting alla retta, con la regressione lineare usando il metodo QR e restituisce 2 float
    /// L'equazione è Y = r1*X  + r0
    ///</summary>
    ///<return>float[] della costante * float[] per il coefficiente della X</returns>
    member this.FittingToLine():float[]*float[] =

            let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData2D).D1) itemlist)
            let ArrayY = Seq.toArray ( Seq.map(fun x -> (x:TData2D).D2) itemlist)
            let firsttime = itemlist.Item(0).Time
            let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData2D).Time , firsttime)) itemlist)

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
            let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData2D).D1) listacorta)
            let ArrayY = Seq.toArray ( Seq.map(fun x -> (x:TData2D).D2) listacorta)
            let firsttime = listacorta.Item(0).Time
            let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData2D).Time , firsttime)) listacorta)

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
        let arrayTimed = List.map (fun x -> new Direzione(timespanseconds((x:TData2D).Time , firsttime),(x:TData2D).D1,(x:TData2D).D2)) listacorta
 
        let result = 
            arrayTimed
            |> Seq.map (fun f -> Math.Sqrt( sqr(distanzaeuclidea(coeff.[0],vnoto.[0],f.D1,f.Time))+sqr(distanzaeuclidea(coeff.[1],vnoto.[1],f.D2,f.Time))))  // radice quadrata del quadrato delle distanze x dimensione
            |> Seq.sum 
        System.Console.WriteLine("Distanza eucidea totale = " + result.ToString() + " diviso le unità " + ( result /  float arrayTimed.Length ).ToString())
        if (result / float listacorta.Length) < tolleranza 
                                                        then true
                                                        else false 



type Buffered3D (?item:List<TData3D>, ?soglia:float) =
    inherit BufferedData<TData3D>()
    

    let threshold = match soglia with
                            | None -> 10000.0
                            | Some h -> h

    let itemlist = match item with
                            | None -> new List<TData3D>()
                            | Some h -> h

    member this.Count () = itemlist.Count
    
    member this.Clear () = 
            itemlist.Clear()

    override this.AddItem(d:TData3D) =
            itemlist.Add  (d)
            itemlist.RemoveAll(fun x -> (x.Time < System.DateTime.Now.AddMilliseconds(-1.0*threshold)))
            |>ignore

    ///<summary>
    ///Restituisce un nuovo oggetto con il buffer tagliato a tot millisecondi
    ///</summary>
    ///<param name="millisec"> la porzione di tempo da tenere a partire dall'istante attuale, in millisecondi </param>
    ///<returns>un nuovo oggetto Buffereed2D</returns>    
    member this.cutBuffer(millisec:float):Buffered3D = 
            let newlist = listcut(itemlist,millisec)
            new Buffered3D(new List<TData3D> ( newlist))


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
            
    //        Console.WriteLine("lunghezza itemlist -> " + (Seq.length itemlist).ToString())
    //        Console.WriteLine("lunghezza datacp -> " + (Seq.length datacp).ToString())    
              
            if (Seq.length datacp >2)
                then
                    let mutable current = datacp.Head
                    let startingtime = datacp.Head.Time
                    let lasttime = (datacp.Item(datacp.Length - 1)).Time
                    let mutable distance = 0.0
                    datacp <- datacp.Tail
                    while(not(datacp.IsEmpty))
                        do
                        distance <- Math.Sqrt(sqr(current.D1 - datacp.Head.D1) + sqr(current.D2 - datacp.Head.D2) + sqr(current.D3 - datacp.Head.D3)) + distance
                        current <- datacp.Head
                        datacp <- datacp.Tail

                    let velocity = (distance / (lasttime - startingtime).TotalMilliseconds)*1000.0
                    velocity   
                else
                    0.0 // TODO : Decidere cosa fare x quando non ho dettagli

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
                            |> List.map(fun x -> (x:TData3D).D1)
                            |> List.average
                            )
                    let D2 = (
                            lista
                            |> List.map(fun x -> (x:TData3D).D2)
                            |> List.average
                            )
                    let D3 = (
                            lista
                            |> List.map(fun x -> (x:TData3D).D3)
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
            let center = itemlist.Item(itemlist.Count-1)
            let newlist = listcut(itemlist,timespan)

            let centeredlist = List.forall(fun x -> (StaticPoint((x:TData3D).D1,center.D1,tolleranza)  &&
                                                     StaticPoint((x:TData3D).D2,center.D2,tolleranza)  &&
                                                     StaticPoint((x:TData3D).D3,center.D3,tolleranza) 
                                                    )) newlist

            centeredlist

    ///<summary>
    /// fa il fitting alla retta, con la regressione lineare usando il metodo QR e restituisce 2 float
    /// L'equazione è Y = r1*X  + r0
    ///</summary>
    ///<return>float[] della costante * float[] per il coefficiente della X</returns>
    member this.FittingToLine():float[]*float[] =

            let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData3D).D1) itemlist)
            let ArrayY = Seq.toArray ( Seq.map(fun x -> (x:TData3D).D2) itemlist)
            let ArrayZ = Seq.toArray ( Seq.map(fun x -> (x:TData3D).D3) itemlist)

            let firsttime = itemlist.Item(0).Time
            let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData3D).Time , firsttime)) itemlist)

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
            let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData3D).D1) listacorta)
            let ArrayY = Seq.toArray ( Seq.map(fun x -> (x:TData3D).D2) listacorta)
            let ArrayZ = Seq.toArray ( Seq.map(fun x -> (x:TData3D).D3) listacorta)

            let firsttime = listacorta.Item(0).Time
            let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData3D).Time , firsttime)) listacorta)

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
        let arrayTimed = List.map (fun x -> new Direzione(timespanseconds((x:TData3D).Time , firsttime),(x:TData3D).D1,(x:TData3D).D2,(x:TData3D).D3)) listacorta
 
        let result = 
            arrayTimed
            |> Seq.map (fun f -> Math.Sqrt( sqr(distanzaeuclidea(coeff.[0],vnoto.[0],f.D1,f.Time))+sqr(distanzaeuclidea(coeff.[1],vnoto.[1],f.D2,f.Time))+sqr(distanzaeuclidea(coeff.[2],vnoto.[2],f.D3,f.Time))))  // radice quadrata del quadrato delle distanze x dimensione
            |> Seq.sum 
        System.Console.WriteLine("Distanza eucidea totale = " + result.ToString() + " diviso le unità " + ( result /  float arrayTimed.Length ).ToString())
        if (result / float listacorta.Length) < tolleranza 
                                                        then true
                                                        else false 

