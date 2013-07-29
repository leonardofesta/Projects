module GestIT.TData

open System
open GestIT.Data
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Distributions
open MathNet.Numerics.IntegralTransforms
open MathNet.Numerics.IntegralTransforms.Algorithms


type TData = 
    inherit Data
    abstract member Time : DateTime
                with get

type TData1D =
    inherit TData
    abstract member D1 : float
                with get 

type TData2D =
    inherit TData
    abstract member D1 : float
                with get 
    abstract member D2 : float       
                with get

type TData3D =
    inherit TData
    abstract member D1 : float
                with get 
    abstract member D2 : float       
                with get
    abstract member D3 : float
                with get

/// <summary>
/// Taglia una lista di TData dato un certo timespan (in millisecondi) a partire dall'ultimo
/// </summary>
/// <param name="lista">La lista di TData</param>
/// <param name="timespan">Il timespan, unità espressa in millisecondi e positiva  </param>
/// <returns>La lista tagliata</returns>
let listcut (lista : List<'U> when 'U :> TData, timespan : float) =
        let length  = -1.0*timespan
        Seq.toList ( Seq.filter (fun t -> ( (t:'U).Time > System.DateTime.Now.AddMilliseconds(length)  )) lista)

/// <summary>
/// Fa il quadrato dell'intero
/// </summary>
let sqr(x:float):float = x*x    

type Direction1D =
        |  Positive = 1
        |  Negative = -1
        |  Casual = 0


/// <summary>
/// Verifica che il valore "point" sia vicino rispetto ad un centro "center", con una tolleranza "tol"
/// </summary>        
/// <param name="point">punto che viene controllato</param>
/// <param name="center">punto considerato come centro</param>
/// <param name="tol">tolleranza dal punto iniziale di controllo, un valore assoluto</param>
/// <returns>vero o falso</returns>
let StaticPoint (point:float, center:float, tol:float ):bool =
   
    if (point <= (center+tol) && point>= (center-tol)) 
        then true
        else false

///<summary>
///Calcola i secondi di differenza tra il 1° e il 2° dato(che ci si aspetta più vecchio) e ritorna i secondi totali di differenza, come float
///</summary>
///<returns>I secondi totali di differenza, come float</returns>
let timespanseconds(actual:DateTime, start:DateTime):float = 
        actual.Subtract(start).TotalSeconds

///<summary>
/// Fa la Regressione Lineare Semplice con il metodo QR usando 2 array di elementi una Dimensione dipendente e una indipendente <br/>
/// 
/// Ritorna una coppia che rappresenta l'equazione della retta  Y = a1 * X + a0 
///</summary>
///<returns>una coppia che rappresenta Y = a1 * X + a0 </returns>
let linearRegression(dependentD:float[], indipendentD:float[] ):(float*float) = 
        // Simple Least Squares Linear Regression, con pezzi tratti da:
        // http://christoph.ruegg.name/blog/2012/9/9/linear-regression-mathnet-numerics.html

        if (dependentD.Length > 1) then
            let X = DenseMatrix.ofColumns dependentD.Length 2 [ Array.init dependentD.Length (fun i -> 1.0); dependentD ]
            let y = DenseVector indipendentD
            let p = X.QR().Solve(y)
            (*
            printfn "X: %A" X
            printfn "y: %s" (y.ToString())
            printfn "p: %s" (p.ToString())
            *)
            // L'equazione è Y = p1*X  + p0
            (p.[0],p.[1])
        else
            // TODO : decidere cosa fare quando il risultato non è ricavabile (1 punto o 0 punti registrati nel buffer
            (0.0,0.0)



type Buffered1D (?item:List<TData1D>) =

    let itemlist = match item with
                            | None -> new List<TData1D>()
                            | Some h -> h
    
    member this.Count () = itemlist.Count    

    member this.Clear () = itemlist.RemoveAll(fun x -> true)
    
    member this.GetArrayBuffer() = Seq.toArray(itemlist)

    member this.GetListBuffer() = Seq.toList(itemlist)

    member this.AddItem(d:TData1D) =
            itemlist.Add  d
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
    ///Calcola la velocità istantanea degli ultimi 2 elementi
    ///</summary>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.InstantVelocity() = 

            if (itemlist.Count >2) 
                then
                    let last = itemlist.Item(itemlist.Count - 1)
                    let sndlast = itemlist.Item(itemlist.Count - 2) 
                    let velocity = (last.D1 - sndlast.D1)/ ( float (last.Time- sndlast.Time).Seconds)
                    velocity   
                else
                    0.0 // TODO : Decidere cosa fare x quando non ho dettagli

    
            
    // forse inutile
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
    ///Controlla se il punto è stazionario rispetto ad una certa differenza
    ///</summary>
    ///<param name="timespan">rappresenta la dimensione di tempo per cui controllare (millisecondi) </param>
    ///<param name="tolleranza">range di quanto può variare il valore dal punto centrale</param>
    ///<returns>vero o falso</returns>    
    member this.StationaryPosition(timespan : float, tolleranza:float) = 
            let center = itemlist.Item(itemlist.Count-1).D1
            let newlist = listcut(itemlist,timespan)
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
        let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData1D).Time , firsttime)) itemlist)
       
        linearRegression(ArrayX,arrayTime)
(*
        //si potrebbe fare con array di 1 così tutti i metodi tornano indietro l'array   
        let dim1x,dim2x  = linearRegression(ArrayX,arrayTime)    //  Y = dim2x * X + dim1x
        ([|dim1x|],[| dim2x|])
*)

    ///<summary>
    /// fa il fitting alla retta a partire da un certo timespan, con la regressione lineare usando il metodo QR e restituisce 2 float
    /// L'equazione è Y = r1*X  + r0
    ///</summary>
    ///<return>float x float</returns>
    member this.FittingToLine(timespan : float):float*float = 
        let listacorta = listcut (itemlist, timespan)
        
        let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData1D).D1) listacorta)
        let firsttime = listacorta.Item(0).Time
        let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData1D).Time , firsttime)) listacorta)

        linearRegression(ArrayX,arrayTime)
(*
        si potrebbe fare con array di 1 così tutti i metodi tornano indietro l'array   
        let dim1x,dim2x  = linearRegression(ArrayX,arrayTime)    //  Y = dim2x * X + dim1x
        ([|dim1x|],[| dim2x|])
*)  
    
    ///<summary>
    ///Fa la trasformata di fourier, usa come coefficiente radice n e per l'inversa è 1/(radice n )  
    ///</summary>
    member this.FFT() = 
        let d1buff = Array.map(fun x -> ( new Numerics.Complex((x:TData1D).D1,0.0 ))) (this.GetArrayBuffer())
        Transform.FourierForward(d1buff)
        d1buff |> Array.map(fun x -> x.Real)

    ///<summary>
    ///Fa la trasformata di fourier, applica un filtro passato, e l'inversa, restituendo un array di valori T1Data  
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


type Buffered2D (?item:List<TData2D>) =

    let itemlist = match item with
                            | None -> new List<TData2D>()
                            | Some h -> h

    member this.Count () = itemlist.Count
  
    member this.Clear () = 
            itemlist.RemoveAll(fun x -> true)

    member this.AddItem(d:TData2D) =
            itemlist.Add  d
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
    ///Calcola la velocità istantanea degli ultimi 2 elementi
    ///</summary>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.InstantVelocity() = 
            
            if (itemlist.Count >2) 
                then
                    let last = itemlist.Item(itemlist.Count - 1)
                    let sndlast = itemlist.Item(itemlist.Count - 2)
                    let velocity = Math.Sqrt( sqr(last.D1 - sndlast.D1) + sqr(last.D2 - sndlast.D2) )  / ( float (last.Time- sndlast.Time).Seconds)

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
       


type Buffered3D (?item:List<TData3D>) =

    let itemlist = match item with
                            | None -> new List<TData3D>()
                            | Some h -> h

    member this.Count () = itemlist.Count
    
    member this.Clear () = 
            itemlist.RemoveAll(fun x -> true)

    member this.AddItem(d:TData3D) =
            itemlist.Add  d
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
    ///Calcola la velocità istantanea degli ultimi 2 elementi
    ///</summary>
    ///<returns>la velocità istantanea oppure 0 se non ci sono 2 elementi necessari</returns>
    member this.InstantVelocity() = 
            
            if (itemlist.Count >2) 
                then
                let last = itemlist.Item(itemlist.Count - 1)
                let sndlast = itemlist.Item(itemlist.Count - 2) 
                let velocity = Math.Sqrt( sqr(last.D1 - sndlast.D1) + sqr(last.D2 - sndlast.D2) + sqr(last.D3 - sndlast.D3) )  / ( float (last.Time- sndlast.Time).Seconds)

                velocity   
            else
                0.0 // TODO : Decidere cosa fare x quando non ho dettagli

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
