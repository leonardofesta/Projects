module GestIT.TData

open System
open GestIT.Data
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Distributions

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

// taglia la lista dopo un tot di millisecondi
let listcut (lista : List<'U> when 'U :> TData, timespan : int) =
        let length  = float (-1*timespan)
        let last = lista.Item(lista.Count - 1)
        Seq.toList ( Seq.filter (fun t -> ( (t:'U).Time > last.Time.AddMilliseconds(length)  )) lista)

// quadrato
let sqr(x:float):float = x*x    

type Direction1D =
        |  Positive = 1
        |  Negative = -1
        |  Casual = 0

type Tolerance = 
        |   None = 10
        |   Light =  9
        |   Medium = 8
        |   Heavy  = 5

        
// verifica che il valore "point" sia vicino rispetto ad un centro "center", con una tolleranza "tol"
let StaticPoint (point:float, center:float, tol:float ) =
   
    if (point <= (center+tol) && point>= (center-tol)) 
    then true
    else false

// calcola i secondi di differenza tra il 1° e il 2° dato, restituendoli in secondi come float
let timespanseconds(actual:DateTime, start:DateTime):float = 
        actual.Subtract(start).TotalSeconds

// Fa la Regressione Lineare Semplice con il metodo QR usando 2 array di elementi una Dimensione dipendente e una indipendente 
// e ritorna una coppia che rappresenta l'equazione della retta  Y = a1 * X + a0 
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



type Buffered1D () =
    
    let itemlist = new  List<TData1D>()

    member this.Count () = itemlist.Count    

    member this.Clear () = 
            itemlist.RemoveAll(fun x -> true)

    member this.AddItem(d:TData1D) =
            itemlist.Add  d
            |>ignore

    member this.InstantVelocity() = 
            
            if (itemlist.Count >2) 
                then
                    let last = itemlist.Item(itemlist.Count - 1)
                    let sndlast = itemlist.Item(itemlist.Count - 2) 
                
                    let velocity = (last.D1 - sndlast.D1)/ ( float (last.Time- sndlast.Time).Seconds)
                    velocity   
                else
                    0.0 // TODO : Decidere cosa fare x quando non ho dettagli

    member this.Direction ( timespan : int, ?tolleranza:Tolerance) = 
            let last = itemlist.Item(itemlist.Count - 1)
            let newlist = listcut (itemlist,timespan)
            
            let positivelist = List.filter(fun x -> (x:TData1D).D1<= last.D1 ) newlist
            let negativelist = List.filter(fun x -> (x:TData1D).D1>= last.D1 ) newlist
            let mutable toll = 10
            
            match tolleranza with 
                | None -> toll<- 10
                | Some s -> toll <- int s

            if (positivelist.Length > (toll*newlist.Length/10))
                then Direction1D.Positive
                elif (positivelist.Length > (toll*newlist.Length/10))
                then Direction1D.Negative
                else Direction1D.Casual
                        
            
    member this.StationaryPosition(timespan : int, tolleranza:float) = 
            let center = itemlist.Item(itemlist.Count-1).D1
            let newlist = listcut(itemlist,timespan)

            let result = List.forall(fun x -> StaticPoint((x:TData1D).D1,center,tolleranza  )) newlist

            result


    // fa il fitting alla retta, con la regressione lineare usando il metodo QR e restituisce 2 float
    // L'equazione è Y = r1*X  + r0
    
    member this.FittingToLine():float*float =
         
        let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData1D).D1) itemlist)
        let firsttime = itemlist.Item(0).Time
        let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData1D).Time , firsttime)) itemlist)

        linearRegression(ArrayX,arrayTime)

//        let dim1x,dim2x  = linearRegression(ArrayX,arrayTime)    //  Y = dim2x * X + dim1x
//        ([|dim1x|],[| dim2x|])
//        si potrebbe fare con array di 1 così tutti i metodi tornano indietro l'array   

       

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

type Buffered2D () =
    
    let itemlist = new  List<TData2D>()

    member this.Count () = itemlist.Count
    
  
    member this.Clear () = 
            itemlist.RemoveAll(fun x -> true)



    member this.AddItem(d:TData2D) =
            itemlist.Add  d
            |>ignore

    member this.InstantVelocity() = 
            
            if (itemlist.Count >2) 
                then
                    let last = itemlist.Item(itemlist.Count - 1)
                    let sndlast = itemlist.Item(itemlist.Count - 2) 
                
                    let velocity = Math.Sqrt( sqr(last.D1 - sndlast.D1) + sqr(last.D2 - sndlast.D2) )  / ( float (last.Time- sndlast.Time).Seconds)
                    velocity   
                else
                    0.0 // TODO : Decidere cosa fare x quando non ho dettagli

    member this.Direction ( timespan : int, ?tolleranza:Tolerance) = 
            let last = itemlist.Item(itemlist.Count - 1)
            let newlist = listcut (itemlist,timespan)
            
            let rightlist = List.filter(fun x -> (x:TData2D).D1< last.D1 ) newlist
            let leftlist = List.filter(fun x -> (x:TData2D).D1> last.D1 ) newlist
            let toplist = List.filter(fun x -> (x:TData2D).D2< last.D2 ) newlist
            let bottomlist = List.filter(fun x -> (x:TData2D).D2> last.D2 ) newlist
            
            let mutable toll = 10
            
            match tolleranza with 
                | None -> toll<- 10
                | Some s -> toll <- int s

            if (rightlist.Length > (toll*newlist.Length/10)) then
                if (toplist.Length > (toll*newlist.Length/10)) 
                        then Direction2D.TopRight
                    elif (bottomlist.Length > (toll*newlist.Length/10)) 
                        then Direction2D.BottomRight
                    else 
                        Direction2D.Right
            elif (leftlist.Length > (toll*newlist.Length/10)) then
                        if (toplist.Length > (toll*newlist.Length/10)) 
                            then Direction2D.TopLeft
                        elif (bottomlist.Length > (toll*newlist.Length/10)) 
                            then Direction2D.BottomLeft
                        else Direction2D.Left
            elif (toplist.Length > (toll*newlist.Length/10)) then Direction2D.Top
            elif (bottomlist.Length > (toll*newlist.Length/10)) then Direction2D.Bottom
            else Direction2D.Casual            
            

    member this.StationaryPosition(timespan : int, tolleranza:float) = 
            let center = itemlist.Item(itemlist.Count-1)
            let newlist = listcut(itemlist,timespan)

            let result = Seq.forall(fun x -> (StaticPoint((x:TData2D).D1,center.D1,tolleranza)  &&
                                                     StaticPoint((x:TData2D).D2,center.D2,tolleranza) 
                                                    )) newlist

            result

// Fa la Regressione Lineare Semplice con il metodo QR usando 2 array di elementi una Dimensione dipendente e una indipendente 
// e ritorna una coppia che rappresenta l'equazione della retta  Y = a1 * X + a0 
    member this.FittingToLine():float[]*float[] =

            let ArrayX = Seq.toArray ( Seq.map(fun x -> (x:TData2D).D1) itemlist)
            let ArrayY = Seq.toArray ( Seq.map(fun x -> (x:TData2D).D2) itemlist)
            let firsttime = itemlist.Item(0).Time
            let arrayTime = Seq.toArray ( Seq.map(fun x -> timespanseconds((x:TData1D).Time , firsttime)) itemlist)

            let dim1x,dim2x  = linearRegression(ArrayX,arrayTime)    //  Y = dim2x * X + dim1x
            let dim1y,dim2y  = linearRegression(ArrayY,arrayTime)    //  Y = dim2x * X + dim1x

            ([|dim1x ; dim1y|],[| dim2x; dim2y|])
       


type Buffered3D () =
    
    let itemlist = new  List<TData3D>()

    member this.Count () = itemlist.Count
    
    member this.Clear () = 
            itemlist.RemoveAll(fun x -> true)


    member this.AddItem(d:TData3D) =
            itemlist.Add  d
            |>ignore

    member this.InstantVelocity() = 
            
            if (itemlist.Count >2) 
                then
                let last = itemlist.Item(itemlist.Count - 1)
                let sndlast = itemlist.Item(itemlist.Count - 2) 
                
                let velocity = Math.Sqrt( sqr(last.D1 - sndlast.D1) + sqr(last.D2 - sndlast.D2) + sqr(last.D3 - sndlast.D3) )  / ( float (last.Time- sndlast.Time).Seconds)
                velocity   
            else
                0.0 // TODO : Decidere cosa fare x quando non ho dettagli
   
    member this.StationaryPosition(timespan : int, tolleranza:float) = 
            let center = itemlist.Item(itemlist.Count-1)
            let newlist = listcut(itemlist,timespan)

            let centeredlist = List.forall(fun x -> (StaticPoint((x:TData3D).D1,center.D1,tolleranza)  &&
                                                     StaticPoint((x:TData3D).D2,center.D2,tolleranza)  &&
                                                     StaticPoint((x:TData3D).D3,center.D3,tolleranza) 
                                                    )) newlist

            centeredlist

// Fa la Regressione Lineare Semplice con il metodo QR usando 2 array di elementi una Dimensione dipendente e una indipendente 
// e ritorna una coppia che rappresenta l'equazione della retta  Y = a1 * X + a0 
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
