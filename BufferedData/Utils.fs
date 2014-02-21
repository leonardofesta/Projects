module BufferData.Utils

// In questo file ci sono funzioni varie di appoggio usate nel codice per evitare ripetizioni

open System
open BufferData.IData
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Distributions
open MathNet.Numerics.IntegralTransforms
open MathNet.Numerics.IntegralTransforms.Algorithms

/// <summary>
/// Taglia una lista di TData dato un certo timespan (in millisecondi) all'indietro
/// </summary>
/// <param name="lista">La lista di TData</param>
/// <param name="timespan">Il timespan, unità espressa in millisecondi e positiva  </param>
/// <returns>La lista tagliata</returns>
let listcut (lista : List<'U> when 'U :> TData<'T>, timespan : float) =
        if Seq.length lista >0
           then
           let lastitem = (lista.Item(Seq.length lista-1)).Time
           let length  = -1.0*timespan
           Seq.toList ( Seq.filter (fun t -> ( (t:'U).Time > lastitem.AddMilliseconds(length)  )) lista)
           else
           Seq.toList lista


/// <summary>
/// Fa il quadrato dell'intero
/// </summary>
let sqr(x:float):float = x*x    



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

let timespanmilliseconds(actual:DateTime, start:DateTime):float = 
        actual.Subtract(start).TotalMilliseconds



///<summary>
/// Fa la Regressione Lineare Semplice con il metodo QR usando 2 array di elementi una Dimensione dipendente e una indipendente <br/>
/// 
/// Ritorna una coppia che rappresenta l'equazione della retta  Y = a1 * X + a0 
///</summary>
///<returns>una coppia che rappresenta Y = a1 * X + a0 </returns>
let linearRegression(dependentD:float[], indipendentD:float[] ):(float*float) = 

        if (dependentD.Length > 1) then
            let X = DenseMatrix.ofColumns dependentD.Length 2 [ Array.init dependentD.Length (fun i -> 1.0); indipendentD ]
            let y = DenseVector dependentD
            let p = X.QR().Solve(y)
            (p.[0],p.[1])
        else
            (0.0,0.0)


///<summary>
/// Dato y = ax+b  ->   | ax+b - c | distanza dalla retta
///</summary>
/// <param name="coeff">coefficiente angolare della retta</param>
/// <param name="vnoto">termine noto della retta</param>
/// <param name="valore">valore della dimensione attuale (da confrontare con la variabile dipendente)</param>
/// <param name="timespan">timespan (variabile indipendente </param>
///<returns>una coppia che rappresenta Y = a1 * X + a0 </returns>
let disteuclidea(coeff:float,vnoto:float,valore:float,timespan:float):float =
    let result = Math.Abs (timespan  * coeff + vnoto -  valore)

    result

///<summary>
/// funzone di supporto per avere una lista di float stampati  in sequenza
///</summary>
let printfloat (mylist:list<float>):String = 
    let mutable stringa = ""
    for x in (List.map (fun x -> x.ToString()) mylist) do
        stringa <- stringa+" "+x

    stringa

///<summary>
/// funzione che data una lista e un intervallo verifica che non vi sia mai una distanza temporale superiore
/// <param name="mylist">Lista di TData da controllare</param>
/// <param name="interval">Intervallo massimo di tempo tra 2 istanze</param>
///</summary>
let continuity(mylist:list<'T> when 'T :> TData<_>, interval:float):bool =
            if (mylist.Length < 2)
                then 
                   false
                else
                   let a = List.rev ((List.rev mylist).Tail)
                   let secondlist = List.append ([(mylist.Head)]) a
                   let timediff = List.map2 (fun x y -> ((x:>TData<_>).Time.Subtract((y:>TData<_>).Time)).TotalMilliseconds
                                                        ) mylist secondlist
                   List.forall(fun x -> x<interval) timediff


///<summary>
/// Verifica la presenza di un dato nella lista
/// <param name="item">dato da cercare</param>
/// <param name="lista">Lista di TData da controllare</param>
///</summary>
let ispresent(item:'T, lista:list<'T> when 'T :> TData<_>):bool =
            if (lista.Length < 1) 
                then 
                    false
                else
                    if (((List.filter(fun (t:'T) -> item.Time.Equals(t.Time))) lista  ).Length = 0 ) 
                        then 
                            false
                        else 
                            true
