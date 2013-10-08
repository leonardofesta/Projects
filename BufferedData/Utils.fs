module GestIT.Utils


open System
open GestIT.IData

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
let listcut (lista : List<'U> when 'U :> TData, timespan : float) =
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
        // Simple Least Squares Linear Regression, con pezzi tratti da:
        // http://christoph.ruegg.name/blog/2012/9/9/linear-regression-mathnet-numerics.html

        if (dependentD.Length > 1) then
            let X = DenseMatrix.ofColumns dependentD.Length 2 [ Array.init dependentD.Length (fun i -> 1.0); indipendentD ]
            let y = DenseVector dependentD
            let p = X.QR().Solve(y)
            (*
            printfn "X: %A" X
            printfn "y: %s" (y.ToString())
            printfn "p: %s" (p.ToString())
            *)
            // L'equazione è Y = p1*X  + p0
            (p.[0],p.[1])
        else
            // TODO : decidere cosa fare quando il risultato non è ricavabile (1 punto o 0 punti registrati nel buffer)
            (0.0,0.0)

// a coefficiente angolare
// b termine noto noto 
// c dimesnione 
// d timespan
let distanzaeuclidea(a:float,b:float,c:float,d:float):float =

// Dato y = ax+b  ->   | ax+b - c | distanza dalla retta

    let result =  Math.Abs (d  * a + b -  c)
//    System.Console.WriteLine(" coeff  = " + a.ToString() + " vnoto  = " + b.ToString() + " dim  = " + c.ToString() + " timespan  = " + d.ToString())
//    System.Console.WriteLine("risultato ->" + result.ToString())
    result
