
open GestIT.TData
open GestIT.Data
open System

type Dato1d(v:float,t:DateTime) =
    interface TData1D with 
        member x.D1 = v
        member x.Time = t

type Dato2d(v:float,v2:float,t:DateTime) =
    interface TData2D with 
        member x.D1 = v
        member x.D2 = v2
        member x.Time = t

        
let timespanseconds(actual:DateTime, start:DateTime):float = 
        actual.Subtract(start).TotalSeconds


[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let now = DateTime.Now.AddDays(-1.0)
    let t = new Buffered1D() 
 //   t.AddItem( new Dato1d(0.0, now))
    t.AddItem( new Dato1d(1.0, now.AddSeconds(2.0) ))    
    t.AddItem( new Dato1d(3.0, now.AddSeconds(4.0) ))    
    t.AddItem( new Dato1d(5.0, now.AddSeconds(6.0) ))
    t.AddItem( new Dato1d(7.0, now.AddSeconds(8.0) ))
    t.AddItem( new Dato1d(9.0, now.AddSeconds(10.0)))
    t.AddItem( new Dato1d(11.0, now.AddSeconds(12.0)))
    t.AddItem( new Dato1d(13.0, now.AddSeconds(14.0)))
    t.AddItem( new Dato1d(15.0, now.AddSeconds(16.0)))
    t.AddItem( new Dato1d(17.0, now.AddSeconds(18.0)))
    t.AddItem( new Dato1d(19.0, now.AddSeconds(20.0)))
    t.AddItem( new Dato1d(21.0, now.AddSeconds(22.0)))
    let bla = t.FittingToLine()
    System.Console.WriteLine bla

    System.Console.WriteLine("-----------------------")
    
    let t2 = new Buffered2D() 
    t2.AddItem( new Dato2d(1.0 , 1.0 , now.AddSeconds(2.0) ))    
    t2.AddItem( new Dato2d(3.0 , 3.0 , now.AddSeconds(4.0) ))    
    t2.AddItem( new Dato2d(5.0 , 5.0 , now.AddSeconds(6.0) ))    
    t2.AddItem( new Dato2d(7.0 , 7.0 , now.AddSeconds(8.0) ))    
    t2.AddItem( new Dato2d(9.0 , 9.0 , now.AddSeconds(10.0) ))    
    t2.AddItem( new Dato2d(11.0 , 11.0 , now.AddSeconds(12.0) ))    
    t2.AddItem( new Dato2d(13.0 , 13.0 , now.AddSeconds(14.0) ))    
    t2.AddItem( new Dato2d(15.0 , 15.0 , now.AddSeconds(16.0) ))    
    t2.AddItem( new Dato2d(17.0 , 17.0 , now.AddSeconds(18.0) ))    
    t2.AddItem( new Dato2d(19.0 , 19.0 , now.AddSeconds(20.0) ))    


    let bla2 = t2.FittingToLine()
    System.Console.WriteLine bla2

    0 // restituisci un intero come codice di uscita
