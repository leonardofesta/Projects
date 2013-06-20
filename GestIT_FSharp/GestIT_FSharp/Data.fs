module GestIT.Data    

open System.Collections
open System

type Data =
    interface
    end

type Time = 
    abstract member Time : System.DateTime
                   with get

type Accumulator<'U> when 'U :> Data = 

    abstract member Additem :   'U -> unit





type NumericData<'U> when 'U :> Data = 

    abstract member Average :   'U 
                with get 
    abstract member Variance :  'U
                with get
    abstract member StDev : 'U
                with get


type Data1d = 
    inherit Data
    abstract member D1 : float
                with get 


type Data2d = 
    inherit Data
    abstract member D1 : float
                with get 
    abstract member D2 : float
                with get 
    
type Data3d =
    inherit Data
    abstract member D1 : float
                with get 
    abstract member D2 : float       
                with get
    abstract member D3 : float
                with get

let media (actual:float, items:int, value:float ):float =
                let n = float items
                ((actual*n + value )/(n+1.0))

let mx2 (actual: float,items:int,value:float):float = 
                let n = float items
                ((actual*n + value*value)/(n+1.0))

type Acc1D() = 
    
    let mutable avg = 0.0
    let mutable items = 0
    let mutable variance = -1.0
    let mutable x2 = 0.0 // sarebbe x^2
    let mutable stdev = -1.0
    

    interface Accumulator<Data1d> with

        member x.Additem( d ) =
            avg <- media(avg,items,d.D1)
            x2 <- mx2(x2,items,d.D1)
            items <- (items+1 )
            variance <- x2 - (avg * avg)
            stdev <- Math.Sqrt  variance

    interface NumericData<Data1d> with 
    

        member x.Average  
                with get () = { new Data1d with 
                                    member x.D1 =  avg } 
        member x.Variance
                with get () = { new Data1d with 
                                    member x.D1 =  variance }
        member x.StDev
                with get () =  { new Data1d with 
                                    member x.D1 =  stdev }
   
type Acc2D() = 
    
    let mutable avg1 = 0.0
    let mutable avg2 = 0.0

    let mutable items = 0

    let mutable variance1 = -1.0
    let mutable variance2 = -1.0

    let mutable x2_1 =0.0 // sarebbe x^2
    let mutable x2_2 =0.0 // sarebbe x^2
    
    let mutable stdev1 = -1.0
    let mutable stdev2 = -1.0
    

    interface Accumulator<Data2d> with 

        member x.Additem( d ) =
            avg1 <- media(avg1,items,d.D1)
            avg2 <- media(avg2,items,d.D2)
            x2_1 <- mx2(x2_1,items,d.D1)
            x2_2 <- mx2(x2_2,items,d.D2)
            items <- (items+1)
            variance1 <-  x2_1 - (avg1 * avg1)
            variance2 <-  x2_2 - (avg2 * avg2)
            stdev1 <- Math.Sqrt variance1
            stdev2 <- Math.Sqrt variance2

    interface NumericData<Data2d> with 

        member x.Average  
                with get () = { new Data2d with 
                                    member x.D1 = avg1 
                                    member x.D2 = avg2
                                    } 
        member x.Variance
                with get () = { new Data2d with 
                                    member x.D1 = variance1 
                                    member x.D2 = variance2
                                    } 

        member x.StDev
                with get () =  { new Data2d with 
                                    member x.D1 = stdev1 
                                    member x.D2 = stdev2
                                    } 

type Acc3D() = 
    
    let mutable avg1 = 0.0
    let mutable avg2 = 0.0
    let mutable avg3 = 0.0

    let mutable items = 0

    let mutable variance1 = -1.0
    let mutable variance2 = -1.0
    let mutable variance3 = -1.0

    let mutable x2_1 = 0.0 // sarebbe x^2
    let mutable x2_2 = 0.0 // sarebbe x^2
    let mutable x2_3 = 0.0 // sarebbe x^2

    let mutable stdev1 = -1.0
    let mutable stdev2 = -1.0
    let mutable stdev3 = -1.0



    interface Accumulator<Data3d> with 

        member x.Additem( d ) =
            avg1 <- media(avg1,items,d.D1)
            avg2 <- media(avg2,items,d.D2)
            avg3 <- media(avg3,items,d.D3)

            x2_1 <- mx2(x2_1,items,d.D1)
            x2_2 <- mx2(x2_2,items,d.D2)
            x2_3 <- mx2(x2_3,items,d.D3)

            items <- (items+1)

            variance1 <-  x2_1 - (avg1 * avg1)
            variance2 <-  x2_2 - (avg2 * avg2)
            variance2 <-  x2_3 - (avg3 * avg3)

            stdev1 <- Math.Sqrt variance1
            stdev2 <- Math.Sqrt variance2
            stdev3 <- Math.Sqrt variance3


    interface NumericData<Data3d> with 

        member x.Average  
                with get () = { new Data3d with 
                                    member x.D1 = avg1 
                                    member x.D2 = avg2
                                    member x.D3 = avg3
                                    } 
        member x.Variance
                with get () = { new Data3d with 
                                    member x.D1 = variance1 
                                    member x.D2 = variance2
                                    member x.D3 = variance3
                                    } 

        member x.StDev
                with get () =  { new Data3d with 
                                    member x.D1 = stdev1 
                                    member x.D2 = stdev2
                                    member x.D3 = stdev3
                                    } 



