module GestIT.Data    

open System
open System.Collections.Generic
open GestIT.IData
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Distributions



// media di x
let media (sum:float, items:int, value:float ):float =
                let n = float items
                ((sum + value )/(n+1.0))

// media degli x^2
let mx2 (actual: float,items:int,value:float):float = 
                let n = float items
                ((actual*n + value*value)/(n+1.0))



type Acc1D() = 
    inherit BufferedData<Data1D>()
    
    let mutable sum = 0.0
    let mutable avg = 0.0
    let mutable items = 0
    let mutable variance = -1.0
    let mutable x2 = 0.0 // sarebbe x^2
    let mutable stdev = -1.0
    
    override this.AddItem(d) = 
            avg <- media(sum,items,d.D1)
            sum <- sum + d.D1
            x2 <- mx2(x2,items,d.D1)
            items <- (items+1 )
            variance <- x2 - (avg * avg)
            stdev <- Math.Sqrt  variance


    interface Accumulator<Data1D> with

        member this.AddItem( d ) = 
            this.AddItem(d)
        
        member this.Restart() = 
            sum <- 0.0
            avg <- 0.0
            items <- 0
            variance <- -1.0
            x2 <- 0.0
            stdev <- -1.0

    interface NumericData<Data1D> with 

        member this.Sum 
                with get() =  { new Data1D with 
                                    member this.D1 = sum 
                                    } 
        member this.Average  
                with get () = { new Data1D with 
                                    member this.D1 =  avg } 
        member this.Variance
                with get () = { new Data1D with 
                                    member this.D1 =  variance }
        member this.StDev
                with get () =  { new Data1D with 
                                    member this.D1 =  stdev }
        

type Acc2D() = 
    inherit BufferedData<Data2D>()

    let mutable sum1 = 0.0
    let mutable sum2 = 0.0

    let mutable avg1 = 0.0
    let mutable avg2 = 0.0

    let mutable items = 0

    let mutable variance1 = -1.0
    let mutable variance2 = -1.0

    let mutable x2_1 =0.0 // sarebbe x^2
    let mutable x2_2 =0.0 // sarebbe x^2
    
    let mutable stdev1 = -1.0
    let mutable stdev2 = -1.0


    override x.AddItem( d ) =
            avg1 <- media(sum1,items,d.D1)
            avg2 <- media(sum2,items,d.D2)
            sum1 <- sum1 + d.D1
            sum2 <- sum2 + d.D2
            x2_1 <- mx2(x2_1,items,d.D1)
            x2_2 <- mx2(x2_2,items,d.D2)
            items <- (items+1)
            variance1 <-  x2_1 - (avg1 * avg1)
            variance2 <-  x2_2 - (avg2 * avg2)
            stdev1 <- Math.Sqrt variance1
            stdev2 <- Math.Sqrt variance2
    

    interface Accumulator<Data2D> with 

        member this.AddItem( d ) = 
            this.AddItem(d)

        member this.Restart() = 
            sum1 <- 0.0
            sum2 <- 0.0
            avg1 <- 0.0
            avg2 <- 0.0
            items <- 0
            variance1 <- -1.0
            variance2 <- -1.0
            x2_1 <- 0.0
            x2_2 <- 0.0
            stdev1 <- -1.0
            stdev2 <- -1.0

    interface NumericData<Data2D> with 

        member this.Sum 
                with get() =  { new Data2D with 
                                    member this.D1 = sum1 
                                    member this.D2 = sum2
                                    } 
        member this.Average  
                with get () = { new Data2D with 
                                    member this.D1 = avg1 
                                    member this.D2 = avg2
                                    } 
        member this.Variance
                with get () = { new Data2D with 
                                    member this.D1 = variance1 
                                    member this.D2 = variance2
                                    }
        member this.StDev
                with get () =  { new Data2D with 
                                    member this.D1 = stdev1 
                                    member this.D2 = stdev2
                                    } 

type Acc3D() = 
    inherit BufferedData<Data3D>()

    let mutable sum1 = 0.0
    let mutable sum2 = 0.0
    let mutable sum3 = 0.0

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

    

    override x.AddItem(d:Data3D) =
            
            avg1 <- media(sum1,items,d.D1)
            avg2 <- media(sum2,items,d.D2)
            avg3 <- media(sum3,items,d.D3)

            sum1 <- sum1 + d.D1
            sum2 <- sum2 + d.D2
            sum3 <- sum3 + d.D3

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


    interface Accumulator<Data3D> with 

        member this.AddItem( d ) = 
            this.AddItem(d)


        member this.Restart() = 
            sum1 <- 0.0
            sum2 <- 0.0
            sum3 <- 0.0
            avg1 <- 0.0
            avg2 <- 0.0
            avg3 <- 0.0
            items <- 0
            variance1 <- -1.0
            variance2 <- -1.0
            variance3 <- -1.0
            x2_1 <- 0.0
            x2_2 <- 0.0
            x2_3 <- 0.0
            stdev1 <- -1.0
            stdev2 <- -1.0
            stdev3 <- -1.0

    interface NumericData<Data3D> with 

        member x.Sum 
                with get() =  { new Data3D with 
                                    member x.D1 = sum1 
                                    member x.D2 = sum2
                                    member x.D3 = sum3
                                    } 
        member x.Average  
                with get () = { new Data3D with 
                                    member x.D1 = avg1 
                                    member x.D2 = avg2
                                    member x.D3 = avg3
                                    } 
        member x.Variance
                with get () = { new Data3D with 
                                    member x.D1 = variance1 
                                    member x.D2 = variance2
                                    member x.D3 = variance3
                                    } 
        member x.StDev
                with get () =  { new Data3D with 
                                    member x.D1 = stdev1 
                                    member x.D2 = stdev2
                                    member x.D3 = stdev3
                                    } 
