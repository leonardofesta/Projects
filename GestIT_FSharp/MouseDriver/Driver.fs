namespace MouseDriver
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open System.IO
    open GestIT

   // MOUSE features that have to be notified from the sensor //
    type MouseFeatureTypes =
        | MouseDown = 0
        | MouseUp = 1
        | MouseMove = 2

    type MouseEventArgsGestIT(button:MouseButtons, clicks:int, x:int, y:int, delta:int, ?ts:System.DateTime) =
        inherit MouseEventArgs(button,clicks,x,y,delta)
        member this.Timestamp =
            match ts with
                | None -> System.DateTime.Now
                | Some t -> t
        
        interface GestIT.History.EventGestIT with 
            member x.getTimestamp() =
                x.Timestamp

        interface System.ICloneable with 
             member this.Clone() =
                let nuovo = new MouseEventArgsGestIT(this.Button,this.Clicks,this.X,this.Y,this.Delta,this.Timestamp)
                nuovo :> obj


    type MouseSensor (debug) =
        inherit UserControl()
        let mutable down = false
        let sensorEvent = new Event<SensorEventArgs<MouseFeatureTypes,MouseEventArgsGestIT>>()
        let formatter = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        let mutable outputStream : System.IO.Stream = null
        let debugSensor = false

        member this.OutputStream
            with get() = outputStream
            and set(v) = outputStream <- v

        interface GestIT.ISensor<MouseFeatureTypes,MouseEventArgsGestIT> with
            [<CLIEvent>]
            member x.SensorEvents = sensorEvent.Publish

        member private this.MyTrigger t e =
            if this.OutputStream <> null then formatter.Serialize(this.OutputStream, (System.DateTime.Now, t, e)) 
            sensorEvent.Trigger(new SensorEventArgs<_,_>(t, e))




        // Override dei metodi 
        override x.OnMouseDown(e) =
            if debug then printfn "MOUSE DOWN"
            let evargs = new MouseEventArgsGestIT(e.Button,e.Clicks,e.X,e.Y,e.Delta)
            sensorEvent.Trigger(new SensorEventArgs<_,_>(MouseFeatureTypes.MouseDown, evargs))
            down <- true
            x.MyTrigger MouseFeatureTypes.MouseDown evargs


        override x.OnMouseUp(e) =
            if debug then printfn "MOUSE UP"
            let evargs = new MouseEventArgsGestIT(e.Button,e.Clicks,e.X,e.Y,e.Delta)
            sensorEvent.Trigger(new SensorEventArgs<_,_>(MouseFeatureTypes.MouseUp, evargs))
            down <- false
            x.MyTrigger MouseFeatureTypes.MouseUp evargs



        override x.OnMouseMove(e) =
                if debug then printfn "MOUSE MOVE"
                let evargs = new MouseEventArgsGestIT(e.Button,e.Clicks,e.X,e.Y,e.Delta)
                sensorEvent.Trigger(new SensorEventArgs<_,_>(MouseFeatureTypes.MouseMove, evargs))
                x.MyTrigger MouseFeatureTypes.MouseMove evargs

    type EvtComparer() = 
        interface IComparer<MouseEventArgsGestIT> with
                member x.Compare(a, b) = (a.Timestamp).CompareTo(b.Timestamp)

