module MouseTest3

    open GestIT
    open GestIT.FSharp
    open GestIT.Utils
    open GestIT.IData
    open GestIT.Data
    open GestIT.TData
    open GestIT.Events
    open System.IO
    open System.IO.Compression
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open System.Diagnostics
    open Playback
       
    type Delegate = delegate of string -> unit

    // tipo per i T1D
    type Td1d(n:int) =
        inherit System.EventArgs()
        
        let data = System.DateTime.Now

        interface TData1D with 
                  member  x.D1 = float n
                  member  x.Time = data

    type Td2d(n1:int,n2:int) =
        inherit System.EventArgs()
        
        let data = System.DateTime.Now

        interface TData2D with 
                  member  x.D1 = float n1
                  member  x.D2 = float n2
                  member  x.Time = data

    type Td3d(n1:int,n2:int,n3:int) =
        inherit System.EventArgs()
        
        let data = System.DateTime.Now

        interface TData3D with 
                  member  x.D1 = float n1
                  member  x.D2 = float n2
                  member  x.D3 = float n3
                  member  x.Time = data


    type MouseFeatureTypes=
        | MouseDown = 0
        | MouseUp = 1
        | MouseMove = 2
        | MouseQuick = 3
        | MouseSlow = 4
        | MouseIdle = 5
        | MouseDiagonal = 6
        | MouseCustom = 7

    type TrayApplication () as this =
            inherit Form()

            let lbl = new Label()
            let btn = new Button()

            do
            
                this.MaximizeBox <- true
                this.Width <- 1000
                this.Height <- 600
                this.FormBorderStyle <- FormBorderStyle.FixedSingle
                lbl.Visible <- true
                lbl.Width <- 200
                lbl.Height <- 40
                this.BackColor <- Color.Azure
                lbl.Location <- new Point(this.Location.X + this.Width / 2 - lbl.Width / 2, this.Location.Y + this.Height / 2 - lbl.Height / 2)
                lbl.Font <- new Font("Verdana", 10.F)
                lbl.Text <- "* Simple Frame *"
                this.Controls.Add(lbl)


            member this.label = lbl

            override x.OnLoad(e:System.EventArgs) =
                x.Visible <- true
                x.ShowInTaskbar <- true
                base.OnLoad(e)


    //Main
    [<EntryPoint; System.STAThread>]
    let main argv = 
  
            let sens = new FusionSensor<MouseFeatureTypes,_>() 
            let app = new TrayApplication()
            let deleg = new Delegate(fun s -> app.label.Text <- s)
            
#if Record
            let recorder = new EvtRecorder(new EvtSettings("Recc"))
#endif

#if Reading
            let datareader = new DataCreator<TData1D>()
            datareader.choosefile()
            

#endif

            // Predicates
            let movemouse (x:MouseEventArgs) =
                        true

            let pushbutton (t:MouseButtons) (x:MouseEventArgs) =
                let button = x.Button
                if (button.Equals(MouseButtons.None))
                    then
                        false
                    elif
                        button.Equals(t)
                        then
                            true
                        else
                            false

            let mouseup (x:MouseEventArgs) = 
                if (x.Button.Equals(MouseButtons.None)) 
                    then 
                        true
                    else 
                        false

            // Ground term declaration
            let leftB   = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseDown, (pushbutton MouseButtons.Left)) 
            let rightB  = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseDown, (pushbutton MouseButtons.Right)) 
            let middleB = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseDown, (pushbutton MouseButtons.Middle)) 
            let moving  = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseMove, movemouse )
            let upButt  = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseUp,   mouseup)

            // Danno nuovo <---> Vedere che fare

            let fermo   = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseIdle,  fun x -> true)
            let veloce  = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseQuick, fun x -> true)
            let lento   = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseSlow,  fun x -> true)
            let diagonale = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseDiagonal, fun x -> true)
            let dritto = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseCustom, fun x -> true)
            let accmedia = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseCustom, fun x-> true)
            let fit1    = new GroundTerm<_,_>(MouseFeatureTypes.MouseCustom, fun x-> true) 

            // ref used for movement tracking
            let a = ref 0
            let b = ref 0
            // Handlers
            let moving_h (sender, f:MouseFeatureTypes, e:MouseEventArgs) =
                let moving =  (e.X <> !a && e.Y <> !b) 
                a:= e.X
                b:= e.Y
                                   
                if (moving) then
                    System.Diagnostics.Debug.WriteLine("Muovendo")
                    app.label.Invoke(deleg, "... ~~> Moving <~~ ...") |> ignore
                    app.label.BackColor <- Color.Lavender
                    app.label.Invalidate()

            let clickleft_h (sender, f:MouseFeatureTypes, e:MouseEventArgs) =
                System.Diagnostics.Debug.WriteLine("Click left")
                app.label.Invoke(deleg, "... ~~> LeftClick <~~ ...") |> ignore
                app.label.BackColor <- Color.Tomato
                app.label.Invalidate()

            let clickright_h (sender, f:MouseFeatureTypes, e:MouseEventArgs) =
                System.Diagnostics.Debug.WriteLine("Click right")
                app.label.Invoke(deleg, "... ~~> RightClick <~~ ...") |> ignore
                app.label.BackColor <- Color.Aquamarine
                app.label.Invalidate()
            
            let triple_h (sender, f:MouseFeatureTypes, e:MouseEventArgs) = 
                System.Diagnostics.Debug.WriteLine("Tripletta")
                app.label.Invoke(deleg, "... ~~> Tripletta <~~ ...") |> ignore
                app.label.BackColor <- Color.Gold
                app.label.Invalidate()

            let clickmiddle_h (sender, f:MouseFeatureTypes, e:MouseEventArgs) =
                System.Diagnostics.Debug.WriteLine("Click middle")
                app.label.Invoke(deleg, "... ~~> MiddleClick <~~ ...") |> ignore
                app.label.BackColor <- Color.Beige
                app.label.Invalidate()

            // Danno nuovo <---> Vedere che fare

            let fermo_h (sender, f:MouseFeatureTypes, e:_) =
             //   System.Diagnostics.Debug.WriteLine("Siamo fermi")
                app.label.Invoke(deleg, "... ~~> Siamo fermi <~~ ...") |> ignore
                app.label.BackColor <- Color.RoyalBlue
                app.label.Invalidate()

            let veloce_h (sender, f:MouseFeatureTypes, e:_) =
             //   System.Diagnostics.Debug.WriteLine("Siamo veloci")
                app.label.Invoke(deleg, "... ~~> Siamo veloci <~~ ...") |> ignore
                app.label.BackColor <- Color.Green
                app.label.Invalidate()

            let lento_h (sender, f:MouseFeatureTypes, e:_) =
             //   System.Diagnostics.Debug.WriteLine("Siamo lenti")
                app.label.Invoke(deleg, "... ~~> Siamo lenti <~~ ...") |> ignore
                app.label.BackColor <- Color.Yellow
                app.label.Invalidate()

            let diagonale_h (sender, f:MouseFeatureTypes, e:_) =
             //   System.Diagnostics.Debug.WriteLine("Siamo lenti")
                app.label.Invoke(deleg, "... ~~> Diagonale <~~ ...") |> ignore
                app.label.BackColor <- Color.Yellow
                app.label.Invalidate()

            let dritto_h(sender, f:MouseFeatureTypes, e:_)= 
             //   System.Diagnostics.Debug.WriteLine("Siamo lenti")
                app.label.Invoke(deleg, "... ~~> Movimento Retto <~~ ...") |> ignore
                app.label.BackColor <- Color.Yellow
                app.label.Invalidate()

            // Danno nuovo <---> Vedere che fare
            // lista predicati vari per le varie prove sui dati

//            let bb = new NumericData<Data1D>

            let buff = new Buffered1D()
//            let buff = new Acc1D()
          
            let avgmore(v:float) =  fun b -> (let bb = (b:Acc1D)
                                              System.Console.WriteLine("la media è --> " + (bb:>NumericData<Data1D>).StDev.D1.ToString())
                                              if ((bb:>NumericData<Data1D>).StDev.D1>v) then  
                                                                                         // (b:>Accumulator<Data1D>).Restart()
                                                                                            true
                                                                                          
                                                                                            else 
                                                                                            false
                                             )

            let retta(v:float ) = fun b -> (let bb = (b:Buffered1D)
                                            let d1,d2 = if(bb.Count()>20) then bb.FittingToLine(100000.0) 
                                                                          else 0.0,0.0
                                            //Trovare mega errore
                                            if (System.Math.Abs(d2 - v) < 1.0) 
                                                                                then 
                                                                                    System.Console.WriteLine("Giusto !! " + "direzione ---> " + d2.ToString() + "   val noto -->   " + d1.ToString() )
                                                                                    true
                                                                                else 
(*                                                                                    bb.GetListBuffer()
                                                                                    |>List.map( fun x -> (x:TData1D).D1)
                                                                                    |>List.iter( fun x -> System.Console.Write (x.ToString() + " "))
                                                                                    System.Console.WriteLine "||"
  *)                                                                                  
                                                                                    System.Console.WriteLine("Male !! " + "direzione ---> " + d2.ToString() + "   val noto -->   " + d1.ToString() )

                                                                                    if (bb.Count()>20) 
                                                                                        then 
                                                                                            ignore
                                                                                        else 
                                                                                            ignore

                                                                                    false  
                                           )

            let cane = ref 0
            let drittofun(time:float,toll:float) = fun b -> (let result = (b:Buffered1D).IsStraightDirection(time,toll)
                                                             if (result) then cane := (!cane + 1)
                                                                              System.Console.WriteLine("Evento Dritto" + (!cane).ToString() )
                                                             result
                                                             )

            let avgvel(v:float) = fun b -> (b:Buffered1D).AverageVelocity(1000.0) > v //velocità media dell'ultimo secondo + alta di
            let stationaryfunction = fun b -> (b:Buffered1D).StationaryPosition(50.0,50.0)
            let velfunction(v:float) = fun b -> System.Diagnostics.Debug.WriteLine("la velocità è :"+ (b:Buffered2D).InstantVelocity().ToString() )
                                                (b:Buffered2D).InstantVelocity() > v 
            
            let IdleEvt  = new TEvent<_,_>( stationaryfunction, true, "idle" )
            let QuickEvt = new TEvent<_,_>( avgvel 500.0, true, "quick")
            let SlowEvt  = new TEvent<_,_>( (fun b -> (not(avgvel(500.0) b) && avgvel(100.0) b ))  , true, "slow")
            let Fit1     = new TEvent<_,_> (retta(1.0), true, "retta")
            let Dritto   = new TEvent<_,_> (drittofun(500.0,30.0), true, "movimento dritto")
            let Sopramedia  = new TEvent<_,_> (avgmore(30.0),true,"superati i 600 di media")


            let evbuffer = new EventBuffer<_,_>(buff) 
            evbuffer.addEvent(IdleEvt)
       //     evbuffer.addEvent(SlowEvt)
       //     evbuffer.addEvent(QuickEvt)
            evbuffer.addEvent(Fit1)
       //     evbuffer.addEvent(Dritto)
       //     evbuffer.addEvent(Sopramedia)
#if Reading
            let handlingfun:(MouseEventArgs -> unit) = fun t -> t |> fun x -> System.Console.WriteLine("item inserito") |> ignore

#else
            let handlingfun:(MouseEventArgs -> unit) = fun t -> (new Td1d(t.X)
#if Record                                                       
                                                                 |> fun t -> (recorder.AddItem(t)
                                                                              System.Console.WriteLine("caaaaane")
                                                                              t)
                                                                 
#endif
                                                                 |> fun t -> evbuffer.AddItem (t)
                                                                                        )      

//            let handlingfun:MouseEventArgs -> unit = fun t -> ( let item = {new Data1D with member this.D1 = float t.X}
//            let handlingfun:(MouseEventArgs -> unit) = fun t -> (evbuffer.AddItem ( new Td1d(t.X)))
//            let handlingfun:(MouseEventArgs -> unit) = fun t -> (evbuffer.AddItem ( new Td2d(t.X,t.Y)))
//            let handlingfun:MouseEventArgs -> unit = fun t -> (evbuffer.AddItem ( new Td3d(t.X,t.Y,t.Clicks)))                                                     )

#endif

            app.MouseMove.Add(handlingfun)

        //    let dddd = Event.merge IdleEvt SlowEvt 
(*
            // Binding between features and IEvents
            s.Listen(MouseFeatureTypes.MouseMove, app.MouseMove)
            s.Listen(MouseFeatureTypes.MouseUp, app.MouseUp)
            s.Listen(MouseFeatureTypes.MouseDown, app.MouseDown)
*)
//            let evtCast<'T> e = Event.map (fun x -> x :> 'T) e
//           let finalEvt = Event.map (fun x -> x :> System.EventArgs) app.MouseDown
            (*
                           Event.merge IdleEvt.Publish QuickEvt.Publish
                           |> Event.merge SlowEvt.Publish
                           |> Event.map :> app.MouseDown
            *)
              
            sens.Listen(MouseFeatureTypes.MouseIdle, IdleEvt.Publish)
           // sens.Listen(MouseFeatureTypes.MouseQuick, QuickEvt.Publish)
           // sens.Listen(MouseFeatureTypes.MouseSlow, SlowEvt.Publish)
            sens.Listen(MouseFeatureTypes.MouseCustom, Fit1.Publish)
           // sens.Listen(MouseFeatureTypes.MouseCustom, Dritto.Publish)
           // sens.Listen(MouseFeatureTypes.MouseCustom, Sopramedia.Publish)
            
#if Record
            //sens.OutputStream <- openFileForZip()
#endif
 

            // GestIT Expression

            (*
            let events =  ( ( ( leftB |-> clickleft_h ) |>> ( middleB |-> clickmiddle_h ) |>> ( rightB |-> clickright_h ) ) |-> triple_h) |^| !*(moving |-> moving_h)
            events.ToGestureNet(s) |> ignore
            *)
            let events = !*((accmedia |-> diagonale_h) |^| (fit1 |-> dritto_h) |^| (fermo |-> fermo_h))
//            let events = !*((diagonale |-> diagonale_h) |^| (fermo |-> fermo_h))
//            let events = !*((dritto |-> dritto_h) |^| (fermo |-> fermo_h))
//            let events = !*(( fermo |-> fermo_h ) |^| ( veloce |-> veloce_h ) |^| ( lento |-> lento_h ))
            events.ToGestureNet(sens) |> ignore

#if Reading
            datareader.start(evbuffer)
#endif           

            Application.Run(app)
#if Record
            recorder.closeFile()
#endif
            0
