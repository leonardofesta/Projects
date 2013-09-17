﻿// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.

module MouseTest2

    open GestIT
    open GestIT.FSharp
    open GestIT.TData
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open System.Diagnostics
        
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


    // MOUSE features that have to be notified from the sensor //
(*    type MouseFeatureTypes=
        | MouseDown = 0
        | MouseUp = 1
        | MouseMove = 2
*)
    type MouseFeatureTypes=
        | MouseDown = 0
        | MouseUp = 1
        | MouseMove = 2
        | MouseQuick = 3
        | MouseSlow = 4
        | MouseIdle = 5


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
            let s2 = new FusionSensor<_,_>() 
            let app = new TrayApplication()
            let deleg = new Delegate(fun s -> app.label.Text <- s)
            
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

            
           
            // Danno nuovo <---> Vedere che fare

            let buff = new Buffered2D()
            let stationaryfunction = fun b -> (b:Buffered2D).StationaryPosition(50.0,50.0)
            let velfunction(v:float) = fun b -> System.Diagnostics.Debug.WriteLine("la velocità è :"+ (b:Buffered2D).InstantVelocity().ToString() )
                                                (b:Buffered2D).InstantVelocity() > v 
            let IdleEvt  = new TEvent<_,_>( stationaryfunction, true, "idle" )
            let QuickEvt = new TEvent<_,_>( velfunction 500.0, true, "quick")
            let SlowEvt  = new TEvent<_,_>( (fun b -> ((b:Buffered2D).InstantVelocity() > 100.0 && (b:Buffered2D).InstantVelocity() < 500.0 ))  , true, "slow")

            let coso = new Wrappone<_,_>(buff)
            coso.addEvent(IdleEvt)
            coso.addEvent(SlowEvt)
            coso.addEvent(QuickEvt)
     
//            let handlingfun:MouseEventArgs -> unit = fun t -> (coso.AddItem ( new Td1d(t.X)) 
            let handlingfun:MouseEventArgs -> unit = fun t -> (coso.AddItem ( new Td2d(t.X,t.Y)) 
                                                              )
            app.MouseMove.Add(handlingfun)

        //    let dddd = Event.merge IdleEvt SlowEvt 
(*
            // Binding between features and IEvents
            s.Listen(MouseFeatureTypes.MouseMove, app.MouseMove)
            s.Listen(MouseFeatureTypes.MouseUp, app.MouseUp)
            s.Listen(MouseFeatureTypes.MouseDown, app.MouseDown)
            *)
//            let evtCast<'T> e = Event.map (fun x -> x :> 'T) e
            let finalEvt = Event.map (fun x -> x :> System.EventArgs) app.MouseDown
            (*
                           Event.merge IdleEvt.Publish QuickEvt.Publish
                           |> Event.merge SlowEvt.Publish
                           |> Event.map :> app.MouseDown
                           *)
                                     
            // Danno nuovo <---> Vedere che fare
            sens.Listen(MouseFeatureTypes.MouseIdle, IdleEvt.Publish)
            sens.Listen(MouseFeatureTypes.MouseQuick, QuickEvt.Publish)
            sens.Listen(MouseFeatureTypes.MouseSlow, SlowEvt.Publish)

            // GestIT Expression

            (*
            let events =  ( ( ( leftB |-> clickleft_h ) |>> ( middleB |-> clickmiddle_h ) |>> ( rightB |-> clickright_h ) ) |-> triple_h) |^| !*(moving |-> moving_h)
            events.ToGestureNet(s) |> ignore
            *)

            let events = !*(( fermo |-> fermo_h ) |^| ( veloce |-> veloce_h ) |^| ( lento |-> lento_h ))
            events.ToGestureNet(sens) |> ignore
            

            Application.Run(app)

            0
