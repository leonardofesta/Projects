module MouseTest2

    open GestIT
    open GestIT.FSharp
    open GestIT.HistoryEngine
    open MouseDriver
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open System.Diagnostics
        
    type Delegate = delegate of string -> unit


    type TrayApplication (s:ISensor<MouseFeatureTypes,MouseEventArgsGestIT>) as this =
            inherit Form()

            let lbl = new Label()
            let btn = new Button()
            let printLabel (s:string) =
                lbl.Text <- s
            let printLabel1 = printLabel
            let deleg = new Delegate(printLabel1)

            (* Predicate *)
            let movemouse (x:MouseEventArgsGestIT) =
                let xpos = x.X
                let ypos = x.Y
                true

            let pushbutton (t:MouseButtons) (x:MouseEventArgsGestIT) =
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

            let mouseup (x:MouseEventArgsGestIT) = 
                true


            let leftB   = new GroundTerm<MouseFeatureTypes,MouseEventArgsGestIT>(MouseFeatureTypes.MouseDown, (pushbutton MouseButtons.Left)) 
            let rightB  = new GroundTerm<MouseFeatureTypes,MouseEventArgsGestIT>(MouseFeatureTypes.MouseDown, (pushbutton MouseButtons.Right)) 
            let middleB = new GroundTerm<MouseFeatureTypes,MouseEventArgsGestIT>(MouseFeatureTypes.MouseDown, (pushbutton MouseButtons.Middle)) 
            let moving  = new GroundTerm<MouseFeatureTypes,MouseEventArgsGestIT>(MouseFeatureTypes.MouseMove, movemouse )
            let upButt  = new GroundTerm<MouseFeatureTypes, MouseEventArgsGestIT>(MouseFeatureTypes.MouseUp, mouseup)

            (* Handler *)
            let moving_h (sender, e:SensorEventArgs<MouseFeatureTypes, MouseEventArgsGestIT>) =
//                System.Diagnostics.Debug.WriteLine("Muovendo")
                lbl.Invoke(deleg, "... ~~> Moving <~~ ...") |> ignore
                lbl.BackColor <- Color.Lavender
                lbl.Invalidate()

            let clickleft_h (sender, e:SensorEventArgs<MouseFeatureTypes, MouseEventArgsGestIT>) =
//                System.Diagnostics.Debug.WriteLine("Click left")
                lbl.Invoke(deleg, "... ~~> LeftClick <~~ ...") |> ignore
                lbl.BackColor <- Color.Tomato
                lbl.Invalidate()

            let clickright_h (sender, e:SensorEventArgs<MouseFeatureTypes, MouseEventArgsGestIT>) =
//                System.Diagnostics.Debug.WriteLine("Click right")
                lbl.Invoke(deleg, "... ~~> RightClick <~~ ...") |> ignore
                lbl.BackColor <- Color.Aquamarine
                lbl.Invalidate()
            
            let triple_h (sender, e:SensorEventArgs<MouseFeatureTypes, MouseEventArgsGestIT>) = 
//                System.Diagnostics.Debug.WriteLine("Tripletta")
                lbl.Invoke(deleg, "... ~~> Tripletta <~~ ...") |> ignore
                lbl.BackColor <- Color.Gold
                lbl.Invalidate()

            let clickmiddle_h (sender, e:SensorEventArgs<MouseFeatureTypes, MouseEventArgsGestIT>) =
//                System.Diagnostics.Debug.WriteLine("Click middle")
                lbl.Invoke(deleg, "... ~~> MiddleClick <~~ ...") |> ignore
                lbl.BackColor <- Color.Honeydew
                lbl.Invalidate()

            let UpdateInformations (f:MouseEventArgsGestIT, e:MouseFeatureTypes, t:System.DateTime) =
                     (* Update informations in the last enqueued frame *)
                System.Diagnostics.Debug.WriteLine("Update Information")
                match e with
                    | MouseFeatureTypes.MouseDown -> (); System.Diagnostics.Debug.WriteLine("mousedown") |> ignore 
                    | MouseFeatureTypes.MouseUp -> (); System.Diagnostics.Debug.WriteLine("mouseup") |> ignore
                    | MouseFeatureTypes.MouseMove -> (); System.Diagnostics.Debug.WriteLine("mousemove") |> ignore
                    | _ -> ()

            do
                
                
                let events =  ( ( ( leftB |-> clickleft_h ) |>> ( middleB |-> clickmiddle_h ) |>> ( rightB |-> clickright_h ) ) |-> triple_h) |^| !*(moving |-> moving_h)
                let he = new HistoryEngine<_,MouseFeatureTypes,MouseEventArgsGestIT>(events,s)
                let mutable d = he.AddFilter( (fun x -> true))
                d <- he.AddFilter( (fun (y,x) -> x.Button = MouseButtons.Left && x.Clicks>1 ) ) 
                d <- he.AddFilter( (fun (y,x) -> x.Button = MouseButtons.Middle && x.Clicks>1) ) 
                d <- he.AddFilter( (fun (y,x) -> x.Button = MouseButtons.Right && x.Clicks>1) )
               
                he.Run()


                this.MaximizeBox <- true
                this.Width <- 1000
                this.Height <- 600
                this.FormBorderStyle <- FormBorderStyle.FixedSingle
                lbl.Visible <- true
                lbl.Width <- 200
                lbl.Height <- 40
                this.BackColor <- Color.Blue
                lbl.Location <- new Point(this.Location.X + this.Width / 2 - lbl.Width / 2, this.Location.Y + this.Height / 2 - lbl.Height / 2)
                lbl.Font <- new Font("Verdana", 10.F)
                lbl.Text <- "* Frame Superfuffa! *"
                this.Controls.Add(lbl)
                this.Controls.Add(s :?> MouseSensor)

            override x.OnLoad(e:System.EventArgs) =
                x.Visible <- true
                x.ShowInTaskbar <- true
            
                s.SensorEvents.Add(fun e ->
                    (* Removing too old frames *)
                    let t = e.Event.Timestamp
                    (* Receiving updates from sensor *)

                    let f = e.Event :?> MouseEventArgsGestIT
                    let id = e.Event.Timestamp
                    UpdateInformations(f, e.FeatureType, id)
                 )
                base.OnLoad(e)

        [<EntryPoint; System.STAThread>]
        let main argv = 
            let mutable ss : ISensor<_,_> option = None
            let s = new MouseDriver.MouseSensor(true)
            ss <- Some(s :> ISensor<MouseFeatureTypes,MouseEventArgsGestIT>)
            match ss with
                | None -> 0
                | Some s -> let a = new TrayApplication(s)
                                
                            Application.Run(a)
                            0
