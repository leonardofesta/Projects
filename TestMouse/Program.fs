// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.

module MouseTest

    open GestIT
    open GestIT.FSharp
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open System.Diagnostics
        
    type Delegate = delegate of string -> unit

    // MOUSE features that have to be notified from the sensor //
    type MouseFeatureTypes=
        | MouseDown = 0
        | MouseUp = 1
        | MouseMove = 2


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
  
            let s = new FusionSensor<MouseFeatureTypes,MouseEventArgs>()  
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
            let leftB   = new GroundTerm<MouseFeatureTypes,MouseEventArgs>(MouseFeatureTypes.MouseDown, (pushbutton MouseButtons.Left)) 
            let rightB  = new GroundTerm<MouseFeatureTypes,MouseEventArgs>(MouseFeatureTypes.MouseDown, (pushbutton MouseButtons.Right)) 
            let middleB = new GroundTerm<MouseFeatureTypes,MouseEventArgs>(MouseFeatureTypes.MouseDown, (pushbutton MouseButtons.Middle)) 
            let moving  = new GroundTerm<MouseFeatureTypes,MouseEventArgs>(MouseFeatureTypes.MouseMove, movemouse )
            let upButt  = new GroundTerm<MouseFeatureTypes,MouseEventArgs>(MouseFeatureTypes.MouseUp, mouseup)


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
                app.label.BackColor <- Color.Honeydew
                app.label.Invalidate()
       
            // Binding between features and IEvents
            s.Listen(MouseFeatureTypes.MouseMove, app.MouseMove)
            s.Listen(MouseFeatureTypes.MouseUp, app.MouseUp)
            s.Listen(MouseFeatureTypes.MouseDown, app.MouseDown)

            // GestIT Expression
            let events =  ( ( ( leftB |-> clickleft_h ) |>> ( middleB |-> clickmiddle_h ) |>> ( rightB |-> clickright_h ) ) |-> triple_h) |^| !*(moving |-> moving_h)
            events.ToGestureNet(s) |> ignore
            

            Application.Run(app)

            0