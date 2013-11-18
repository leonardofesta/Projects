module MouseTest4

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
        | MouseFunzia = 8

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
//            let datareader = new DataCreator<TData2D>()

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


            // Danno nuovo <---> Vedere che fare

            let fermo   = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseIdle,  fun x -> true)
            let dritto = new GroundTerm<MouseFeatureTypes,_>(MouseFeatureTypes.MouseDiagonal, fun x -> true)
            let lineare = new GroundTerm<_,_>(MouseFeatureTypes.MouseCustom,fun x -> true)

            let straight = new GroundTerm<_,_>(MouseFeatureTypes.MouseCustom,fun x-> true)
            let funzionafunzione = new GroundTerm<_,_>(MouseFeatureTypes.MouseFunzia,fun x-> true)

            // Danno nuovo <---> Vedere che fare

            let fermo_h (sender, f:MouseFeatureTypes, e:_) =
             //   System.Diagnostics.Debug.WriteLine("Siamo fermi")
                app.label.Invoke(deleg, "... ~~> Siamo fermi <~~ ...") |> ignore
                app.label.BackColor <- Color.RoyalBlue
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

            let funzia_h(sender, f:MouseFeatureTypes, e:_)= 
                System.Diagnostics.Debug.WriteLine("Funziaaaaaaaaaaaaaa!")
                app.label.Invoke(deleg, "... ~~> Segue la funzione <~~ ..." + System.DateTime.Now.Millisecond.ToString()) |> ignore
                app.label.BackColor <- Color.Brown
                app.label.Invalidate()

            // Danno nuovo <---> Vedere che fare
            // lista predicati vari per le varie prove sui dati

//            let bb = new NumericData<Data1D>

            let buff = new Buffered2D()

            let tryfun () = fun b -> ( let bb = (b:Buffered1D)
                                       let dati = bb.GetListBuffer()
                                       let q1 = query { for d:TData1D in dati do select d.D1 }
                                        
                                       true
                                     ) 

            let drittofun(time:float,toll:float) = fun b -> (let bb = (b:Buffered2D)
                                                             if (bb.Count()>20) 
                                                                 then
                                                                    let result = bb.IsStraightDirection(time,toll)
                                                                    result
                                                                 else
                                                                    false
                                                             )
            let movimentoorizzontale (dist:float, time:float,toll:float ) = fun b -> (let bb= (b:Buffered2D)
                                                                                     if ((bb.Count()>20) && (bb.TotalDistance(time)>dist))
                                                                                            then
                                                                                            (*
                                                                                                let dati = bb.GetListBuffer()
                                                                                                let q1 = query { for d:TData2D in dati do select d.D1 }
                                                                                                System.Console.WriteLine("----")
                                                                                                Seq.iter (fun f -> System.Console.WriteLine( f.ToString())) q1
                                                                                                System.Console.WriteLine("....." + Seq.length( q1).ToString()  )
                                                                                            *) 
                                                                                                let result = bb.LinearMovement(time,toll)
                                                                                                result
                                                                                            else
                                                                                                false
                                                                                     )

            let stationaryfunction = fun b -> (b:Buffered2D).StationaryPosition(50.0,50.0)
            let ffollow = fun (x,y) -> (x:TimespanData).D1<(y:TimespanData).D1
            let funziafun  = fun b -> let valore = (b:Buffered2D).FollowingFunction((fun x -> new TimespanData(x,500.0,1000.0)),ffollow,2000.0)
                                      System.Console.WriteLine("blablablabla " + valore.ToString())
                                      valore >0.99

            let IdleEvt  = new TEvent<_,_>( stationaryfunction, true, "idle" )
            let Dritto   = new TEvent<_,_> (drittofun(500.0,10.0), true, "movimento dritto")
            let Lineare  = new TEvent<_,_> ( movimentoorizzontale(200.0,700.0,25.0),true,"movimento orizzontale")
            let Funzia   = new TEvent<_,_> (funziafun, true, "segue la funzione")
            let evbuffer = new EventBuffer<_,_>(buff) 
            evbuffer.addEvent(IdleEvt)
       //     evbuffer.addEvent(Dritto)
            evbuffer.addEvent(Lineare)
            evbuffer.addEvent(Funzia)

#if Reading
            let handlingfun:(MouseEventArgs -> unit) = fun t -> t 
                                                               // |> fun x -> System.Console.WriteLine("item inserito") 
                                                                |> ignore

#else

            let handlingfun:(MouseEventArgs -> unit) = fun t -> (new Td2d(t.X,t.Y)
#if Record                                                       
                                                                 |> fun t -> (recorder.AddItem(t)
                                                                              System.Console.WriteLine("caaaaane")
                                                                              t)
                                                                 
#endif
                                                                 |> fun t -> evbuffer.AddItem (t)
                                                                                        )      

//            let handlingfun:MouseEventArgs -> unit = fun t -> ( let item = {new Data1D with member this.D1 = float t.X}
//            let handlingfun:(MouseEventArgs -> unit) = fun t -> (evbuffer.AddItem ( new Td1d(t.X)))
//            let handlingfun:MouseEventArgs -> unit = fun t -> (evbuffer.AddItem ( new Td3d(t.X,t.Y,t.Clicks)))                                                     )

#endif

            app.MouseMove.Add(handlingfun)

        //    let dddd = Event.merge IdleEvt SlowEvt 

//            let evtCast<'T> e = Event.map (fun x -> x :> 'T) e
//           let finalEvt = Event.map (fun x -> x :> System.EventArgs) app.MouseDown
           
            sens.Listen(MouseFeatureTypes.MouseIdle, IdleEvt.Publish)
           // sens.Listen(MouseFeatureTypes.MouseCustom, Dritto.Publish)
            sens.Listen(MouseFeatureTypes.MouseCustom, Lineare.Publish)            
            sens.Listen(MouseFeatureTypes.MouseFunzia,Funzia.Publish)
#if Record
            //sens.OutputStream <- openFileForZip()
#endif
 

            // GestIT Expression
//            (fermo |-> fermo_h )|^| (lineare |-> dritto_h) |^| 
            let events = !*( (funzionafunzione |-> funzia_h ))
            events.ToGestureNet(sens) |> ignore

#if Reading
            datareader.start(evbuffer)
#endif           

            Application.Run(app)
#if Record
            recorder.closeFile()
#endif
            0


