module Playback

open System
open System.Xml
open System.IO
open System.Runtime.Serialization.Formatters.Binary
open GestIT
open GestIT.IData
open GestIT.Events
open System.IO.Compression
open System.Windows.Forms
open System.Threading

type EvtSettings(filename:string,?zipfile:bool ) =

    member this.FileName = filename

    member this.zip = match zipfile with 
                        | None -> false
                        | Some t -> t



type EvtRecorder(setting:EvtSettings) = 
    let f:FileStream =  (setting.FileName + " " + System.DateTime.Now.ToString("HHmmss") + ".events")
                        |> fun t -> File.Open(t, FileMode.Create, FileAccess.Write)
                                            
    let formatter:BinaryFormatter = new BinaryFormatter()

    member this.AddItem(d:'T when 'T:>Data) = 
                formatter.Serialize(f, (System.DateTime.Now, d))

    member this.closeFile() = 
                f.Close()

type EvtCreator<'T,'U> when 'T : equality and 'U :> System.EventArgs ()= 
    
    let mutable f:FileStream = null
    let formatter:BinaryFormatter = new BinaryFormatter()
    let evts = new System.Collections.Generic.Dictionary<_,_>()
   
      /// <summary>
      /// Deserialize stream into a tuple of three elements.
      /// </summary>
    let readObj (s) =
        formatter.Deserialize(s) :?> (System.DateTime*'U)
    
    member private this.choosefile() = 
            let ofd = new OpenFileDialog()
            ofd.InitialDirectory <- Directory.GetCurrentDirectory()
            ofd.Filter <- "Event Files (*.events)|*.events"
            ofd.Multiselect <- false
            let userclicked = ofd.ShowDialog()
            if (userclicked = DialogResult.OK) 
                then
                   f <- File.Open(ofd.FileName,FileMode.Open)                
                else 
                   System.Console.WriteLine("Errore, file non trovato")
                   
            

    member this.start(buffer:'V when 'V:> BufferedData<_> )= 
          let worker = new Thread(fun () -> 
                       let (t, ev) = readObj(f)
                       let mutable lastTime = t

                       buffer.AddItem(ev)

                       let mutable eof = false
                       while not eof do
                        try
                          let (t, ev) = readObj(f)
                          let dt = (t - lastTime).TotalMilliseconds
                          lastTime <- t
                          if  dt > 5. then Thread.Sleep(int dt)
                          buffer.AddItem(ev)
                        with _ -> eof <- true
                       )
          worker.IsBackground <- true
          worker.Start()

  
    interface ISensor<'T,'U> with
                member this.Item 
                            with get f = 
                                 if not(evts.ContainsKey(f)) then evts.Add(f, new Event<'U>())
                                 evts.[f].Publish