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
open System.Collections.Generic

    [<AbstractClassAttribute>]
    type Td () = 
        inherit System.EventArgs() 
        abstract member printfn : unit -> String


    type Td1d(n:float,t:DateTime) =
        inherit Td()
        
        interface TData1D with 
                  member  this.D1 =  n
                  member  this.Time = t
        
        override this.printfn() = 
                n.ToString()

    type Td2d(n1:float,n2:float,t:DateTime) =
        inherit Td()
        
        interface TData2D with 
                  member  x.D1 =  n1
                  member  x.D2 =  n2
                  member  x.Time = t

        override this.printfn() = 
                n1.ToString() + " " + n2.ToString() 


    type Td3d(n1:float,n2:float,n3:float,t:DateTime) =
        inherit Td()
        
        interface TData3D with 
                  member  x.D1 =  n1
                  member  x.D2 =  n2
                  member  x.D3 =  n3
                  member  x.Time = t

        override this.printfn() = 
                n1.ToString() + " " + n2.ToString() + " " + n3.ToString()


type MyParser<'U when 'U :> TData> () =
  member x.myparse (s:String):'U =
    let values = s.Split [|' '|]
    let tvalue = Array.map(fun t -> int(t)) [|values.[0];values.[1];values.[2];values.[3]|]
    let dvalue = values.[4].Split[|','|] |> Array.map Double.Parse
    let n = DateTime.Now
    let time = new DateTime(n.Year,n.Month,n.Day,tvalue.[0],tvalue.[1],tvalue.[2],tvalue.[3])

    let data = match dvalue.Length with
                | 1 -> new Td1d(dvalue.[0],time) :> TData
                | 2 -> new Td2d(dvalue.[0],dvalue.[1],time) :> TData
                | 3 -> new Td3d(dvalue.[0],dvalue.[1],dvalue.[2],time) :> TData
                | _ -> failwith "Illegal type"
    
    data :?> 'U
 
type EvtSettings(filename:string,?zipfile:bool ) =

    member this.FileName = filename

    member this.zip = match zipfile with 
                        | None -> false
                        | Some t -> t



type EvtRecorder(setting:EvtSettings) = 
    let f:FileStream =  (setting.FileName + " " + System.DateTime.Now.ToString("HHmmss") + ".events")
                        |> fun t -> File.Open(t, FileMode.Create, FileAccess.Write)
    
    let f2:FileStream =  (setting.FileName + " " + System.DateTime.Now.ToString("HHmmss") + " Stringa.events")
                        |> fun t -> File.Open(t, FileMode.Create, FileAccess.Write)
    
                                            
    let formatter:BinaryFormatter = new BinaryFormatter()
    let r:StreamWriter = new StreamWriter(f2)

    member this.AddItem(d:'T when 'T:>Data) = 
                formatter.Serialize(f, (System.DateTime.Now, d))
                r.WriteLine(System.DateTime.Now.ToString("HH mm ss fff") + " , " +  d.ToString() )
                r.Flush()

    member this.closeFile() = 
                f.Close()
                r.Close()
        


type DataCreator<'U>  when 'U :> TData ()= 
    
    let lista = new List<'U>()

    let mutable f:FileStream  = null
 
    member this.choosefile() = 
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
            let str:StreamReader = new StreamReader(f)
            let p = new MyParser<'U>()
            while(not(str.EndOfStream))            
                do
                let linea  = str.ReadLine()
                let value  = p.myparse(linea)
                lista.Add(value)
            

 
    member this.start(buffer:'V when 'V:> EventBuffer<_,_> )= 
          let worker = new Thread(fun () -> 
                       let mutable l = Seq.toList lista
                       let ev = l.Head
                       l <- l.Tail

                       let mutable lastTime = ev.Time

                       buffer.AddItem(ev)

                       let mutable eof = false
                       while not eof do
                        try
                          let ev = l.Head
                          l <- l.Tail
                          let dt = (ev.Time - lastTime).TotalMilliseconds
                          lastTime <- ev.Time
                          if  dt > 5. then Thread.Sleep(int dt)
                          buffer.AddItem(ev)
                        with _ -> eof <- true
                       )
          worker.IsBackground <- true
          worker.Start()

  

type EvtPlayer<'T,'U> when 'T : equality and 'U :> System.EventArgs ()= 
    
    let mutable f:FileStream = null
    let formatter:BinaryFormatter = new BinaryFormatter()
    let evts = new System.Collections.Generic.Dictionary<_,_>()
   
      /// <summary>
      /// Deserialize stream into a tuple of three elements.
      /// </summary>
    let readObj (s) =
        formatter.Deserialize(s) :?> (System.DateTime*'U)
    
    member this.choosefile() = 
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


type SimulationEventWriter<'T when 'T :> Td>(filename:string, msinterval:int, quantity:int, startingvalue:'T, generationfun:'T->'T )  =

    let f:FileStream =  (filename + " " + System.DateTime.Now.ToString("HHmmss") + ".events")
                        |> fun t -> File.Open(t, FileMode.Create, FileAccess.Write)

    let writer:StreamWriter = new StreamWriter(f)
    
    let time = System.DateTime.Now.AddHours(-1.0)

    let mutable currentvalue = startingvalue
    do
      for i in 1 .. quantity do
        writer.WriteLine( time.AddMilliseconds(float(i * msinterval)).ToString("HH mm ss fff ") +  currentvalue.printfn())
        currentvalue <- generationfun(currentvalue)
    
      writer.Flush()
      writer.Close()
      f.Close()