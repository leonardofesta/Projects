module Eventwriter

    open GestIT.IData
    open Playback
    
    (*
    //Main
    [<EntryPoint; System.STAThread>]
    let main argv = 
        let f1d = fun t -> new Td1d((t:TData1D).D1 + 1.0, (t:TData1D).Time)
        let f2d = fun t -> new Td2d((t:TData2D).D1 + 1.0, (t:TData2D).D2 + 1.0, (t:TData2D).Time)
        let f3d = fun t -> new Td3d((t:TData3D).D1 + 1.0, (t:TData3D).D2 + 1.0, (t:TData3D).D3 + 1.0, (t:TData3D).Time)
        let swriter:SimulationEventWriter<_> = new SimulationEventWriter<_>("bla",100,50,new Td1d(1.0,System.DateTime.Now), f1d)
        let swriter2:SimulationEventWriter<_> = new SimulationEventWriter<_>("blabla",100,50,new Td2d(2.0,20.0,System.DateTime.Now), f2d)
        let swriter3:SimulationEventWriter<_> = new SimulationEventWriter<_>("blablabla",100,50,new Td3d(1.0,10.0,100.0,System.DateTime.Now), f3d)

        0
    *)