﻿// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
module Program
// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
    open System.Collections.Generic
    open System.Diagnostics
    open GestIT
    open ClonableLeapFrame
    open LeapDriver
    
    (* Structures *)
    let s = new LeapDriver.LeapSensor()
    let frameQueue = new Queue<ClonableFrame>()
    let mutable lastFrameInQueue = new ClonableFrame() // it represents the last enqueued frame
    let vectorX = new Leap.Vector((float32)1, (float32)0, (float32)0)
    let vectorY = new Leap.Vector((float32)0, (float32)(-1),(float32) 0)
    let vectorZ = new Leap.Vector((float32)0, (float32)0, (float32)(-1))
    (* Timestamps *)
    let ts_openedhand = ref(-1L : TimeStamp)
    let ts_closedhand = ref(-1L : TimeStamp)
    let mutable lastEnter:TimeStamp = -1L
    let mutable lastFingerLeft:TimeStamp = -1L
    let mutable lastFingerRight:TimeStamp = -1L
    let mutable lastFingerUp:TimeStamp = -1L
    let mutable lastFingerDown:TimeStamp = -1L
    let mutable lastHandRight:TimeStamp = -1L
    let mutable lastHandLeft:TimeStamp = -1L
    let threshpointfinger:TimeStamp = 300000L
    let mutable minX_glob = new ClonableFrame()
    let mutable maxY_glob = new ClonableFrame()
    let mutable nVariations = 0

    (* Predicates *)
    let speed (x:float32) (y:float32) = x / y
    let p = new Predicate<LeapEventArgs>(fun x -> true)
    
    let movehandup (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.HandList.Count <> 1 then
            false
        else
            let id = x.Id
            let o = x.Frame.HandList.[id].Position
            let coda =
                frameQueue
                |> Seq.filter (fun y -> y.HandList.ContainsKey(id) && y.Timestamp >= f.Timestamp - 150000L)
            if coda |> Seq.isEmpty then
                false
            else
                let minY =
                    coda
                    |> Seq.maxBy (fun z -> z.HandList.[id].Position.y)
                if o.y - minY.HandList.[id].Position.y > 100.f then
                    coda
                    |> Seq.filter (fun z -> z.Timestamp <= minY.Timestamp)
                    |> Seq.forall (fun z -> z.HandList.[id].Position.y >= minY.HandList.[id].Position.y)
//                    let lastframes =
//                        coda
//                        |> Seq.filter (fun z -> z.Timestamp >= f.Timestamp - 50000L)
//                        |> Seq.forall (fun z -> z.HandList.[id].Velocity.MagnitudeSquared < 1000.f * 1000.f)
                else
                    false
        

    let movehanddown (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.HandList.Count <> 1 then
            false
        else
            let id = x.Id
            let o = x.Frame.HandList.[id].Position
            let coda =
                frameQueue
                |> Seq.filter (fun y -> y.HandList.ContainsKey(id) && y.Timestamp >= f.Timestamp - 150000L)
            if coda |> Seq.isEmpty then
                false
            else
                let maxY =
                    coda
                    |> Seq.maxBy (fun z -> z.HandList.[id].Position.y)
                if maxY.HandList.[id].Position.y - o.y > 100.f then
                    coda
                    |> Seq.filter (fun z -> z.Timestamp >= maxY.Timestamp)
                    |> Seq.forall (fun z -> z.HandList.[id].Position.y <= maxY.HandList.[id].Position.y)
//                    let lastframes =
//                        coda
//                        |> Seq.filter (fun z -> z.Timestamp >= f.Timestamp - 50000L)
//                        |> Seq.forall (fun z -> z.HandList.[id].Velocity.MagnitudeSquared < 1000.f * 1000.f)
                else
                    false

    let pushhanddown (x:LeapEventArgs) =
        let thresh = 50.f
        let f = x.Frame
        if (lastEnter >= f.Timestamp - 1000000L) || (f.PointableList.Count < 4) then
            false
        else
            let id = x.Id
            let o = x.Frame.HandList.[id].Position
            let coda =
                frameQueue
                |> Seq.filter (fun y -> y.HandList.ContainsKey(id) && y.Timestamp >= f.Timestamp - 100000L)
            if coda |> Seq.isEmpty then
                false
            else
                let maxY =
                    coda
                    |> Seq.maxBy (fun z -> z.HandList.[id].Position.y)
                if maxY.HandList.[id].Position.y - o.y > 80.f then
                    coda
                    |> Seq.filter (fun z -> z.Timestamp >= maxY.Timestamp)
                    |> Seq.forall (fun z ->
                                    let v = z.HandList.[id].Position
                                    let dx = v.x - o.x
                                    let dz = v.z - o.z
                                    (dx*dx + dz*dz) < thresh * thresh
                                    )
                else
                    false

    let movefingerleft (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.PointableList.Count > 2 || f.PointableList.Count = 0 || f.Timestamp - lastFingerLeft < threshpointfinger then
            false
        else
            let finger =
                f.PointableList.Values
                |> Seq.maxBy (fun y -> y.Length)
            finger.Position.x <= -60.f
                   
    let movefingerright (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.PointableList.Count > 2 || f.PointableList.Count = 0 || f.Timestamp - lastFingerRight < threshpointfinger then
            false
        else
            let finger =
                f.PointableList.Values
                |> Seq.maxBy (fun y -> y.Length)
            finger.Position.x >= 50.f

    let movefingerup (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.PointableList.Count > 2 || f.PointableList.Count = 0 || f.Timestamp - lastFingerUp < threshpointfinger then
            false
        else
            let finger =
                f.PointableList.Values
                |> Seq.maxBy (fun y -> y.Length)
            finger.Position.y >= 210.f

    let movefingerdown (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.PointableList.Count > 2 || f.PointableList.Count = 0 || f.Timestamp - lastFingerDown < threshpointfinger then
            false
        else
            let finger =
                f.PointableList.Values
                |> Seq.maxBy (fun y -> y.Length)
            finger.Position.y <= 170.f

    let openhand (x:LeapEventArgs) =
        let f = x.Frame
        f.HandList.Count = 1 && f.PointableList.Count >= 4

    let closehandframe (f:ClonableFrame) =
        f.HandList.Count = 1 && f.PointableList.Count <= 1

    let closehand (x:LeapEventArgs) =
        closehandframe (x.Frame)

    let keepclosed (x:LeapEventArgs) =
            let latestFrames =
                frameQueue
                |> Seq.filter (fun y -> y.Timestamp >= x.Frame.Timestamp - 100000L)
            if Seq.length latestFrames = 0 then
                false
            else
                latestFrames
                |> Seq.forall (fun y -> y.HandList.ContainsKey(x.Id) && (closehandframe y) )

    let timedevent p refts thresh (x:LeapEventArgs) =
        let f = x.Frame
        (p x) && x.Frame.Timestamp - !refts < thresh

    let closetimedhand = timedevent closehand ts_openedhand 150000L
    let opentimedhand = timedevent openhand ts_closedhand 150000L

    let pointableCountIs n =
        new Predicate<LeapEventArgs>(fun x -> x.Frame.PointableList.Count = n)

    (*  GroundTerms definitions *)
    let vedomano1 = new GroundTerm<_,_>(LeapFeatureTypes.ActiveHand, p)
    let vedomano2 = new GroundTerm<_,_>(LeapFeatureTypes.ActiveHand, p)
    let s1 = new Parallel<_,_>(vedomano1, vedomano2)

    movedhanddown.Gesture.Add(fun (sender,e) -> ())


    (* Sensor *)
    let UpdateInformations (f:ClonableFrame, e:LeapFeatureTypes, id:FakeId) =
        (* Update informations in the last enqueued frame *)
        match e with
            | LeapFeatureTypes.ActiveHand -> lastFrameInQueue.HandList.Add(id, f.HandList.[id].Clone())
            | LeapFeatureTypes.ActiveFinger | LeapFeatureTypes.ActiveTool -> lastFrameInQueue.PointableList.Add(id, f.PointableList.[id].Clone())
            | LeapFeatureTypes.MoveHand -> lastFrameInQueue.HandList.[id] <- f.HandList.[id].Clone()
            | LeapFeatureTypes.MoveFinger | LeapFeatureTypes.MoveTool -> lastFrameInQueue.PointableList.[id] <- f.PointableList.[id].Clone()
            | LeapFeatureTypes.NotActiveHand -> lastFrameInQueue.HandList.Remove(id) |> ignore
            | LeapFeatureTypes.NotActiveFinger | LeapFeatureTypes.NotActiveTool -> lastFrameInQueue.PointableList.Remove(id) |> ignore
            | _ -> ()

        (s :> ISensor<_,_>).SensorEvents.Add(fun e ->
            (* Removing too old frames *)
            let t = e.Event.Frame.Timestamp
            while (frameQueue.Count > 0 && (t - frameQueue.Peek().Timestamp > (int64)250000)) do
                frameQueue.Dequeue() |> ignore
            (* Receiving updates from sensor *)
            let f = e.Event.Frame
            let id = e.Event.Id
            if lastFrameInQueue.Timestamp <> f.Timestamp then
                (* in this case, surely lastFrame.TS < f.TS, so it has to be added to the queue *)
                let newFrame = f.Clone()
                frameQueue.Enqueue(newFrame)
                lastFrameInQueue <- newFrame
            else
                (* update frame informations *)
                UpdateInformations(f, e.FeatureType, id)
                openedhand1.Gesture.Add(fun (sender,e) -> ts_openedhand := e.Event.Frame.Timestamp)
        )
