module GestIT.IData


type Data =
    interface
    end

type Accumulator<'U> when 'U :> Data = 

    abstract member AddItem :   'U -> unit

    abstract member Restart :   unit -> unit

//capire se vale la pena avere questa interfaccia
type NumericData<'U> when 'U :> Data = 

    abstract member Sum         :   'U
                with get
    abstract member Average     :   'U 
                with get 
    abstract member Variance    :   'U
                with get
    abstract member StDev       :   'U
                with get


type Data1D = 
    inherit Data
    abstract member D1 : float
                with get 


type Data2D = 
    inherit Data
    abstract member D1 : float
                with get 
    abstract member D2 : float
                with get 
    
type Data3D =
    inherit Data
    abstract member D1 : float
                with get 
    abstract member D2 : float       
                with get
    abstract member D3 : float
                with get

                

type TData = 
    inherit Data
    abstract member Time : System.DateTime
                with get

type TData1D =
    inherit TData
    abstract member D1 : float
                with get 

type TData2D =
    inherit TData
    abstract member D1 : float
                with get 
    abstract member D2 : float       
                with get

type TData3D =
    inherit TData
    abstract member D1 : float
                with get 
    abstract member D2 : float       
                with get
    abstract member D3 : float
                with get




[<AbstractClass>]
type BufferedData<'T>() =
    inherit System.EventArgs() 
    abstract member AddItem: 'T -> unit