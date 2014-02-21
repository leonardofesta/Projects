module BufferData.IData


type Data<'T> =
    interface
        abstract member Info : 'T
    end

type Accumulator<'U,'T> when 'U :> Data<'T> = 

    abstract member AddItem :   'U -> unit

    abstract member Restart :   unit -> unit

type NumericData<'U,'T> when 'U :> Data<'T> = 
    
    abstract member Last        :   'U
                with get
    abstract member SecondLast  :   'U
                with get
    abstract member Sum         :   'U
                with get
    abstract member Average     :   'U 
                with get 
    abstract member Variance    :   'U
                with get
    abstract member StDev       :   'U
                with get
    abstract member Count       :   int
                with get 

type Data1D<'T> = 
    inherit Data<'T>
    abstract member D1 : float
                with get 


type Data2D<'T> = 
    inherit Data<'T>
    abstract member D1 : float
                with get 
    abstract member D2 : float
                with get 
    
type Data3D<'T> =
    inherit Data<'T>
    abstract member D1 : float
                with get 
    abstract member D2 : float       
                with get
    abstract member D3 : float
                with get

                

type TData<'T> = 
    inherit Data<'T>
    abstract member Time : System.DateTime
                with get

type TData1D<'T> =
    inherit TData<'T>
    abstract member D1 : float
                with get 

type TData2D<'T> =
    inherit TData<'T>
    abstract member D1 : float
                with get 
    abstract member D2 : float       
                with get

type TData3D<'T> =
    inherit TData<'T>
    abstract member D1 : float
                with get 
    abstract member D2 : float       
                with get
    abstract member D3 : float
                with get




[<AbstractClass>]
type BufferedData<'T>() =
    inherit System.EventArgs() 
    abstract member AddItem: 'T*('T->bool) -> unit
    abstract member AddItem: 'T -> unit