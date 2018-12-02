 
 /*------------------------------------------------------------------------
    File        : JBoxOOTimer
    Purpose     : OO timer
    Syntax      : 
    Description : 
    Author(s)   : brynjar@chemistry.no
    Created     :
    Notes       : 2 parameters used: interval in milliseconds and callback procedure
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.*. 



CLASS JBoxOOTimer INHERITS System.Windows.Forms.Timer: 

/*   DEFINE PRIVATE VARIABLE iInterval AS INT NO-UNDO.                               */
/*   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO. */
/*   DEFINE PRIVATE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.           */


  CONSTRUCTOR PUBLIC JBoxOOTimer (INPUT iiInterval AS INT ):
    SUPER().
    THIS-OBJECT:Interval = iiInterval.
    THIS-OBJECT:Tick:Subscribe(THIS-OBJECT:handler_Tick).
/*     iInterval = iiInterval. */
/*     InitializeComponent().  */
  END CONSTRUCTOR.	
  
  
/*
	METHOD PRIVATE VOID InitializeComponent(  ):

    THIS-OBJECT:components = NEW System.ComponentModel.Container().
    THIS-OBJECT:timer1 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
    timer1:Interval = iInterval.

    THIS-OBJECT:Timer1:Tick:Subscribe(THIS-OBJECT:OnTick).
      
  END METHOD.  
  
  
  METHOD PUBLIC VOID OnTick ( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       MESSAGE "tikk"   VIEW-AS ALERT-BOX. 
  END METHOD.
  */
  METHOD PUBLIC VOID handler_Tick ( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
     MESSAGE "tikk"   VIEW-AS ALERT-BOX. 
  END METHOD.

END CLASS.
