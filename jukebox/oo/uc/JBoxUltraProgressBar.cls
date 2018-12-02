USING Progress.Lang.*.
USING Progress.Util.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING System.TimeSpan.*.
USING Infragistics.Win.*. 

USING uc.* FROM PROPATH.

/* Ref: http://help.infragistics.com/Help/Doc/WinForms/2012.1/CLR2.0/html/WinGanttView_Adding_Tasks_to_WinGanttView.html */
/* Ref: http://www.infragistics.com/help/windows-forms/Infragistics4.Win.UltraWinGanttView.v14.1~Infragistics.Win.UltraWinGanttView_namespace.html */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxUltraProgressBar INHERITS uc.JBoxUCbase: 
	  			
  DEFINE PRIVATE VARIABLE components        AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraProgressBar1 AS Infragistics.Win.UltraWinProgressBar.UltraProgressBar NO-UNDO.
  DEFINE PRIVATE VARIABLE timer1            AS System.Windows.Forms.Timer NO-UNDO.

  CONSTRUCTOR PUBLIC JBoxUltraProgressBar (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                     INPUT ihWinProc     AS HANDLE,
                                     INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                     INPUT iiDeltaWidth  AS INTEGER):
    SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxUltraProgressBar ( INPUT ihWinProc     AS HANDLE,
                                      INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
    SUPER(?,ihWinProc,ihPlaceHolder,0).
  END CONSTRUCTOR.

	METHOD PUBLIC OVERRIDE VOID InitializeComponent(  ):
	  
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
    THIS-OBJECT:ultraProgressBar1 = NEW Infragistics.Win.UltraWinProgressBar.UltraProgressBar().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* ultraProgressBar1 */
    /*  */
    THIS-OBJECT:ultraProgressBar1:Location = NEW System.Drawing.Point(0, 0).
    THIS-OBJECT:ultraProgressBar1:Name = "ultraProgressBar1".
    THIS-OBJECT:ultraProgressBar1:TabIndex = 0.
    THIS-OBJECT:ultraProgressBar1:Text = "[Formatted]".
    /*  */
    /* UltraProgressBarForm */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
    THIS-OBJECT:Name = "UltraProgressBarForm".
    THIS-OBJECT:Text = "UltraProgressBarForm".
        
    /*
    /* Custom: Event handlers: */
    THIS-OBJECT:ultraProgressBar1:TextChanged:Subscribe(THIS-OBJECT:handler_TextChanged).
    THIS-OBJECT:ultraProgressBar1:Leave:Subscribe(THIS-OBJECT:handler_Leave).
    THIS-OBJECT:ultraProgressBar1:KeyDown:Subscribe(THIS-OBJECT:handler_KeyDown).
    THIS-OBJECT:ultraProgressBar1:KeyUp:Subscribe(THIS-OBJECT:handler_KeyUp).
    THIS-OBJECT:ultraProgressBar1:PreviewKeyDown:Subscribe(THIS-OBJECT:handler_PreviewKeyDown).
    THIS-OBJECT:ultraProgressBar1:EditValueChanged:Subscribe(THIS-OBJECT:handler_EditValueChanged).
    */

    timer1 = NEW System.Windows.Forms.Timer ().
    timer1:INTERVAL = 30000. /* every 30 sec */ 
    timer1:ENABLED = TRUE.
    timer1:Tick:SUBSCRIBE(THIS-OBJECT:TimerTick).
    
    SUPER:InitializeComponent().

    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
    
  END METHOD.
 
  /* Mandatory methods: */

  METHOD PUBLIC Infragistics.Win.UltraWinProgressBar.UltraProgressBar getDotNetWidget():
    RETURN ultraProgressBar1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(ultraProgressBar1:GetType()).
  END METHOD.


  /* Methods invoked from jukebox (via window super): */
  /* (Semi-)class specific methods - event handlers: */
  /* Class specific methods: */

  METHOD PRIVATE VOID TimerTick (INPUT sender as System.Object
                                ,INPUT e as System.EventArgs):

/*    
    label1:text = "Login timeout!  Performing auto shutdown ........".
    label1:update().
    timer1:ENABLED = FALSE.
    
    PAUSE 3 NO-MESSAGE IN WINDOW hABLWindow. 
    SetStartupProgress(100,"Login timeout!  Performing auto shutdown ........").
*/    
  END METHOD.  

  METHOD PUBLIC LOGICAL SetStartupProgress (INPUT  iNum AS INT,input ipcText as char ):
    DEF VAR Y AS INT NO-UNDO.
    DEF VAR iValue AS INT NO-UNDO. 
    
    ultraProgressBar1:VISIBLE = TRUE. 
    ultraProgressBar1:BringToFront().
/*    label1:Show().                       */
/*    label1:Enabled   = TRUE.             */
/*    label1:text = FILL(" ",50) + ipcText.*/
/*    label1:Update().                     */

/*/*    hABLWindow:HIDDEN = false.*/          */
/*    PAUSE 0 NO-MESSAGE IN WINDOW hABLWindow.*/
    DO iValue = ultraProgressBar1:VALUE TO iNum WHILE ultraProgressBar1:VALUE LE 100:
        /*ultraProgressBar1:VALUE = iValue. */
        PAUSE 0.01 NO-MESSAGE IN WINDOW hWindow.
        ultraProgressBar1:performstep(). 
    END.
        
/*    IF ultraProgressBar1:VALUE = 100 THEN                       */
/*    DO:                                                         */
/*        APPLY "CLOSE":U TO THIS-PROCEDURE.                      */
/*        SESSION:REMOVE-SUPER-PROCEDURE(THIS-PROCEDURE) NO-ERROR.*/
/*        DELETE OBJECT hPopUpForm_ABLContainer NO-ERROR.         */
/*        DELETE OBJECT THIS-PROCEDURE NO-ERROR.                  */
/*    END.                                                        */
    RETURN TRUE.
END.

	DESTRUCTOR PUBLIC JBoxUltraProgressBar ( ):

	END DESTRUCTOR.

	
END CLASS.
