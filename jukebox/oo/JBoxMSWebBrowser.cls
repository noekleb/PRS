USING Progress.Lang.*.
USING Progress.Windows.UserControl.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxMSWebBrowser INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components  AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PUBLIC PROPERTY  webBrowser1 AS System.Windows.Forms.WebBrowser NO-UNDO GET. SET.

  DEFINE PUBLIC VARIABLE hProc         AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hWindow       AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hDesignWidget AS HANDLE NO-UNDO.
		
  DEFINE PRIVATE PROPERTY bDisableTextChanged AS LOG NO-UNDO     
    GET.    
    SET.

	CONSTRUCTOR PUBLIC JBoxMSWebBrowser ( INPUT ihProc         AS HANDLE,
                                        INPUT ihDesignWidget AS HANDLE ):
	 
    SUPER().
    ASSIGN hProc         = ihProc          
           hWindow       = ihProc:CURRENT-WINDOW
           hDesignWidget = ihDesignWidget
           . 
    InitializeComponent().
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	METHOD PUBLIC VOID InitializeComponent(  ):
    THIS-OBJECT:webBrowser1 = NEW System.Windows.Forms.WebBrowser().
    THIS-OBJECT:SuspendLayout().
    THIS-OBJECT:webBrowser1:Location = NEW System.Drawing.Point(1, 1).
    THIS-OBJECT:webBrowser1:Size = NEW System.Drawing.Size(hDesignWidget:WIDTH-PIXELS,hDesignWidget:HEIGHT-PIXELS).
    THIS-OBJECT:webBrowser1:Name = "webBrowser1".
    THIS-OBJECT:webBrowser1:TabIndex = 0.

/*     THIS-OBJECT:webBrowser1:TextChanged:Subscribe(THIS-OBJECT:handler_TextChanged). */
/*     THIS-OBJECT:webBrowser1:KeyPress:Subscribe(THIS-OBJECT:handler_KeyPress).  */

    THIS-OBJECT:Controls:Add(THIS-OBJECT:webBrowser1).
    THIS-OBJECT:Location = NEW System.Drawing.Point(DYNAMIC-FUNCTION("getAbsPosition",hDesignWidget,"x"),DYNAMIC-FUNCTION("getAbsPosition",hDesignWidget,"y")).  
    THIS-OBJECT:Size = NEW System.Drawing.Size(hDesignWidget:WIDTH-PIXELS,hDesignWidget:HEIGHT-PIXELS).
    THIS-OBJECT:Name = "JBoxMSWebBrowser".
    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.

  END METHOD.
 
  /*
  METHOD PRIVATE VOID handler_TextChanged (sender AS CLASS System.Object, e AS CLASS System.EventArgs):
    DEF VAR hFieldMap  AS HANDLE NO-UNDO.
    DEF VAR hToolbar   AS HANDLE NO-UNDO.
    IF NOT bDisableTextChanged THEN DO:
      hFieldMap = DYNAMIC-FUNCTION("GetLinkedObject",hDesignWidget,"FieldMap","from").
      IF VALID-HANDLE(hFieldMap) THEN DO:
        hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",hFieldMap,"toolbar","from").
        IF VALID-HANDLE(hToolbar) THEN
          DYNAMIC-FUNCTION("setToolbar",hToolbar,"modified").
      END.
    END.
    bDisableTextChanged = NO.
  END METHOD. 
  */

/*   METHOD PRIVATE VOID handler_KeyPress (sender AS CLASS System.Object, e AS CLASS System.EventArgs):  */
/*     MESSAGE e                                                                                         */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                            */
/*   END METHOD.                                                                                         */

  /*
  METHOD PUBLIC VOID DisableTextChanged (ibDisableTextChanged AS LOG):
    IF webBrowser1:Parent:Parent:VISIBLE THEN
      bDisableTextChanged = ibDisableTextChanged.
  END METHOD.
  */

	DESTRUCTOR PUBLIC JBoxMSWebBrowser ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
END CLASS.
