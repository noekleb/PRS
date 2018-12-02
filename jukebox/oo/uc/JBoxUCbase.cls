USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING uc.* FROM PROPATH.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxUCbase INHERITS UserControl: 
	
  DEFINE PUBLIC PROPERTY iDeltaWidth      AS INTEGER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY iDeltaHeight     AS INTEGER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY bApplyReturn     AS LOG     NO-UNDO GET. SET.
  DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO. 
  DEFINE PUBLIC VARIABLE hWinSuper        AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hWinProc         AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hWindow          AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hPlaceHolder     AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hbUserControls   AS HANDLE NO-UNDO. /* Buffer to temp-table for user controls in window super */
  DEFINE PUBLIC VARIABLE oForm            AS Form   NO-UNDO.
  DEFINE PUBLIC VARIABLE hBrwBufQry       AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hBuffer          AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE cBufferDisplay   AS CHAR   NO-UNDO.
  DEFINE PUBLIC VARIABLE cPreviewKey      AS CHAR   NO-UNDO.

  DEF VAR iDefaultFontSize    AS INT  NO-UNDO.
  DEF VAR cDefaultFont        AS CHAR NO-UNDO.

  DEFINE VARIABLE  bDisableTextChanged AS LOG NO-UNDO INIT YES.

	CONSTRUCTOR PUBLIC JBoxUCbase (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                 INPUT ihWinProc     AS HANDLE,
                                 INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                 INPUT iiDeltaWidth  AS INTEGER):
    SUPER().
    ASSIGN hWinSuper        = IF VALID-HANDLE(ihWinSuper) THEN ihWinSuper ELSE DYNAMIC-FUNCTION("getWindowSuper" IN ihWinProc)        
           hWinProc         = ihWinProc
           hWindow          = ihWinProc:CURRENT-WINDOW
           hPlaceHolder     = ihPlaceHolder
           cDefaultFont     = THIS-OBJECT:Font:Name
           iDefaultFontSize = THIS-OBJECT:Font:Size
           iDeltaWidth      = iiDeltaWidth.
           .

    InitializeComponent().
                
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxUCbase (  INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                   INPUT ihWinProc     AS HANDLE,
                                   INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                   INPUT iiDeltaWidth  AS INTEGER,
                                   input ihQuery AS HANDLE, 
                                   INPUT ihTable as handle):
    SUPER().
    ASSIGN hWinSuper        = IF VALID-HANDLE(ihWinSuper) THEN ihWinSuper ELSE DYNAMIC-FUNCTION("getWindowSuper" IN ihWinProc)        
           hWinProc         = ihWinProc
           hWindow          = ihWinProc:CURRENT-WINDOW
           hPlaceHolder     = ihPlaceHolder
           cDefaultFont     = THIS-OBJECT:Font:Name
           iDefaultFontSize = THIS-OBJECT:Font:Size
           iDeltaWidth      = iiDeltaWidth.
           .

    DYNAMIC-INVOKE(THIS-OBJECT,"PreInitializeComponent",ihQuery,ihTable).
    
    InitializeComponent().
                
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxUCbase ( INPUT ihWinProc     AS HANDLE,
                                  INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
	 
    THIS-OBJECT(?,ihWinProc,ihPlaceHolder,0).
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxUCbase ( INPUT ihWinProc     AS HANDLE,
                                  INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                  INPUT iiDeltaWidth  AS INT):
	  
    THIS-OBJECT(?,ihWinProc,ihPlaceHolder,iiDeltaWidth).
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	METHOD PUBLIC VOID InitializeComponent(  ):
	  DEF VAR iAbsX AS INT NO-UNDO.
	      
    hPlaceHolder:WIDTH-PIXELS = hPlaceHolder:WIDTH-PIXELS + iDeltaWidth.

    THIS-OBJECT:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS,hPlaceHolder:HEIGHT-PIXELS).
    
    iAbsX = DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"x") NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      THIS-OBJECT:Location = NEW System.Drawing.Point(DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"x"),DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"y")).
    ELSE    
      THIS-OBJECT:Location = NEW System.Drawing.Point(hPlaceHolder:X,hPlaceHolder:Y).
      
    oForm = CAST(DYNAMIC-FUNCTION("getWindowForm" IN hWinProc),"Progress.Windows.Form").

    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.

  END METHOD.
 
  /* Mandatory methods: */
/*
  METHOD PUBLIC C1.Win.C1GanttView.C1GanttView getDotNetWidget():
    RETURN c1GanttView1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(c1GanttView1:GetType()).
  END METHOD.
*/
  METHOD PUBLIC LOGICAL RegisterWithJukeBox(INPUT ibView AS LOG):

    hbUserControls = DYNAMIC-FUNCTION("getUserControlsBuffer" IN hWinProc).
/*    oForm = CAST(DYNAMIC-FUNCTION("getWindowForm" IN hWinProc),"Progress.Windows.Form").*/
    
    IF NOT VALID-OBJECT(oForm) THEN
      oForm = CAST(DYNAMIC-FUNCTION("getWindowForm" IN hWinProc),"Progress.Windows.Form").
    IF NOT VALID-OBJECT(oForm) THEN RETURN NO.

    oForm:Controls:Add(THIS-OBJECT).

    IF ibView THEN THIS-OBJECT:BringToFront().
    ELSE THIS-OBJECT:SendToBack().

    DYNAMIC-FUNCTION("NewObject",hWindow,hPlaceHolder,STRING(THIS-OBJECT:GetType())) NO-ERROR.

    hbUserControls:BUFFER-CREATE().
    ASSIGN hbUserControls::oUserControl     = CAST(THIS-OBJECT,"Progress.Lang.Object")
           hbUserControls::cUserControlType = STRING(THIS-OBJECT:GetType())
           hbUserControls::oWidget          = CAST(DYNAMIC-INVOKE(THIS-OBJECT,"getDotNetWidget"),"Progress.Lang.Object")
           hbUserControls::cWidgetType      = DYNAMIC-INVOKE(THIS-OBJECT,"getDotNetWidgetType")
           hbUserControls::hPlaceHolder     = hPlaceHolder
           hbUserControls::hTabFolder       = DYNAMIC-FUNCTION("getCurrTabFolder" IN hWinProc)
           hbUserControls::iTabPage         = DYNAMIC-FUNCTION("getCurrTabPage" IN hWinProc)
           hbUserControls::hSourceProc      = hWinProc
           hbUserControls::cMethodSuffix    = "CalcEdit"
           hbUserControls::iDeltaWidth      = iDeltaWidth
           hbUserControls::iDeltaHeight     = iDeltaHeight
           .
    
    RETURN YES.
  END METHOD.

  METHOD PUBLIC LOGICAL CreateDisplayLink(INPUT ihBrwBufQry AS HANDLE,INPUT icDisplay AS CHAR):
    ASSIGN hBrwBufQry     = ihBrwBufQry
           cBufferDisplay = icDisplay
           .

    CASE hBrwBufQry:TYPE:
      WHEN "browse" THEN hBuffer = hBrwBufQry:QUERY:GET-BUFFER-HANDLE(1).
      WHEN "query"  THEN hBuffer = hBrwBufQry:GET-BUFFER-HANDLE(1).
      OTHERWISE hBuffer = hBrwBufQry.
    END CASE.

    DYNAMIC-FUNCTION("CreateDotNetDisplayLink",hBrwBufQry,hPlaceHolder,cBufferDisplay).
  END METHOD.

  METHOD PUBLIC CHARACTER SUPERB():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.

  /* Methods invoked from jukebox (via window super): */

  METHOD PUBLIC CHARACTER dotNetDisplay():
    DEF VAR cEntityId AS CHAR NO-UNDO.
    DEF VAR ix        AS INT  NO-UNDO.
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR dDate     AS DATE FORMAT "99/99/9999" NO-UNDO.


    bApplyReturn = NO.

    RETURN "".
  END.

  METHOD PUBLIC CHARACTER dotNetNew():
    RETURN "".
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetFocus():
/*    THIS-OBJECT:ActiveControl = c1GanttView1.*/
/*    c1GanttView1:Focus().                    */
    THIS-OBJECT:Focus().
    RETURN "".
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetPreSave():
    RETURN "".
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetSave():
    RETURN "".
  END METHOD.

  METHOD PUBLIC VOID setEnableReturn(INPUT ibEnable AS LOG):
    bApplyReturn = ibEnable.
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetEnable(ibEnable AS LOG):
    RETURN "".
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetVisible(ibVisible AS LOG):
/*    c1GanttView1:Visible = ibVisible.*/
    RETURN "".
  END METHOD.

  METHOD PUBLIC LOGICAL applyTab():
/*    THIS-OBJECT:c1GanttView1:SelectNextControl(?,NO,NO,NO,NO).*/
  END METHOD.

	DESTRUCTOR PUBLIC JBoxUCbase ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
END CLASS.
