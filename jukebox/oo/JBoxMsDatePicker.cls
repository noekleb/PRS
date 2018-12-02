USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxMsDatePicker INHERITS UserControl: 
	
  DEFINE PUBLIC PROPERTY iDeltaWidth      AS INTEGER NO-UNDO GET. SET.
	DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE datePicker1     AS System.Windows.Forms.DateTimePicker NO-UNDO.
  DEFINE PUBLIC VARIABLE hWinSuper        AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hWinProc         AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hWindow          AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hPlaceHolder     AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hbUserControls   AS HANDLE NO-UNDO. /* Buffer to temp-table for user controls in window super */
  DEFINE PUBLIC VARIABLE oForm            AS Form   NO-UNDO.
  DEFINE PUBLIC VARIABLE hFieldMap        AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE cFieldMapDisplay AS CHAR   NO-UNDO.
  DEFINE PUBLIC VARIABLE oJBoxContextMenu AS JBoxContextMenu NO-UNDO. /* See last method: InitializeMenu */
  DEFINE PUBLIC VARIABLE cPreviewKey      AS CHAR  NO-UNDO.

  DEF VAR iDefaultFontSize    AS INT  NO-UNDO.
  DEF VAR cDefaultFont        AS CHAR NO-UNDO.

  DEFINE VARIABLE  bDisableTextChanged AS LOG NO-UNDO INIT YES.

	CONSTRUCTOR PUBLIC JBoxMsDatePicker (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
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
           hPlaceHolder:WIDTH-PIXELS = hPlaceHolder:WIDTH-PIXELS + iiDeltaWidth
           .

    InitializeComponent().
                
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	CONSTRUCTOR PUBLIC JBoxMsDatePicker ( INPUT ihWinProc     AS HANDLE,
                                      INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
	 
    THIS-OBJECT(?,ihWinProc,ihPlaceHolder,0).
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	CONSTRUCTOR PUBLIC JBoxMsDatePicker ( INPUT ihWinProc     AS HANDLE,
                                        INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                        INPUT iiDeltaWidth  AS INT):
	 
    THIS-OBJECT(?,ihWinProc,ihPlaceHolder,iiDeltaWidth).
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	METHOD PUBLIC VOID InitializeComponent(  ):

    THIS-OBJECT:datePicker1 = NEW System.Windows.Forms.DateTimePicker().
    THIS-OBJECT:SuspendLayout().
    THIS-OBJECT:datePicker1:Format = System.Windows.Forms.DateTimePickerFormat:Custom.
    THIS-OBJECT:datePicker1:Location = NEW System.Drawing.Point(1, 1).
    THIS-OBJECT:datePicker1:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS,hPlaceHolder:HEIGHT-PIXELS).
    THIS-OBJECT:datePicker1:Name = "datePicker1".
    THIS-OBJECT:datePicker1:TabIndex = 0.
    THIS-OBJECT:datePicker1:TabStop = NO.

    THIS-OBJECT:datePicker1:TextChanged:Subscribe(THIS-OBJECT:handler_TextChanged).
    THIS-OBJECT:datePicker1:Leave:Subscribe(THIS-OBJECT:handler_Leave).
    THIS-OBJECT:datePicker1:KeyDown:Subscribe(THIS-OBJECT:handler_KeyDown).
    THIS-OBJECT:datePicker1:PreviewKeyDown:Subscribe(THIS-OBJECT:handler_PreviewKeyDown).

    THIS-OBJECT:Controls:Add(THIS-OBJECT:datePicker1).
    THIS-OBJECT:Location = NEW System.Drawing.Point(DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"x"),DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"y")).  
    THIS-OBJECT:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS,hPlaceHolder:HEIGHT-PIXELS).
    THIS-OBJECT:Name = "JBoxMsDatePicker".
    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.

  END METHOD.
 
  /* Mandatory methods: */

  METHOD PUBLIC System.Windows.Forms.DateTimePicker getDotNetWidget():
    RETURN datePicker1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(datePicker1:GetType()).
  END METHOD.

  METHOD PUBLIC LOGICAL RegisterWithJukeBox(INPUT ibView AS LOG):

    hbUserControls = DYNAMIC-FUNCTION("getUserControlsBuffer" IN hWinProc).
    oForm = CAST(DYNAMIC-FUNCTION("getWindowForm" IN hWinProc),"Progress.Windows.Form").

    oForm:Controls:Add(THIS-OBJECT).

    IF ibView THEN THIS-OBJECT:BringToFront().
    ELSE THIS-OBJECT:SendToBack().

    DYNAMIC-FUNCTION("NewObject",hWindow,hPlaceHolder,STRING(THIS-OBJECT:GetType())).

    hbUserControls:BUFFER-CREATE().
    ASSIGN hbUserControls::oUserControl     = CAST(THIS-OBJECT,"Progress.Lang.Object")
           hbUserControls::cUserControlType = STRING(THIS-OBJECT:GetType())
           hbUserControls::oWidget          = CAST(THIS-OBJECT:getDotNetWidget(),"Progress.Lang.Object")
           hbUserControls::cWidgetType      = THIS-OBJECT:getDotNetWidgetType()
           hbUserControls::hPlaceHolder     = hPlaceHolder
           hbUserControls::hTabFolder       = DYNAMIC-FUNCTION("getCurrTabFolder" IN hWinProc)
           hbUserControls::iTabPage         = DYNAMIC-FUNCTION("getCurrTabPage" IN hWinProc)
           hbUserControls::hSourceProc      = hWinProc
           hbUserControls::cMethodSuffix    = "DatePick"
           hbUserControls::iDeltaWidth      = iDeltaWidth
           .

  END METHOD.

  METHOD PUBLIC LOGICAL CreateDisplayLink(INPUT ihFieldMap AS HANDLE,INPUT icDisplay AS CHAR):
    ASSIGN hFieldMap        = ihFieldMap
           cFieldMapDisplay = icDisplay
           .
    DYNAMIC-FUNCTION("CreateDotNetDisplayLink",hFieldMap,hPlaceHolder,cFieldMapDisplay).
  END METHOD.

  METHOD PUBLIC CHARACTER DEFAULT():
    MESSAGE PROGRAM-NAME(2)
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.

  /* Methods invoked from jukebox (via window super): */

  METHOD PUBLIC CHARACTER dotNetDisplay():
    DEF VAR cEntityId AS CHAR NO-UNDO.
    DEF VAR ix        AS INT  NO-UNDO.
    DEF VAR cFileName AS CHAR NO-UNDO.

    bDisableTextChanged = YES.
    datePicker1:Font = NEW System.Drawing.Font(cDefaultFont,iDefaultFontSize,System.Drawing.FontStyle:Regular).

    IF hFieldMap:AVAIL THEN 
      datePicker1:Text = hFieldMap:BUFFER-FIELD(cFieldMapDisplay):STRING-VALUE.
    ELSE
      datePicker1:Text = "".
    datePicker1:Enabled = hFieldMap:AVAIL.
    RETURN "".
  END.

  METHOD PUBLIC CHARACTER dotNetFocus():
    THIS-OBJECT:ActiveControl = datePicker1.
    datePicker1:Focus().
  END METHOD.

  METHOD PUBLIC CHARACTER PreSaveRecord():
         /* Invoked from jboxuilib.SaveRecord before record i saved so input data from the dot.net control
            may be added to the dynamic save operation. */
    DEF VAR hWidget          AS HANDLE NO-UNDO.
    DEF VAR cScreenUpdFields AS CHAR   NO-UNDO.

    cScreenUpdFields = DYNAMIC-FUNCTION("getAttribute",hFieldMap,"screenUpdateFields").

    IF CAN-DO(cScreenUpdFields,cFieldMapDisplay) THEN DO:
      hWidget = WIDGET-HANDLE(ENTRY(LOOKUP(cFieldMapDisplay,cScreenUpdFields),DYNAMIC-FUNCTION("getAttribute",hFieldMap,"screenUpdateWidgets"))) NO-ERROR.
      IF VALID-HANDLE(hWidget) THEN hWidget:SCREEN-VALUE = datePicker1:Text.
    END.

  END METHOD.

  METHOD PUBLIC CHARACTER dotNetSave():
    RETURN "".
  END.

  METHOD PUBLIC CHARACTER dotNetEnable(ibEnable AS LOG):
    datePicker1:Enabled = ibEnable.
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetVisible(ibVisible AS LOG):
    datePicker1:Visible = ibVisible.
  END METHOD.

  /* (Semi-)class specific methods - event handlers: */
  METHOD PRIVATE VOID handler_TextChanged (sender AS CLASS System.Object, e AS CLASS System.EventArgs):
    IF NOT bDisableTextChanged THEN 
      RUN InvokeMethod IN hWinSuper (hPlaceHolder,"TextChanged").
    bDisableTextChanged = NO.
  END METHOD. 

  METHOD PRIVATE VOID handler_Leave (sender AS CLASS System.Object, e AS CLASS System.EventArgs):
    DEF VAR hNextTabItem AS HANDLE NO-UNDO.

    IF CAN-DO("return,tab,shift",cPreviewKey) THEN DO:
      IF NOT DevExpress.Utils.KeyboardHandler.KeyboardHandler:IsShiftPressed THEN
        hNextTabItem = hPlaceHolder:NEXT-TAB-ITEM NO-ERROR.
      ELSE
        hNextTabItem = hPlaceHolder:PREV-TAB-ITEM NO-ERROR.
  
      IF VALID-HANDLE(hNextTabItem) THEN 
        RUN widgetFocus IN hWinSuper (hNextTabItem).
    END.
  END METHOD. 

  METHOD PRIVATE VOID handler_KeyDown (sender AS CLASS System.Object, e AS CLASS System.Windows.Forms.KeyEventArgs):
    

/*     IF DevExpress.Utils.KeyboardHandler.KeyboardHandler:IsControlPressed AND e:KeyCode = System.Windows.Forms.Keys:S THEN DO: */
/*     IF e:Control /* AND e:KeyCode = System.Windows.Forms.Keys:S */ THEN DO: */
/*     END.                                                                    */
/*     ELSE                                                                    */

    IF CAN-DO("d,t",STRING(e:KEYCODE)) THEN DO:
      datePicker1:Text = STRING(TODAY).
      MESSAGE e:Control
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE IF STRING(e:KeyCode) = "Space" THEN DO:
      MESSAGE "space"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      datePicker1:ClearValue(System.Windows.Forms.DateTimePicker.SelectedDateProperty).
    END.
    ELSE IF STRING(e:KEYCODE) = "" THEN DO:
/*       IF e:Shift THEN */
        datePicker1:Text = ?.
    END.
    ELSE IF STRING(e:KEYCODE) = "s" AND e:Control THEN DO:
      MESSAGE "ctrl-s"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

  END METHOD. 

  METHOD PRIVATE VOID handler_PreviewKeyDown (sender AS CLASS System.Object, e AS CLASS System.Windows.Forms.PreviewKeyDownEventArgs):
    cPreviewKey = STRING(e:KEYCODE).
  END METHOD.

  METHOD PUBLIC CHARACTER TextChangedDatePick():
    DEF VAR hToolbar AS HANDLE NO-UNDO.
  
    IF VALID-HANDLE(hFieldMap) /* AND datePicker1:Modified */ THEN DO:
      hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",hFieldMap,"toolbar","from").
      IF VALID-HANDLE(hToolbar) THEN
        DYNAMIC-FUNCTION("setToolbar",hToolbar,"modified").
      hPlaceholder:MODIFIED = YES.
    END.

    bDisableTextChanged = NO.
    RETURN "".
  END METHOD.

  /* Class specific methods: */

	DESTRUCTOR PUBLIC JBoxMsDatePicker ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
END CLASS.
