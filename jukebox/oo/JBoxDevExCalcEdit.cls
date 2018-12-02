USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.

/* using System.Globalization.*.             */
/* using System.Threading.*.                 */
/* using DevExpress.XtraEditors.Controls.*.  */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxDevExCalcEdit INHERITS UserControl: 
	
  DEFINE PUBLIC PROPERTY iDeltaWidth      AS INTEGER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY bApplyReturn     AS LOG     NO-UNDO GET. SET.
	DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PUBLIC VARIABLE calcEdit1        AS DevExpress.XtraEditors.CalcEdit NO-UNDO.
  DEFINE PUBLIC VARIABLE hWinSuper        AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hWinProc         AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hWindow          AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hPlaceHolder     AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hbUserControls   AS HANDLE NO-UNDO. /* Buffer to temp-table for user controls in window super */
  DEFINE PUBLIC VARIABLE oForm            AS Form   NO-UNDO.
  DEFINE PUBLIC VARIABLE hBrwBufQry       AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hBuffer          AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE cBufferDisplay   AS CHAR   NO-UNDO.
  DEFINE PUBLIC VARIABLE oJBoxContextMenu AS JBoxContextMenu NO-UNDO. /* See last method: InitializeMenu */
  DEFINE PUBLIC VARIABLE cPreviewKey      AS CHAR   NO-UNDO.
  DEFINE PRIVATE VARIABLE cPosition       AS CHAR   NO-UNDO.

  DEF VAR iDefaultFontSize    AS INT  NO-UNDO.
  DEF VAR cDefaultFont        AS CHAR NO-UNDO.

  DEFINE VARIABLE  bDisableTextChanged AS LOG NO-UNDO INIT YES.

	CONSTRUCTOR PUBLIC JBoxDevExCalcEdit (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
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


/* Thread:CurrentThread:CurrentUICulture = new CultureInfo("en-us").  */
/* Localizer:Active = Localizer:CreateDefaultLocalizer().             */

    InitializeComponent().
                
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	CONSTRUCTOR PUBLIC JBoxDevExCalcEdit ( INPUT ihWinProc     AS HANDLE,
                                         INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
	 
    THIS-OBJECT(?,ihWinProc,ihPlaceHolder,0).
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	CONSTRUCTOR PUBLIC JBoxDevExCalcEdit ( INPUT ihWinProc     AS HANDLE,
                                         INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                         INPUT iiDeltaWidth  AS INT):
	  
    THIS-OBJECT(?,ihWinProc,ihPlaceHolder,iiDeltaWidth).
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	METHOD PUBLIC VOID InitializeComponent(  ):
    /* Custom: */
    hPlaceHolder:WIDTH-PIXELS = hPlaceHolder:WIDTH-PIXELS + iDeltaWidth.

    THIS-OBJECT:calcEdit1 = NEW DevExpress.XtraEditors.CalcEdit().
/*     CAST(THIS-OBJECT:calcEdit1:Properties:VistaTimeProperties, System.ComponentModel.ISupportInitialize):BeginInit(). */
    CAST(THIS-OBJECT:calcEdit1:Properties, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* calcEdit1 */
    /*  */
    THIS-OBJECT:calcEdit1:EditValue = ?.
    THIS-OBJECT:Controls:Add(THIS-OBJECT:calcEdit1).

    /* Custom: Location, Size and Name + others: */
    THIS-OBJECT:Location = NEW System.Drawing.Point(DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"x"),DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"y")).  
    THIS-OBJECT:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS,hPlaceHolder:HEIGHT-PIXELS).
    THIS-OBJECT:Name = "JBoxDevExCalcEdit".
/*     DEFINE VARIABLE mask1 AS CLASS DevExpress.XtraEditors.Mask.MaskType NO-UNDO.                    */
/*     mask1 = NEW DevExpress.XtraEditors.Mask.MaskType(DevExpress.XtraEditors.Mask.MaskType:Numeric). */
 
/*    THIS-OBJECT:calcEdit1:MinimumSize = NEW System.Drawing.Size(18,hPlaceHolder:HEIGHT-PIXELS).*/
/*    THIS-OBJECT:calcEdit1:MaximumSize = NEW System.Drawing.Size(18,hPlaceHolder:HEIGHT-PIXELS).*/
/*    THIS-OBJECT:calcEdit1:TabStop = FALSE.                                                     */
 
    THIS-OBJECT:calcEdit1:Properties:Mask:MaskType = DevExpress.XtraEditors.Mask.MaskType:Numeric.
/*     THIS-OBJECT:calcEdit1:Properties:Mask:EditMask = "Numeric". */
    THIS-OBJECT:calcEdit1:Properties:Mask:UseMaskAsDisplayFormat = YES.

    DEFINE VARIABLE arrayvar0 AS DevExpress.XtraEditors.Controls.EditorButton EXTENT 1 NO-UNDO.
    arrayvar0[1] = NEW DevExpress.XtraEditors.Controls.EditorButton(DevExpress.XtraEditors.Controls.ButtonPredefines:Combo).
    THIS-OBJECT:calcEdit1:Properties:Buttons:AddRange(arrayvar0).
    DEFINE VARIABLE arrayvar1 AS DevExpress.XtraEditors.Controls.EditorButton EXTENT 1 NO-UNDO.
    arrayvar1[1] = NEW DevExpress.XtraEditors.Controls.EditorButton().
/*     THIS-OBJECT:calcEdit1:Properties:VistaTimeProperties:Buttons:AddRange(arrayvar1). */
    THIS-OBJECT:calcEdit1:TabIndex = 0.

    THIS-OBJECT:Controls:Add(THIS-OBJECT:calcEdit1).
/*     CAST(THIS-OBJECT:calcEdit1:Properties:VistaTimeProperties, System.ComponentModel.ISupportInitialize):EndInit(). */
    CAST(THIS-OBJECT:calcEdit1:Properties, System.ComponentModel.ISupportInitialize):EndInit().

    /* Custom: Event handlers: */
    THIS-OBJECT:calcEdit1:TextChanged:Subscribe(THIS-OBJECT:handler_TextChanged).
    THIS-OBJECT:calcEdit1:Leave:Subscribe(THIS-OBJECT:handler_Leave).
    THIS-OBJECT:calcEdit1:KeyDown:Subscribe(THIS-OBJECT:handler_KeyDown).
    THIS-OBJECT:calcEdit1:KeyUp:Subscribe(THIS-OBJECT:handler_KeyUp).
    THIS-OBJECT:calcEdit1:PreviewKeyDown:Subscribe(THIS-OBJECT:handler_PreviewKeyDown).
    THIS-OBJECT:calcEdit1:EditValueChanged:Subscribe(THIS-OBJECT:handler_EditValueChanged).

    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.

  END METHOD.
 
  /* Mandatory methods: */

  METHOD PUBLIC DevExpress.XtraEditors.CalcEdit getDotNetWidget():
    RETURN calcEdit1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(calcEdit1:GetType()).
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
           hbUserControls::cMethodSuffix    = "CalcEdit"
           hbUserControls::iDeltaWidth      = iDeltaWidth
/*           hbUserControls::cPosition        = "right-of"*/
           .
/*     IF cPosition NE "" THEN                 */
/*       hbUserControls::cPosition = cPosition.*/

    calcEdit1:Properties:Mask:EditMask = "n0".
  END METHOD.

  METHOD PUBLIC LOGICAL CreateDisplayLink(INPUT ihBrwBufQry AS HANDLE,INPUT icDisplay AS CHAR):
    ASSIGN hBrwBufQry     = ihBrwBufQry
           cBufferDisplay = icDisplay
           .

    CASE hBrwBufQry:TYPE:
      WHEN "browse" THEN DO:
        hBuffer = hBrwBufQry:QUERY:GET-BUFFER-HANDLE(1).
        IF hbUserControls:AVAIL THEN hbUserControls::cPosition = "right".
        ELSE cPosition = "right".
      END.  
      WHEN "query"  THEN hBuffer = hBrwBufQry:GET-BUFFER-HANDLE(1).
      OTHERWISE hBuffer = hBrwBufQry.
    END CASE.

    DYNAMIC-FUNCTION("CreateDotNetDisplayLink",hBrwBufQry,hPlaceHolder,cBufferDisplay).
  END METHOD.

  METHOD PUBLIC CHARACTER DEFAULT():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.

  /* Methods invoked from jukebox (via window super): */

  METHOD PUBLIC CHARACTER dotNetDisplay():
    DEF VAR cEntityId AS CHAR NO-UNDO.
    DEF VAR ix        AS INT  NO-UNDO.
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR dDate     AS DATE FORMAT "99/99/9999" NO-UNDO.

    bDisableTextChanged = YES.
    calcEdit1:Font = NEW System.Drawing.Font(cDefaultFont,iDefaultFontSize,System.Drawing.FontStyle:Regular).

    bApplyReturn = NO.

    IF hBuffer:AVAIL THEN 
      calcEdit1:EditValue = hBuffer:BUFFER-FIELD(cBufferDisplay):BUFFER-VALUE.
    ELSE
      calcEdit1:EditValue = ?.
    calcEdit1:Enabled = hBuffer:AVAIL AND NOT hPlaceHolder:READ-ONLY.

    RETURN "".
  END.

  METHOD PUBLIC CHARACTER dotNetNew():
    IF CAN-QUERY(hPlaceholder,"initial") THEN
      calcEdit1:EditValue = hPlaceholder:INITIAL.
    ELSE calcEdit1:EditValue = hBuffer:BUFFER-FIELD(cBufferDisplay):INITIAL.
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetFocus():
    THIS-OBJECT:ActiveControl = calcEdit1.
    calcEdit1:Focus().
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
    calcEdit1:Enabled = ibEnable.
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetVisible(ibVisible AS LOG):
    calcEdit1:Visible = ibVisible.
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
    DEF VAR hToolbar AS HANDLE NO-UNDO.

    IF e:Control THEN DO:
      IF VALID-HANDLE(hBrwBufQry) THEN 
        hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",hBrwBufQry,"toolbar","from").
      IF VALID-HANDLE(hToolbar) THEN DO:
        CASE STRING(e:KEYCODE):
          WHEN "s" THEN RUN InvokeMethod(hToolbar,"SaveRecord").
          WHEN "z" THEN RUN InvokeMethod(hToolbar,"UndoRecord").
          WHEN "n" THEN RUN InvokeMethod(hToolbar,"NewRecord").
        END CASE.        
      END.
    END.

  END METHOD. 

  METHOD PRIVATE VOID handler_PreviewKeyDown (sender AS CLASS System.Object, e AS CLASS System.Windows.Forms.PreviewKeyDownEventArgs):
    cPreviewKey = STRING(e:KEYCODE).
  END METHOD.

  METHOD PRIVATE VOID handler_KeyUp (sender AS CLASS System.Object, e AS CLASS System.Windows.Forms.KeyEventArgs):      
/*     calcEdit1:EditValue = DEC(calcEdit1:TEXT). */

    IF NOT bDisableTextChanged AND LOOKUP(STRING(e:KEYCODE),"0;1;2;3;4;5;6;7;8;9;,") > 0 THEN
      RUN InvokeMethod IN hWinSuper (hPlaceHolder,"TextChanged").
    bDisableTextChanged = NO.

    IF STRING(e:KEYCODE) = "return" AND bApplyReturn THEN DO:
      DYNAMIC-FUNCTION("setAttribute",hPlaceholder,"last-event","enter").
      APPLY "return" TO hPlaceholder.
    END.

  END METHOD. 

  METHOD PRIVATE VOID handler_EditValueChanged (sender AS CLASS System.Object, e AS CLASS System.EventArgs):
    /* Value changed from popup */
    IF NOT bDisableTextChanged THEN 
      RUN InvokeMethod IN hWinSuper (hPlaceHolder,"TextChanged").
    bDisableTextChanged = NO.
  END METHOD.

  METHOD PUBLIC CHARACTER TextChangedCalcEdit():
    DEF VAR hToolbar AS HANDLE NO-UNDO.

    hPlaceholder:SCREEN-VALUE = STRING(calcEdit1:EditValue).
  
    IF VALID-HANDLE(hBrwBufQry) AND hPlaceholder:MODIFIED THEN DO:
      hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",hBrwBufQry,"toolbar","from").
      IF VALID-HANDLE(hToolbar) THEN
        DYNAMIC-FUNCTION("setToolbar",hToolbar,"modified").
      hPlaceholder:MODIFIED = YES.
    END.

    bDisableTextChanged = NO.
    RETURN "".
  END METHOD.

  /* Class specific methods: */

  METHOD PUBLIC LOGICAL applyTab():
    THIS-OBJECT:calcEdit1:SelectNextControl(?,NO,NO,NO,NO).
  END METHOD.

	DESTRUCTOR PUBLIC JBoxDevExCalcEdit ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
END CLASS.
