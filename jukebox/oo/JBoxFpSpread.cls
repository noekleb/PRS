USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING FarPoint.Win.Spread.ColumnHeader.*.
USING FarPoint.Win.Spread.PrintInfo.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxFpSpread INHERITS UserControl: 
	
  DEFINE PUBLIC PROPERTY iDeltaWidth      AS INTEGER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY bApplyReturn     AS LOG     NO-UNDO GET. SET.
	DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE fpSpread1_Sheet1 AS FarPoint.Win.Spread.SheetView NO-UNDO.
  DEFINE PUBLIC VARIABLE fpSpread1 AS FarPoint.Win.Spread.FpSpread NO-UNDO.
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

  DEF VAR iDefaultFontSize    AS INT  NO-UNDO.
  DEF VAR cDefaultFont        AS CHAR NO-UNDO.

  DEFINE VARIABLE  bDisableTextChanged AS LOG NO-UNDO INIT YES.

	CONSTRUCTOR PUBLIC JBoxFpSpread (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
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

	CONSTRUCTOR PUBLIC JBoxFpSpread ( INPUT ihWinProc     AS HANDLE,
                                    INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
	 
    THIS-OBJECT(?,ihWinProc,ihPlaceHolder,0).
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	CONSTRUCTOR PUBLIC JBoxFpSpread ( INPUT ihWinProc     AS HANDLE,
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

    THIS-OBJECT:fpSpread1 = NEW FarPoint.Win.Spread.FpSpread().
    THIS-OBJECT:fpSpread1_Sheet1 = NEW FarPoint.Win.Spread.SheetView().
    CAST(THIS-OBJECT:fpSpread1, System.ComponentModel.ISupportInitialize):BeginInit().
    CAST(THIS-OBJECT:fpSpread1_Sheet1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* fpSpread1 */
    /*  */
    THIS-OBJECT:Controls:Add(THIS-OBJECT:fpSpread1).

    /* Custom: Location, Size and Name + others: */
    THIS-OBJECT:Location = NEW System.Drawing.Point(DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"x"),DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"y")).  
    THIS-OBJECT:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS,hPlaceHolder:HEIGHT-PIXELS).
    THIS-OBJECT:Name = "JBoxFpSpread".
 
    THIS-OBJECT:fpSpread1:BackColor = System.Drawing.SystemColors:Window.
        

    THIS-OBJECT:fpSpread1:TabIndex = 0.

    DEFINE VARIABLE arrayvar2 AS FarPoint.Win.Spread.SheetView EXTENT 1 NO-UNDO.
    arrayvar2[1] = THIS-OBJECT:fpSpread1_Sheet1.
    THIS-OBJECT:fpSpread1:Sheets:AddRange(arrayvar2).
    THIS-OBJECT:fpSpread1:Size = NEW System.Drawing.Size(784, 438).
    THIS-OBJECT:fpSpread1:TabIndex = 1.
    /*  */
    /* fpSpread1_Sheet1 */
    /*  */
    THIS-OBJECT:fpSpread1_Sheet1:Reset().
    THIS-OBJECT:fpSpread1_Sheet1:SheetName = "Sheet1".

/*     THIS-OBJECT:Controls:Add(THIS-OBJECT:fpSpread1).  */
/*     CAST(THIS-OBJECT:fpSpread1:Properties, System.ComponentModel.ISupportInitialize):EndInit(). */

    /*
    /* Custom: Event handlers: */
    THIS-OBJECT:fpSpread1:TextChanged:Subscribe(THIS-OBJECT:handler_TextChanged).
    THIS-OBJECT:fpSpread1:Leave:Subscribe(THIS-OBJECT:handler_Leave).
    THIS-OBJECT:fpSpread1:KeyDown:Subscribe(THIS-OBJECT:handler_KeyDown).
    THIS-OBJECT:fpSpread1:KeyUp:Subscribe(THIS-OBJECT:handler_KeyUp).
    THIS-OBJECT:fpSpread1:PreviewKeyDown:Subscribe(THIS-OBJECT:handler_PreviewKeyDown).
    THIS-OBJECT:fpSpread1:EditValueChanged:Subscribe(THIS-OBJECT:handler_EditValueChanged).
    */

    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.

  END METHOD.
 
  /* Mandatory methods: */

  METHOD PUBLIC FarPoint.Win.Spread.FpSpread getDotNetWidget():
    RETURN fpSpread1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(fpSpread1:GetType()).
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
           .

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

  METHOD PUBLIC CHARACTER DEFAULT():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.

  /* Methods invoked from jukebox (via window super): */

  METHOD PUBLIC CHARACTER dotNetDisplay():
    DEF VAR cEntityId AS CHAR NO-UNDO.
    DEF VAR ix        AS INT  NO-UNDO.
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR dDate     AS DATE FORMAT "99/99/9999" NO-UNDO.

/*    DEFINE VARIABLE task1 AS C1.Win.C1GanttView.Task NO-UNDO.           */
/*                                                                        */
/*    bDisableTextChanged = YES.                                          */
/*                                                                        */
/*    IF hBuffer:AVAIL THEN                                               */
/*      addTask(hBuffer:BUFFER-FIELD(ENTRY(1,cBufferDisplay)):BUFFER-VALUE*/
/*             ,hBuffer:BUFFER-FIELD(ENTRY(2,cBufferDisplay)):BUFFER-VALUE*/
/*             ,hBuffer:BUFFER-FIELD(ENTRY(3,cBufferDisplay)):BUFFER-VALUE*/
/*             ,hBuffer:BUFFER-FIELD(ENTRY(4,cBufferDisplay)):BUFFER-VALUE*/
/*             ,0                                                         */
/*              ).                                                        */


/*     fpSpread1:Font = NEW System.Drawing.Font(cDefaultFont,iDefaultFontSize,System.Drawing.FontStyle:Regular). */
/*                                                                                                                  */
/*     MESSAGE cBufferDisplay                 */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

    bApplyReturn = NO.

/*     IF hBuffer:AVAIL THEN                                                         */
/*       fpSpread1:EditValue = hBuffer:BUFFER-FIELD(cBufferDisplay):BUFFER-VALUE. */
/*     ELSE                                                                          */
/*       fpSpread1:EditValue = ?.                                                 */
/*     fpSpread1:Enabled = hBuffer:AVAIL. */

    RETURN "".
  END.

  METHOD PUBLIC CHARACTER dotNetNew():
/*     IF CAN-QUERY(hPlaceholder,"initial") THEN                                   */
/*       fpSpread1:EditValue = hPlaceholder:INITIAL.                            */
/*     ELSE fpSpread1:EditValue = hBuffer:BUFFER-FIELD(cBufferDisplay):INITIAL. */
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetFocus():
    THIS-OBJECT:ActiveControl = fpSpread1.
    fpSpread1:Focus().
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
    fpSpread1:Enabled = ibEnable.
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetVisible(ibVisible AS LOG):
    fpSpread1:Visible = ibVisible.
  END METHOD.

  /* (Semi-)class specific methods - event handlers: */
  METHOD PRIVATE VOID handler_TextChanged (sender AS CLASS System.Object, e AS CLASS System.EventArgs):
    IF NOT bDisableTextChanged THEN 
      RUN InvokeMethod IN hWinSuper (hPlaceHolder,"TextChanged").
    bDisableTextChanged = NO.
  END METHOD. 

  METHOD PRIVATE VOID handler_Leave (sender AS CLASS System.Object, e AS CLASS System.EventArgs):
    DEF VAR hNextTabItem AS HANDLE NO-UNDO.

/*    IF CAN-DO("return,tab,shift",cPreviewKey) THEN DO:                           */
/*      IF NOT DevExpress.Utils.KeyboardHandler.KeyboardHandler:IsShiftPressed THEN*/
/*        hNextTabItem = hPlaceHolder:NEXT-TAB-ITEM NO-ERROR.                      */
/*      ELSE                                                                       */
/*        hNextTabItem = hPlaceHolder:PREV-TAB-ITEM NO-ERROR.                      */
/*                                                                                 */
/*      IF VALID-HANDLE(hNextTabItem) THEN                                         */
/*        RUN widgetFocus IN hWinSuper (hNextTabItem).                             */
/*    END.                                                                         */
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
/*     fpSpread1:EditValue = DEC(fpSpread1:TEXT). */

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

/*     hPlaceholder:SCREEN-VALUE = STRING(fpSpread1:EditValue).  */
  
    IF VALID-HANDLE(hBrwBufQry) AND hPlaceholder:MODIFIED THEN DO:
      hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",hBrwBufQry,"toolbar","from").
      IF VALID-HANDLE(hToolbar) THEN
        DYNAMIC-FUNCTION("setToolbar",hToolbar,"modified").
      hPlaceholder:MODIFIED = YES.
    END.

    bDisableTextChanged = NO.
    RETURN "".
  END METHOD.
  
  METHOD PUBLIC LOGICAL applyTab():
    THIS-OBJECT:fpSpread1:SelectNextControl(?,NO,NO,NO,NO).
  END METHOD.

  /* Class specific methods: */

  METHOD PUBLIC VOID InitializeSpreadSheetFromHandle(INPUT phttTempTable AS HANDLE
                                                    ,INPUT ibLabels AS LOG):
    DEFINE VARIABLE hQuery      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hBuff       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hField      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cFieldValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFieldCnt   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRow        AS INTEGER   NO-UNDO INITIAL 0.
    
    IF NOT VALID-HANDLE(phttTempTable) THEN 
      RETURN.
    
    IF phttTempTable:TYPE = "buffer" THEN
      hBuff = phttTempTable.
    ELSE IF phttTempTable:TYPE = "query" THEN
      hBuff = phttTempTable:GET-BUFFER-HANDLE(1).
    ELSE IF phttTempTable:TYPE = "browse" THEN
      hBuff = phttTempTable:QUERY:GET-BUFFER-HANDLE(1).
    ELSE
      hBuff = phttTempTable:DEFAULT-BUFFER-HANDLE. 
    IF ibLabels THEN DO: 
      DO iFieldCnt = 1 TO hBuff:NUM-FIELDS:
        ASSIGN 
          hField      = hBuff:BUFFER-FIELD(iFieldCnt).
          cFieldValue = IF hField:LABEL = ? THEN
                             ""
                        ELSE STRING(hField:LABEL).                                         
        fpSpread1_Sheet1:SetValue(iRow, iFieldCnt - 1, cFieldValue).                                                        
      END.
      iRow = iRow + 1.
    END.
       
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuff).
    hQuery:QUERY-PREPARE("FOR EACH " + hBuff:NAME).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE hBuff:AVAILABLE:
      DO iFieldCnt = 1 TO hBuff:NUM-FIELDS:
        ASSIGN 
          hField      = hBuff:BUFFER-FIELD(iFieldCnt).
          cFieldValue = IF hField:BUFFER-VALUE = ? THEN
                             ""
                        ELSE STRING(hField:BUFFER-VALUE).                                         
        fpSpread1_Sheet1:SetValue(iRow, iFieldCnt - 1, cFieldValue).                                                        
      END.
      iRow = iRow + 1.
      hQuery:GET-NEXT().    
    END.
        
  END METHOD.  
  
  METHOD PUBLIC VOID InitializeSpreadSheetFromFile(INPUT pcFileName AS CHARACTER, 
                                                   INPUT pcDelimiter AS CHARACTER):
    DEFINE VARIABLE styleInfo    AS FarPoint.Win.Spread.StyleInfo NO-UNDO.
    DEFINE VARIABLE headerCell   AS FarPoint.Win.Spread.Cell NO-UNDO.
    DEFINE VARIABLE cHeaderLine  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCnt         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iColumnWidth AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cNewLIne     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDelimiter   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDotNetTab   AS CHARACTER NO-UNDO INITIAL "\t".
    DEFINE VARIABLE c4GLTab      AS CHARACTER NO-UNDO INITIAL "~t".
                   
    IF pcFileName = "" OR pcFileName = ? THEN 
      RETURN.    
      
    INPUT FROM VALUE(pcFileName) NO-ECHO.
    IMPORT UNFORMATTED cHeaderLine.
    INPUT CLOSE.  
          
    ASSIGN       
      cNewLine   = System.Environment:NewLine
      cDelimiter = IF pcDelimiter = c4GLTab THEN 
                        cDotNetTab 
                   ELSE pcDelimiter. 
    
    fpSpread1_Sheet1:LoadTextFile(pcFileName, FarPoint.Win.Spread.TextFileFlags:Unformatted, FarPoint.Win.Spread.Model.IncludeHeaders:ColumnHeadersCustomOnly, cNewLine, "", cDelimiter). 
    
    DO iCnt = 1 TO NUM-ENTRIES(cHeaderLine, pcDelimiter):
      headerCell = fpSpread1_Sheet1:ColumnHeader:Cells:Get(0, iCnt - 1).
      headerCell:HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment:Left.     
      headerCell:Font = new System.Drawing.Font(cDefaultFont, iDefaultFontSize, System.Drawing.FontStyle:Bold).
      iColumnWidth = fpSpread1_Sheet1:GetPreferredColumnWidth(iCnt - 1, false, false, false).
      fpSpread1_Sheet1:Columns[iCnt - 1]:Width = iColumnWidth.
    END.
  END.


	DESTRUCTOR PUBLIC JBoxFpSpread ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
END CLASS.
