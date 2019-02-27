USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING uc.* FROM PROPATH.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxC1Gantt INHERITS uc.JBoxUCbase: 
	
	DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PUBLIC VARIABLE c1GanttView1     AS C1.Win.C1GanttView.C1GanttView NO-UNDO.
  DEFINE VARIABLE  bDisableTextChanged    AS LOG NO-UNDO INIT YES.

  CONSTRUCTOR PUBLIC JBoxC1Gantt (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                        INPUT ihWinProc     AS HANDLE,
                                        INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                        INPUT iiDeltaWidth  AS INTEGER):
    SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
  END CONSTRUCTOR.
  CONSTRUCTOR PUBLIC JBoxC1Gantt ( INPUT ihWinProc     AS HANDLE,
                                   INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
    SUPER(?,ihWinProc,ihPlaceHolder,0).
  END CONSTRUCTOR.

	METHOD PUBLIC OVERRIDE VOID InitializeComponent(  ):
    /* Custom: */
    hPlaceHolder:WIDTH-PIXELS = hPlaceHolder:WIDTH-PIXELS + iDeltaWidth.

    THIS-OBJECT:c1GanttView1 = NEW C1.Win.C1GanttView.C1GanttView().
/*     CAST(THIS-OBJECT:c1GanttView1:Properties:VistaTimeProperties, System.ComponentModel.ISupportInitialize):BeginInit(). */
/*     CAST(THIS-OBJECT:c1GanttView1:Properties, System.ComponentModel.ISupportInitialize):BeginInit(). */
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* c1GanttView1 */
    /*  */
    THIS-OBJECT:Controls:Add(THIS-OBJECT:c1GanttView1).

    /* Custom: Location, Size and Name + others: */
/*    THIS-OBJECT:Location = NEW System.Drawing.Point(DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"x"),DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"y")).*/
/*    THIS-OBJECT:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS,hPlaceHolder:HEIGHT-PIXELS).                                                        */
    THIS-OBJECT:Name = "JBoxC1Gantt".
 
    THIS-OBJECT:c1GanttView1:BackColor = System.Drawing.SystemColors:Window.
        

    THIS-OBJECT:c1GanttView1:StartDate = Progress.Util.DateTimeHelper:Create(2013, 6, 29, 0, 0, 0, 0).
    THIS-OBJECT:c1GanttView1:TabIndex = 0.

     THIS-OBJECT:Controls:Add(THIS-OBJECT:c1GanttView1).

    /*
    /* Custom: Event handlers: */
    THIS-OBJECT:c1GanttView1:TextChanged:Subscribe(THIS-OBJECT:handler_TextChanged).
    THIS-OBJECT:c1GanttView1:Leave:Subscribe(THIS-OBJECT:handler_Leave).
    THIS-OBJECT:c1GanttView1:KeyDown:Subscribe(THIS-OBJECT:handler_KeyDown).
    THIS-OBJECT:c1GanttView1:KeyUp:Subscribe(THIS-OBJECT:handler_KeyUp).
    THIS-OBJECT:c1GanttView1:PreviewKeyDown:Subscribe(THIS-OBJECT:handler_PreviewKeyDown).
    THIS-OBJECT:c1GanttView1:EditValueChanged:Subscribe(THIS-OBJECT:handler_EditValueChanged).
    */
    
    SUPER:InitializeComponent().

/*    
    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.
*/
  END METHOD.
 
  /* Mandatory methods: */

  METHOD PUBLIC C1.Win.C1GanttView.C1GanttView getDotNetWidget():
    RETURN c1GanttView1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(c1GanttView1:GetType()).
  END METHOD.

/*
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
*/
  METHOD PUBLIC CHARACTER DEFAULT():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.

  /* Methods invoked from jukebox (via window super): */

  METHOD PUBLIC OVERRIDE CHARACTER dotNetDisplay():
    DEF VAR cEntityId AS CHAR NO-UNDO.
    DEF VAR ix        AS INT  NO-UNDO.
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR dDate     AS DATE FORMAT "99/99/9999" NO-UNDO.

    DEFINE VARIABLE task1 AS C1.Win.C1GanttView.Task NO-UNDO.

    bDisableTextChanged = YES.
        
    IF hBuffer:AVAIL THEN
      addTask(hBuffer:BUFFER-FIELD(ENTRY(1,cBufferDisplay)):BUFFER-VALUE
             ,hBuffer:BUFFER-FIELD(ENTRY(2,cBufferDisplay)):BUFFER-VALUE
             ,hBuffer:BUFFER-FIELD(ENTRY(3,cBufferDisplay)):BUFFER-VALUE 
             ,hBuffer:BUFFER-FIELD(ENTRY(4,cBufferDisplay)):BUFFER-VALUE
             ,0
              ).


/*     c1GanttView1:Font = NEW System.Drawing.Font(cDefaultFont,iDefaultFontSize,System.Drawing.FontStyle:Regular). */
/*                                                                                                                  */
/*     MESSAGE cBufferDisplay                 */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

    bApplyReturn = NO.

/*     IF hBuffer:AVAIL THEN                                                         */
/*       c1GanttView1:EditValue = hBuffer:BUFFER-FIELD(cBufferDisplay):BUFFER-VALUE. */
/*     ELSE                                                                          */
/*       c1GanttView1:EditValue = ?.                                                 */
/*     c1GanttView1:Enabled = hBuffer:AVAIL. */

    RETURN "".
  END.
/*
  METHOD PUBLIC CHARACTER dotNetNew():
/*     IF CAN-QUERY(hPlaceholder,"initial") THEN                                   */
/*       c1GanttView1:EditValue = hPlaceholder:INITIAL.                            */
/*     ELSE c1GanttView1:EditValue = hBuffer:BUFFER-FIELD(cBufferDisplay):INITIAL. */
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetFocus():
    THIS-OBJECT:ActiveControl = c1GanttView1.
    c1GanttView1:Focus().
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
    c1GanttView1:Enabled = ibEnable.
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetVisible(ibVisible AS LOG):
    c1GanttView1:Visible = ibVisible.
  END METHOD.
*/
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
/*     c1GanttView1:EditValue = DEC(c1GanttView1:TEXT). */

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

/*     hPlaceholder:SCREEN-VALUE = STRING(c1GanttView1:EditValue).  */
  
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

  METHOD PUBLIC LOGICAL setColumns(icColumns AS CHAR):
    DEF VAR ix AS INT NO-UNDO.

    DEFINE VARIABLE taskPropertyColumn1 AS C1.Win.C1GanttView.TaskPropertyColumn NO-UNDO.

    DO ix = 1 TO NUM-ENTRIES(icColumns,";"):
      taskPropertyColumn1 = NEW C1.Win.C1GanttView.TaskPropertyColumn().
      taskPropertyColumn1:ID = ix.
      taskPropertyColumn1:Caption = ENTRY(1,ENTRY(ix,icColumns,";"),"|").
      CASE ENTRY(2,ENTRY(ix,icColumns,";"),"|"):
        WHEN "Mode"            THEN taskPropertyColumn1:Property = C1.Win.C1GanttView.TaskProperty:Mode.
        WHEN "Name"            THEN taskPropertyColumn1:Property = C1.Win.C1GanttView.TaskProperty:Name.
        WHEN "Duration"        THEN DO:
          taskPropertyColumn1:Property = C1.Win.C1GanttView.TaskProperty:Duration.
          taskPropertyColumn1:TextAlign = System.Windows.Forms.HorizontalAlignment:Right.
          taskPropertyColumn1:Width = 50.
/*           taskPropertyColumn1:Visible = FALSE.  */
        END.
        WHEN "DurationUnits"   THEN DO:
          taskPropertyColumn1:Property = C1.Win.C1GanttView.TaskProperty:DurationUnits.
          taskPropertyColumn1:Visible = FALSE.
        END.
        WHEN "Start"           THEN taskPropertyColumn1:Property = C1.Win.C1GanttView.TaskProperty:Start.
        WHEN "Finish"         THEN taskPropertyColumn1:Property = C1.Win.C1GanttView.TaskProperty:Finish.
        WHEN "PercentComplete" THEN DO:
          taskPropertyColumn1:Property = C1.Win.C1GanttView.TaskProperty:PercentComplete.
          taskPropertyColumn1:Visible = FALSE.
        END.
        WHEN "Calendar"         THEN taskPropertyColumn1:Property = C1.Win.C1GanttView.TaskProperty:Calendar.
        OTHERWISE
          MESSAGE "Invalid column type for GanttView: " ENTRY(2,ENTRY(ix,icColumns,";"),"|")
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END CASE.
      THIS-OBJECT:c1GanttView1:Columns:Add(taskPropertyColumn1).
    END.

    THIS-OBJECT:c1GanttView1:DefaultWorkingTimes:Interval_1:Empty = FALSE.
    THIS-OBJECT:c1GanttView1:DefaultWorkingTimes:Interval_1:From = Progress.Util.DateTimeHelper:Create(1, 1, 1, 9, 0, 0, 0).
    THIS-OBJECT:c1GanttView1:DefaultWorkingTimes:Interval_1:To = Progress.Util.DateTimeHelper:Create(1, 1, 1, 13, 0, 0, 0).
    THIS-OBJECT:c1GanttView1:DefaultWorkingTimes:Interval_2:Empty = FALSE.
    THIS-OBJECT:c1GanttView1:DefaultWorkingTimes:Interval_2:From = Progress.Util.DateTimeHelper:Create(1, 1, 1, 14, 0, 0, 0).
    THIS-OBJECT:c1GanttView1:DefaultWorkingTimes:Interval_2:To = Progress.Util.DateTimeHelper:Create(1, 1, 1, 18, 0, 0, 0).
    THIS-OBJECT:c1GanttView1:StartDate = Progress.Util.DateTimeHelper:Create(2013, 6, 29, 0, 0, 0, 0).

    THIS-OBJECT:c1GanttView1:Timescale:BottomTier:Align = C1.Win.C1GanttView.ScaleLabelAlignment:Center.
    THIS-OBJECT:c1GanttView1:Timescale:BottomTier:Format = "w".
    THIS-OBJECT:c1GanttView1:Timescale:BottomTier:Visible = TRUE.
    THIS-OBJECT:c1GanttView1:Timescale:MiddleTier:Format = "nnnn d".
    THIS-OBJECT:c1GanttView1:Timescale:MiddleTier:Units = C1.Win.C1GanttView.TimescaleUnits:Weeks.
    THIS-OBJECT:c1GanttView1:Timescale:MiddleTier:Visible = TRUE.

/*
    DEFINE VARIABLE taskPropertyColumn2 AS C1.Win.C1GanttView.TaskPropertyColumn NO-UNDO.
    taskPropertyColumn2 = NEW C1.Win.C1GanttView.TaskPropertyColumn().
    taskPropertyColumn2:ID = 2.
    taskPropertyColumn2:Caption = "Start".
    taskPropertyColumn2:Property = C1.Win.C1GanttView.TaskProperty:Start.
    THIS-OBJECT:c1GanttView1:Columns:Add(taskPropertyColumn2).

    DEFINE VARIABLE taskPropertyColumn3 AS C1.Win.C1GanttView.TaskPropertyColumn NO-UNDO.
    taskPropertyColumn3 = NEW C1.Win.C1GanttView.TaskPropertyColumn().
    taskPropertyColumn3:ID = 3.
    taskPropertyColumn3:Caption = "Finish".
    taskPropertyColumn3:Property = C1.Win.C1GanttView.TaskProperty:Finish.
    THIS-OBJECT:c1GanttView1:Columns:Add(taskPropertyColumn3).
*/
      
  END METHOD.

  METHOD PUBLIC LOGICAL addTask(iiTaskId          AS INTEGER
                               ,icTaskName        AS CHARACTER
                               ,idStart           AS DATE
                               ,idEnd             AS DATE
                               ,iiPercentComplete AS INTEGER
                                ):

    DEFINE VARIABLE task1 AS C1.Win.C1GanttView.Task NO-UNDO.
    task1 = NEW C1.Win.C1GanttView.Task().

    task1:ID = iiTaskId.
    task1:Name = icTaskName.
    task1:PercentComplete = iiPercentComplete.
    task1:Start = Progress.Util.DateTimeHelper:Create(YEAR(idStart), MONTH(idStart), DAY(idStart), 0, 0, 0, 0).
    task1:Finish = Progress.Util.DateTimeHelper:Create(YEAR(idEnd), MONTH(idEnd), DAY(idEnd), 0, 0, 0, 0).
        
    THIS-OBJECT:c1GanttView1:Tasks:Add(task1).
      
  END METHOD.

/*
  METHOD PUBLIC LOGICAL applyTab():
    THIS-OBJECT:c1GanttView1:SelectNextControl(?,NO,NO,NO,NO).
  END METHOD.

	DESTRUCTOR PUBLIC JBoxC1Gantt ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
	*/
END CLASS.
