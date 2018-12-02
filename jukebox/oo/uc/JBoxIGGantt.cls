USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING uc.* FROM PROPATH.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxIGGantt INHERITS uc.JBoxUCbase: 
	
	DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraGanttView1 AS Infragistics.Win.UltraWinGanttView.UltraGanttView NO-UNDO.
  DEFINE VARIABLE  bDisableTextChanged    AS LOG NO-UNDO INIT YES.

  CONSTRUCTOR PUBLIC JBoxIGGantt (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                        INPUT ihWinProc     AS HANDLE,
                                        INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                        INPUT iiDeltaWidth  AS INTEGER):
    SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
  END CONSTRUCTOR.
  CONSTRUCTOR PUBLIC JBoxIGGantt ( INPUT ihWinProc     AS HANDLE,
                                   INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
    SUPER(?,ihWinProc,ihPlaceHolder,0).
  END CONSTRUCTOR.

	METHOD PUBLIC OVERRIDE VOID InitializeComponent(  ):

    THIS-OBJECT:ultraGanttView1 = NEW Infragistics.Win.UltraWinGanttView.UltraGanttView().
    CAST(THIS-OBJECT:ultraGanttView1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* ultraGanttView1 */
    /*  */
    THIS-OBJECT:ultraGanttView1:GridAreaWidth = 226.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("Constraint"):VisiblePosition = 6.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("ConstraintDateTime"):VisiblePosition = 7.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("Dependencies"):VisiblePosition = 4.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("Deadline"):VisiblePosition = 8.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("Duration"):VisiblePosition = 1.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("EndDateTime"):VisiblePosition = 3.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("Milestone"):VisiblePosition = 9.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("Name"):VisiblePosition = 0.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("Notes"):VisiblePosition = 10.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("PercentComplete"):VisiblePosition = 11.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("Resources"):VisiblePosition = 5.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("StartDateTime"):VisiblePosition = 2.
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue("RowNumber"):VisiblePosition = 12.
    THIS-OBJECT:ultraGanttView1:Location = NEW System.Drawing.Point(7, 14).
    THIS-OBJECT:ultraGanttView1:Name = "ultraGanttView1".
    THIS-OBJECT:ultraGanttView1:Size = NEW System.Drawing.Size(588, 286).
    THIS-OBJECT:ultraGanttView1:TabIndex = 0.
    THIS-OBJECT:ultraGanttView1:Text = "ultraGanttView1".
    THIS-OBJECT:ultraGanttView1:VerticalSplitterMinimumResizeWidth = 10.
    /*  */
    /* testIGcannt */
    /*  */
    THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGanttView1).
    THIS-OBJECT:Name = "testIGcannt".

    /*
    /* Custom: Event handlers: */
    THIS-OBJECT:ultraGanttView1:TextChanged:Subscribe(THIS-OBJECT:handler_TextChanged).
    THIS-OBJECT:ultraGanttView1:Leave:Subscribe(THIS-OBJECT:handler_Leave).
    THIS-OBJECT:ultraGanttView1:KeyDown:Subscribe(THIS-OBJECT:handler_KeyDown).
    THIS-OBJECT:ultraGanttView1:KeyUp:Subscribe(THIS-OBJECT:handler_KeyUp).
    THIS-OBJECT:ultraGanttView1:PreviewKeyDown:Subscribe(THIS-OBJECT:handler_PreviewKeyDown).
    THIS-OBJECT:ultraGanttView1:EditValueChanged:Subscribe(THIS-OBJECT:handler_EditValueChanged).
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

  METHOD PUBLIC Infragistics.Win.UltraWinGanttView.UltraGanttView getDotNetWidget():
    RETURN ultraGanttView1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(ultraGanttView1:GetType()).
  END METHOD.


  /* Methods invoked from jukebox (via window super): */

  /* (Semi-)class specific methods - event handlers: */

  /* Class specific methods: */

  METHOD PUBLIC LOGICAL addProject(icTitle AS CHAR,
                                   idBase  AS DATE,
                                   icKey   AS CHAR).
                                   
    DEF VAR oProject AS Infragistics.Win.UltraWinSchedule.Project NO-UNDO.

    THIS-OBJECT:ultraGanttView1:CalendarInfo:Projects:Add(icTitle, idBase). 

/*                                   
Dim quarterlyProject As Project = Me.ultraCalendarInfo1.Projects.Add("QuartlerlyProject", DateTime.Today) 
quarterlyProject.Key = "projkey1" 
*/
  END METHOD.

  METHOD PUBLIC LOGICAL setColumns(icColumns AS CHAR):
    DEF VAR ix AS INT NO-UNDO.

/*
    DEFINE VARIABLE taskPropertyColumn1 AS Infragistics.Win.UltraWinGanttView.TaskPropertyColumn NO-UNDO.

    DO ix = 1 TO NUM-ENTRIES(icColumns,";"):
      taskPropertyColumn1 = NEW Infragistics.Win.UltraWinGanttView.TaskPropertyColumn().
      taskPropertyColumn1:ID = ix.
      taskPropertyColumn1:Caption = ENTRY(1,ENTRY(ix,icColumns,";"),"|").
      CASE ENTRY(2,ENTRY(ix,icColumns,";"),"|"):
        WHEN "Mode"            THEN taskPropertyColumn1:Property = Infragistics.Win.UltraWinGanttView.TaskProperty:Mode.
        WHEN "Name"            THEN taskPropertyColumn1:Property = Infragistics.Win.UltraWinGanttView.TaskProperty:Name.
        WHEN "Duration"        THEN DO:
          taskPropertyColumn1:Property = Infragistics.Win.UltraWinGanttView.TaskProperty:Duration.
          taskPropertyColumn1:TextAlign = System.Windows.Forms.HorizontalAlignment:Right.
          taskPropertyColumn1:Width = 50.
/*           taskPropertyColumn1:Visible = FALSE.  */
        END.
        WHEN "DurationUnits"   THEN DO:
          taskPropertyColumn1:Property = Infragistics.Win.UltraWinGanttView.TaskProperty:DurationUnits.
          taskPropertyColumn1:Visible = FALSE.
        END.
        WHEN "Start"           THEN taskPropertyColumn1:Property = Infragistics.Win.UltraWinGanttView.TaskProperty:Start.
        WHEN "Finish"         THEN taskPropertyColumn1:Property = Infragistics.Win.UltraWinGanttView.TaskProperty:Finish.
        WHEN "PercentComplete" THEN DO:
          taskPropertyColumn1:Property = Infragistics.Win.UltraWinGanttView.TaskProperty:PercentComplete.
          taskPropertyColumn1:Visible = FALSE.
        END.
        WHEN "Calendar"         THEN taskPropertyColumn1:Property = Infragistics.Win.UltraWinGanttView.TaskProperty:Calendar.
        OTHERWISE
          MESSAGE "Invalid column type for GanttView: " ENTRY(2,ENTRY(ix,icColumns,";"),"|")
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END CASE.
      THIS-OBJECT:ultraGanttView1:Columns:Add(taskPropertyColumn1).
    END.

    THIS-OBJECT:ultraGanttView1:DefaultWorkingTimes:Interval_1:Empty = FALSE.
    THIS-OBJECT:ultraGanttView1:DefaultWorkingTimes:Interval_1:From = Progress.Util.DateTimeHelper:Create(1, 1, 1, 9, 0, 0, 0).
    THIS-OBJECT:ultraGanttView1:DefaultWorkingTimes:Interval_1:To = Progress.Util.DateTimeHelper:Create(1, 1, 1, 13, 0, 0, 0).
    THIS-OBJECT:ultraGanttView1:DefaultWorkingTimes:Interval_2:Empty = FALSE.
    THIS-OBJECT:ultraGanttView1:DefaultWorkingTimes:Interval_2:From = Progress.Util.DateTimeHelper:Create(1, 1, 1, 14, 0, 0, 0).
    THIS-OBJECT:ultraGanttView1:DefaultWorkingTimes:Interval_2:To = Progress.Util.DateTimeHelper:Create(1, 1, 1, 18, 0, 0, 0).
    THIS-OBJECT:ultraGanttView1:StartDate = Progress.Util.DateTimeHelper:Create(2013, 6, 29, 0, 0, 0, 0).

    THIS-OBJECT:ultraGanttView1:Timescale:BottomTier:Align = Infragistics.Win.UltraWinGanttView.ScaleLabelAlignment:Center.
    THIS-OBJECT:ultraGanttView1:Timescale:BottomTier:Format = "w".
    THIS-OBJECT:ultraGanttView1:Timescale:BottomTier:Visible = TRUE.
    THIS-OBJECT:ultraGanttView1:Timescale:MiddleTier:Format = "nnnn d".
    THIS-OBJECT:ultraGanttView1:Timescale:MiddleTier:Units = Infragistics.Win.UltraWinGanttView.TimescaleUnits:Weeks.
    THIS-OBJECT:ultraGanttView1:Timescale:MiddleTier:Visible = TRUE.
*/

/*
    DEFINE VARIABLE taskPropertyColumn2 AS Infragistics.Win.UltraWinGanttView.TaskPropertyColumn NO-UNDO.
    taskPropertyColumn2 = NEW Infragistics.Win.UltraWinGanttView.TaskPropertyColumn().
    taskPropertyColumn2:ID = 2.
    taskPropertyColumn2:Caption = "Start".
    taskPropertyColumn2:Property = Infragistics.Win.UltraWinGanttView.TaskProperty:Start.
    THIS-OBJECT:ultraGanttView1:Columns:Add(taskPropertyColumn2).

    DEFINE VARIABLE taskPropertyColumn3 AS Infragistics.Win.UltraWinGanttView.TaskPropertyColumn NO-UNDO.
    taskPropertyColumn3 = NEW Infragistics.Win.UltraWinGanttView.TaskPropertyColumn().
    taskPropertyColumn3:ID = 3.
    taskPropertyColumn3:Caption = "Finish".
    taskPropertyColumn3:Property = Infragistics.Win.UltraWinGanttView.TaskProperty:Finish.
    THIS-OBJECT:ultraGanttView1:Columns:Add(taskPropertyColumn3).
*/
      
  END METHOD.

  METHOD PUBLIC LOGICAL addTask(iiTaskId          AS INTEGER
                               ,icTaskName        AS CHARACTER
                               ,idStart           AS DATE
                               ,idEnd             AS DATE
                               ,iiPercentComplete AS INTEGER
                                ):
/*
    DEFINE VARIABLE task1 AS Infragistics.Win.UltraWinGanttView.Task NO-UNDO.
    task1 = NEW Infragistics.Win.UltraWinGanttView.Task().

    task1:ID = iiTaskId.
    task1:Name = icTaskName.
    task1:PercentComplete = iiPercentComplete.
    task1:Start = Progress.Util.DateTimeHelper:Create(YEAR(idStart), MONTH(idStart), DAY(idStart), 0, 0, 0, 0).
    task1:Finish = Progress.Util.DateTimeHelper:Create(YEAR(idEnd), MONTH(idEnd), DAY(idEnd), 0, 0, 0, 0).
        
    THIS-OBJECT:ultraGanttView1:Tasks:Add(task1).
  */    
  END METHOD.

/*
  METHOD PUBLIC LOGICAL applyTab():
    THIS-OBJECT:ultraGanttView1:SelectNextControl(?,NO,NO,NO,NO).
  END METHOD.

	DESTRUCTOR PUBLIC JBoxC1Gantt ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
	*/
END CLASS.
