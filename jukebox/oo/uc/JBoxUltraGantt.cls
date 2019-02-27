USING Progress.Lang.*.
USING Progress.Util.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING System.TimeSpan.*.
USING Infragistics.Win.*. 
USING Infragistics.Win.UltraWinGanttView.*. 
USING Infragistics.Win.UltraWinSchedule.*. 
USING Infragistics.Win.UltraWinGanttView.UltraGanttView.*. 


USING uc.* FROM PROPATH.

/* Ref: http://help.infragistics.com/Help/Doc/WinForms/2012.1/CLR2.0/html/WinGanttView_Adding_Tasks_to_WinGanttView.html */
/* Ref: http://www.infragistics.com/help/windows-forms/Infragistics4.Win.UltraWinGanttView.v14.1~Infragistics.Win.UltraWinGanttView_namespace.html */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxUltraGantt INHERITS uc.JBoxUCbase: 
	  			
  DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PUBLIC  VARIABLE ultraGanttView1 AS Infragistics.Win.UltraWinGanttView.UltraGanttView NO-UNDO.
  DEFINE VARIABLE  bDisableTextChanged    AS LOG NO-UNDO INIT YES.
  DEFINE PUBLIC VARIABLE oProject AS Infragistics.Win.UltraWinSchedule.Project NO-UNDO.
  DEFINE PUBLIC VARIABLE oTask    AS Infragistics.Win.UltraWinSchedule.Task NO-UNDO.
  DEFINE PUBLIC VARIABLE oOwner   AS Infragistics.Win.UltraWinSchedule.Owner NO-UNDO.

  CONSTRUCTOR PUBLIC JBoxUltraGantt (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                     INPUT ihWinProc     AS HANDLE,
                                     INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                     INPUT iiDeltaWidth  AS INTEGER):
    SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxUltraGantt ( INPUT ihWinProc     AS HANDLE,
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
    THIS-OBJECT:ultraGanttView1:GridAreaWidth = 400.
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
    
   
    THIS-OBJECT:ultraGanttView1:Location = NEW System.Drawing.Point(1,1).
    THIS-OBJECT:ultraGanttView1:Name = "ultraGanttView1".
    THIS-OBJECT:ultraGanttView1:Size = NEW System.Drawing.Size(510, 207).
    THIS-OBJECT:ultraGanttView1:TabIndex = 0.
    THIS-OBJECT:ultraGanttView1:Text = "ultraGanttView1".
    THIS-OBJECT:ultraGanttView1:VerticalSplitterMinimumResizeWidth = 10.
    /*  */
    /* UltraGantt */
    /*  */
    THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGanttView1).
    THIS-OBJECT:Name = "UltraGantt".
    THIS-OBJECT:Size = NEW System.Drawing.Size(537, 232).
/*    SUPER:InitializeComponent().*/
    
    /*
	    THIS-OBJECT:ultraGanttView1:CalendarInfo:CustomTaskColumns:Add("Truck",TypeHelper:GetType( "System.string"), False, "Truck 123").
  */
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

    CAST(THIS-OBJECT:ultraGanttView1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
    
/*    THIS-OBJECT:ResumeLayout(FALSE).*/
/*    THIS-OBJECT:PerformLayout().    */
/*    CATCH e AS Progress.Lang.Error: */
/*        UNDO, THROW e.              */
/*    END CATCH.                      */

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
                                   

    oProject = THIS-OBJECT:ultraGanttView1:CalendarInfo:Projects:Add(icTitle, idBase). 
    oProject:Key = icKey. 

  END METHOD.
  
  METHOD PUBLIC Infragistics.Win.UltraWinSchedule.Task addTask(INPUT idStart     AS DATETIME,
                                 INPUT iiDaySpan   AS INT,
                                 INPUT icText      AS CHAR,
                                 INPUT icParent    AS CHAR):

    DEFINE VARIABLE oTimeSpan AS System.TimeSpan NO-UNDO.
    oTimeSpan = NEW System.TimeSpan(iiDaySpan,0,0,0).
                                   
    RETURN THIS-OBJECT:ultraGanttView1:CalendarInfo:Tasks:Add(idStart,oTimeSpan,icText,icParent).
  
  END METHOD.

METHOD PUBLIC Infragistics.Win.UltraWinSchedule.Task addSubTask(
                                INPUT ioTask      AS Infragistics.Win.UltraWinSchedule.Task,
                                INPUT idStart     AS DATETIME,
                                INPUT iiDaySpan   AS INT,
                                INPUT icText      AS CHAR):

    IF NOT VALID-OBJECT(ioTask) THEN LEAVE. 

    DEF VAR oTimeSpan  AS System.TimeSpan NO-UNDO.
    DEF VAR oSubTask   AS Infragistics.Win.UltraWinSchedule.Task NO-UNDO.
    
    oTimeSpan = NEW System.TimeSpan(iiDaySpan,0,0,0).
    oSubTask  = ioTask:Tasks:Add(idStart,oTimeSpan,icText).
    
    RETURN oSubTask.
END METHOD.


METHOD PUBLIC Infragistics.Win.UltraWinSchedule.Task addTask
                                (INPUT idStart     AS DATETIME,
                                 INPUT iiDaySpan   AS INT,
                                 INPUT icText      AS CHAR,
                                 INPUT icParent    AS CHAR,
                                 INPUT icRole      AS CHAR,
                                 INPUT icResource  AS CHAR):

    DEF VAR oTimeSpan  AS System.TimeSpan NO-UNDO.
    DEF VAR RESOURCE   AS Infragistics.Win.UltraWinSchedule.Owner NO-UNDO.
    DEF VAR ioTask     AS Infragistics.Win.UltraWinSchedule.Task NO-UNDO.
    
    ASSIGN 
        oTimeSpan = NEW System.TimeSpan(iiDaySpan,0,0,0)
        ioTask    = THIS-OBJECT:ultraGanttView1:CalendarInfo:Tasks:Add(idStart,oTimeSpan,icText,icParent)
        Resource  = THIS-OBJECT:ultraGanttView1:CalendarInfo:owners:Add(icRole,icResource).

    ioTask:Resources:add(resource).
                                   
    RETURN ioTask.
  END METHOD.


METHOD PUBLIC VOID SetGridAreaWidth (INPUT iwidth AS INT ):
    THIS-OBJECT:ultraGanttView1:GridAreaWidth = iwidth.
END. 

METHOD PUBLIC VOID SetColumnReadOnly (INPUT icColumnName AS CHAR ):
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue(icColumnName):ReadOnly = Infragistics.Win.DefaultableBoolean:True.
END. 

METHOD PUBLIC VOID SetColumnDisabled (INPUT icColumnName AS CHAR ):
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue(icColumnName):Enabled  = Infragistics.Win.DefaultableBoolean:False. 
END. 

METHOD PUBLIC VOID SetColumnEnable (INPUT icColumnName AS CHAR ):
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue(icColumnName):Enabled  = Infragistics.Win.DefaultableBoolean:True. 
END. 

METHOD PUBLIC VOID SetColumnWidth (INPUT icColumnName AS CHAR, INPUT iWidth AS INT ):
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue(icColumnName):width  = iWidth. 
END. 

METHOD PUBLIC VOID addColumn(INPUT icFieldName AS CHARACTER,
                             INPUT icDataType AS CHARACTER,
                             INPUT iiSequence AS INTEGER,
                             INPUT ilReadOnly AS LOGICAL):
 
    DEFINE VARIABLE initvalue AS CHAR NO-UNDO. 
    DEFINE VARIABLE datatype AS SYSTEM.Type NO-UNDO.
 
    IF icDataType BEGINS "CHAR" THEN 
      datatype = TypeHelper:GetType( "System.String").
    ELSE IF icDataType BEGINS "INTE" THEN 
       datatype = TypeHelper:GetType( "System.integer").
    ELSE IF icDataType BEGINS "LOG" THEN 
       datatype = TypeHelper:GetType( "System.bolean").
    ELSE IF icDataType BEGINS "Date" THEN 
       datatype = TypeHelper:GetType( "System.DateTime").
     
    THIS-OBJECT:ultraGanttView1:CalendarInfo:CustomTaskColumns:Add(icFieldName,DataType, TRUE, initValue).
    IF iiSequence NE ? THEN 
    THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue(icFieldName):VisiblePosition = iiSequence.
    
    IF ilReadOnly THEN 
    DO:
        THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue(icFieldName):ReadOnly = Infragistics.Win.DefaultableBoolean:True. 
        THIS-OBJECT:ultraGanttView1:GridSettings:ColumnSettings:GetValue(icFieldName):Enabled  = Infragistics.Win.DefaultableBoolean:False. 
    END.                                
END. 


  METHOD PUBLIC VOID SetTaskFieldValue   (INPUT ioTask  AS Infragistics.Win.UltraWinSchedule.Task,INPUT icFieldName AS CHAR,INPUT icCharacter AS  CHARACTER ):
     ioTask:setCustomProperty(icFieldName,icCharacter).
  /*   message oSubTask:getCustomProperty("Task Type") view-as alert-box. */

  END.
   
  METHOD PUBLIC VOID SetTaskFieldValue   (INPUT ioTask  AS Infragistics.Win.UltraWinSchedule.Task,INPUT icFieldName AS CHAR,INPUT icCharacter AS  DATETIME ):
     ioTask:setCustomProperty(icFieldName,icCharacter).
  END.
  
  METHOD PUBLIC VOID SetTaskFieldValue   (INPUT ioTask  AS Infragistics.Win.UltraWinSchedule.Task,INPUT icFieldName AS CHAR,INPUT icCharacter AS  INTEGER ):
     ioTask:setCustomProperty(icFieldName,icCharacter).
  END.
  
  METHOD PUBLIC VOID SetTaskConstraint   (INPUT ioTask  AS Infragistics.Win.UltraWinSchedule.Task,INPUT icdConstraintDateTime AS DATETIME, INPUT icType AS CHARACTER ):

    /* icType = StartNoEarlierThan, FinishNoLaterThan,AsSoonAsPossible */

    IF icType = "FinishNoLaterThan" THEN 
      ioTask:Constraint = Infragistics.Win.UltraWinSchedule.TaskConstraint:FinishNoLaterThan.
    ELSE IF icType = "AsSoonAsPossible"  THEN
      ioTask:Constraint = Infragistics.Win.UltraWinSchedule.TaskConstraint:AsSoonAsPossible.
    ELSE IF icType = "StartNoEarlierThan"  THEN
      ioTask:Constraint = Infragistics.Win.UltraWinSchedule.TaskConstraint:StartNoEarlierThan.
      
    ioTask:ConstraintDateTime =  icdConstraintDateTime.

  END. 

  METHOD PUBLIC VOID SetTaskDeadLine   (INPUT ioTask  AS Infragistics.Win.UltraWinSchedule.Task,INPUT icdConstraintDateTime AS DATETIME ):
     ioTask:DeadLine =  icdConstraintDateTime.
  END. 
  
  METHOD PUBLIC VOID addTaskDependency(INPUT ioFromTask AS Infragistics.Win.UltraWinSchedule.Task,
                                       INPUT ioToTask   AS Infragistics.Win.UltraWinSchedule.Task  ,
                                       INPUT icType     AS  CHARACTER ):

    /*  FinishToFinish The completion of the dependent task is contingent on the completion of the predecessor.  
        FinishToStart (Default) The beginning of the dependent task is contingent on the completion of the predecessor.  
        StartToFinish The completion of the dependent task is contingent on the beginning of the predecessor.  
        StartToStart The beginning of the dependent task is contingent on the beginning of the predecessor.      */

    IF NOT VALID-OBJECT(ioFromTask) THEN LEAVE. 
    IF NOT VALID-OBJECT(ioToTask) THEN LEAVE. 

    IF icType = "FF" THEN 
        ioToTask:Dependencies:Add(ioFromTask,Infragistics.Win.UltraWinSchedule.TaskDependencyType:FinishToFinish). 
    ELSE IF icType = "FS" OR icType = "" OR icType = ? THEN 
        ioToTask:Dependencies:Add(ioFromTask,Infragistics.Win.UltraWinSchedule.TaskDependencyType:FinishToStart). 
    ELSE IF icType = "SF" THEN 
        ioToTask:Dependencies:Add(ioFromTask,Infragistics.Win.UltraWinSchedule.TaskDependencyType:StartToFinish). 
    ELSE IF icType = "SS" THEN 
        ioToTask:Dependencies:Add(ioFromTask,Infragistics.Win.UltraWinSchedule.TaskDependencyType:StartToStart). 
      
END. 

  METHOD PUBLIC Infragistics.Win.UltraWinSchedule.Task addSubTask(
                                INPUT ioTask      AS Infragistics.Win.UltraWinSchedule.Task,
                                INPUT idStart     AS DATETIME,
                                INPUT iiDaySpan   AS INT,
                                INPUT icText      AS CHAR,
                                INPUT icResKey    AS CHAR,
                                INPUT icResource  AS CHAR):


    DEF VAR oTimeSpan  AS System.TimeSpan NO-UNDO.
    DEF VAR RESOURCE   AS Infragistics.Win.UltraWinSchedule.Owner NO-UNDO.
    DEF VAR oSubTask   AS Infragistics.Win.UltraWinSchedule.Task NO-UNDO.

    IF NOT VALID-OBJECT(ioTask) THEN LEAVE. 
    
    oTimeSpan = NEW System.TimeSpan(iiDaySpan,0,0,0).
    oSubTask  = ioTask:Tasks:Add(idStart,oTimeSpan,icText).
    
    IF icResource NE "" THEN DO:
      IF icResKey = "" THEN icResKey = GUID.  
      Resource  = THIS-OBJECT:ultraGanttView1:CalendarInfo:owners:Add(icResKey,icResource).
      oSubTask:Resources:add(resource).
    END.  

    RETURN oSubTask.
  END METHOD.
  
  
   METHOD PUBLIC Infragistics.Win.UltraWinSchedule.Owner addResource( INPUT ioTask  AS Infragistics.Win.UltraWinSchedule.Task,
                                INPUT icRole      AS CHAR,
                                INPUT icResource  AS CHAR):
    
    IF NOT VALID-OBJECT(ioTask) THEN LEAVE. 
    
    DEF VAR Resource AS Infragistics.Win.UltraWinSchedule.Owner.
    Resource = THIS-OBJECT:ultraGanttView1:CalendarInfo:owners:add(icRole,icResource).
    ioTask:Resources:add(resource).
     
    RETURN Resource. 
  END METHOD.


  METHOD PUBLIC VOID ShowProject():
    THIS-OBJECT:ultraGanttView1:Project = oProject. /* THIS-OBJECT:ultraGanttView1:CalendarInfo:Projects(1). */ 
  END METHOD.  

	
END CLASS.
