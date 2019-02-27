 
 /*------------------------------------------------------------------------
    File        : IGGanttForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Wed May 14 22:50:54 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS winsrc.samples.oo.IGGanttForm INHERITS Form: 
	
  DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraGanttView1 AS Infragistics.Win.UltraWinGanttView.UltraGanttView NO-UNDO.

  DEFINE PUBLIC VARIABLE oProject AS Infragistics.Win.UltraWinSchedule.Project NO-UNDO.
  DEFINE PUBLIC VARIABLE oTask    AS Infragistics.Win.UltraWinSchedule.Task NO-UNDO.
  DEFINE PUBLIC VARIABLE oOwner   AS Infragistics.Win.UltraWinSchedule.Owner NO-UNDO.
		
	CONSTRUCTOR PUBLIC IGGanttForm (  ):
		
		
    SUPER().
    InitializeComponent().
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID button1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	  AddProject("Testprosjekt",today,"test").
    AddTask(TODAY,5,"Testaktivitet","test").
    AddSubTask(TODAY,2,"Test sub-aktivitet","","").
  
    ShowProject().
		
		RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
    appearance1 = NEW Infragistics.Win.Appearance().
    THIS-OBJECT:ultraGanttView1 = NEW Infragistics.Win.UltraWinGanttView.UltraGanttView().
    THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
    CAST(THIS-OBJECT:ultraGanttView1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* ultraGanttView1 */
    /*  */
    THIS-OBJECT:ultraGanttView1:GridAreaWidth = 250.
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
    THIS-OBJECT:ultraGanttView1:Location = NEW System.Drawing.Point(12, 19).
    THIS-OBJECT:ultraGanttView1:Name = "ultraGanttView1".
    THIS-OBJECT:ultraGanttView1:Size = NEW System.Drawing.Size(716, 392).
    THIS-OBJECT:ultraGanttView1:TabIndex = 0.
    THIS-OBJECT:ultraGanttView1:TaskSettings:AllowDelete = Infragistics.Win.DefaultableBoolean:True.
    THIS-OBJECT:ultraGanttView1:TaskSettings:AllowEditDuration = Infragistics.Win.DefaultableBoolean:True.
    THIS-OBJECT:ultraGanttView1:Text = "ultraGanttView1".
    appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
    THIS-OBJECT:ultraGanttView1:TimelineSettings:BarSettings:BarAppearance = appearance1.
    THIS-OBJECT:ultraGanttView1:VerticalSplitterMinimumResizeWidth = 10.
    THIS-OBJECT:ultraGanttView1:TaskAdded:Subscribe(THIS-OBJECT:oppgave).
    /*  */
    /* button1 */
    /*  */
    THIS-OBJECT:button1:Location = NEW System.Drawing.Point(734, 28).
    THIS-OBJECT:button1:Name = "button1".
    THIS-OBJECT:button1:Size = NEW System.Drawing.Size(75, 23).
    THIS-OBJECT:button1:TabIndex = 1.
    THIS-OBJECT:button1:Text = "button1".
    THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
    THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
    THIS-OBJECT:button1:Click:Subscribe(THIS-OBJECT:button1_Click).
    /*  */
    /* IGGanttForm */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(835, 430).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:button1).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGanttView1).
    THIS-OBJECT:Name = "IGGanttForm".
    THIS-OBJECT:Text = "IGGanttForm".
    CAST(THIS-OBJECT:ultraGanttView1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.

  METHOD PUBLIC LOGICAL addProject(icTitle AS CHAR,
                                   idBase  AS DATE,
                                   icKey   AS CHAR).
                                   

    oProject = THIS-OBJECT:ultraGanttView1:CalendarInfo:Projects:Add(icTitle, idBase). 

    oProject:Key = icKey. 

/*                                   
Dim quarterlyProject As Project = Me.ultraCalendarInfo1.Projects.Add("QuartlerlyProject", DateTime.Today) 
quarterlyProject.Key = "projkey1" 
*/
  END METHOD.
  
  METHOD PUBLIC VOID addTask(INPUT idStart     AS DATETIME,
                                 INPUT iiDaySpan   AS INT,
                                 INPUT icText      AS CHAR,
                                 INPUT icParent    AS CHAR):

    DEF VAR oTimeSpan AS System.TimeSpan NO-UNDO.

    oTimeSpan = NEW System.TimeSpan(iiDaySpan).
                                   
    oTask = THIS-OBJECT:ultraGanttView1:CalendarInfo:Tasks:Add(idStart,oTimeSpan,icText,icParent). 
  
  /*
  ' Create a Summary or Parent Task 
Dim requirementsTask As Task = Me.ultraCalendarInfo1.Tasks.Add(DateTime.Today, TimeSpan.FromDays(5), "Requirements", "projkey1") 
  */
    
  END METHOD.

  METHOD PUBLIC VOID addSubTask(INPUT idStart     AS DATETIME,
                                INPUT iiDaySpan   AS INT,
                                INPUT icText      AS CHAR,
                                INPUT icRole      AS CHAR,
                                INPUT icResource  AS CHAR):

    DEF VAR oTimeSpan AS System.TimeSpan NO-UNDO.
    DEF VAR oSubTask  AS Infragistics.Win.UltraWinSchedule.Task NO-UNDO.

    oTimeSpan = NEW System.TimeSpan(iiDaySpan).
                                   
    oSubTask = oTask:Tasks:Add(idStart,oTimeSpan,icText). 

/*    oOwner = Infragistics.Win.UltraWinSchedule.Owners.Add(icRole AS System.EnumHelper(),icResource). */ 

  
  /*
  ' Create a Summary or Parent Task 
Dim requirementsTask As Task = Me.ultraCalendarInfo1.Tasks.Add(DateTime.Today, TimeSpan.FromDays(5), "Requirements", "projkey1") 
  */
    
  END METHOD.
  
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID oppgave( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGanttView.TaskAddedEventArgs ):
		
		MESSAGE this-object:ultraGanttView1:GridSettings:ColumnSettings
    VIEW-AS ALERT-BOX.
		RETURN.

	END METHOD.

  /*
' Create a child task 
Dim budgetTask As Task = requirementsTask.Tasks.Add(DateTime.Today, TimeSpan.FromDays(2), "Budget Analysis") 
' Set Deadline 
budgetTask.Deadline = DateTime.Today.AddDays(3) 
'Assign a Resource for this task 
Dim budgetOwner As Owner = Me.ultraCalendarInfo1.Owners.Add("BudgetOwner", "Bill Isacky") 
budgetTask.Resources.Add(budgetOwner) 

' Create another child task 
Dim teamTask As Task = requirementsTask.Tasks.Add(DateTime.Today.AddDays(3), TimeSpan.FromDays(2), "Team Allocation") 
' Set a Constraint for this Task 
teamTask.ConstraintDateTime = DateTime.Today.AddDays(4) 
teamTask.Constraint = TaskConstraint.FinishNoLaterThan 
  
  */

  METHOD PUBLIC VOID ShowProject():
    
/*    THIS-OBJECT:ultraGanttView1:CalendarInfo = this-object:u*/
      
    THIS-OBJECT:ultraGanttView1:Project = oProject. /* THIS-OBJECT:ultraGanttView1:CalendarInfo:Projects(1). */ 
    
/*
Me.ultraGanttView1.CalendarInfo = Me.ultraCalendarInfo1 
' Assign the new Project to GanttView so that this Project is shown in GanttView and not the unassigned Project. 
Me.ultraGanttView1.Project = Me.ultraGanttView1.CalendarInfo.Projects(1) 

*/    
  END METHOD.  


	DESTRUCTOR PUBLIC IGGanttForm ( ):

	END DESTRUCTOR.

END CLASS.