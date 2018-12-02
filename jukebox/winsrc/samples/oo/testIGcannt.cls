 
 /*------------------------------------------------------------------------
    File        : testIGcannt
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Mon Apr 14 12:33:52 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS winsrc.samples.oo.testIGcannt INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraGanttView1 AS Infragistics.Win.UltraWinGanttView.UltraGanttView NO-UNDO.

		
	CONSTRUCTOR PUBLIC testIGcannt (  ):
		
		
    SUPER().
    InitializeComponent().
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
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
    THIS-OBJECT:ultraGanttView1:Click:Subscribe(THIS-OBJECT:ultraGanttView1_Click).
    /*  */
    /* testIGcannt */
    /*  */
    THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGanttView1).
    THIS-OBJECT:Name = "testIGcannt".
    THIS-OBJECT:Size = NEW System.Drawing.Size(605, 312).
    CAST(THIS-OBJECT:ultraGanttView1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraGanttView1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.

	DESTRUCTOR PUBLIC testIGcannt ( ):

	END DESTRUCTOR.

END CLASS.