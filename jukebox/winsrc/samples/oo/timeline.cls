 
 /*------------------------------------------------------------------------
    File        : timeline
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Tue Apr 29 10:40:55 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS winsrc.samples.oo.timeline INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraTimelineView1 AS Infragistics.Win.UltraWinSchedule.UltraTimelineView NO-UNDO.

		
	CONSTRUCTOR PUBLIC timeline (  ):
		
		
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
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE dateInterval1 AS Infragistics.Win.UltraWinSchedule.DateInterval NO-UNDO.
    dateInterval1 = NEW Infragistics.Win.UltraWinSchedule.DateInterval().
    THIS-OBJECT:ultraTimelineView1 = NEW Infragistics.Win.UltraWinSchedule.UltraTimelineView().
    CAST(THIS-OBJECT:ultraTimelineView1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* ultraTimelineView1 */
    /*  */
    THIS-OBJECT:ultraTimelineView1:ColumnWidth = 0.
    THIS-OBJECT:ultraTimelineView1:Location = NEW System.Drawing.Point(13, 34).
    THIS-OBJECT:ultraTimelineView1:Name = "ultraTimelineView1".
    THIS-OBJECT:ultraTimelineView1:OwnerHeaderTextOrientation = NEW Infragistics.Win.TextOrientationInfo(2, Infragistics.Win.TextFlowDirection:Horizontal).
    THIS-OBJECT:ultraTimelineView1:PrimaryInterval = dateInterval1.
    THIS-OBJECT:ultraTimelineView1:Size = NEW System.Drawing.Size(763, 442).
    THIS-OBJECT:ultraTimelineView1:TabIndex = 0.
    THIS-OBJECT:ultraTimelineView1:SelectedDateTimeRangeChanging:Subscribe(THIS-OBJECT:ultraTimelineView1_SelectedDateTimeRangeChanging).
    /*  */
    /* timeline */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(788, 524).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTimelineView1).
    THIS-OBJECT:Name = "timeline".
    THIS-OBJECT:Text = "timeline".
    CAST(THIS-OBJECT:ultraTimelineView1, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID ultraTimelineView1_SelectedDateTimeRangeChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinSchedule.SelectedDateTimeRangeChangingEventArgs ):
		
		RETURN.

	END METHOD.

	DESTRUCTOR PUBLIC timeline ( ):

	END DESTRUCTOR.

END CLASS.